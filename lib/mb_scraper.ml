open Core

type known_album_data = {
  album_id : int;
  album_name : string;
  artists : string list;
  year : int;
}

type song_data = {
  title : string;
  duration_ms : int;
  track_number : int;
}

type album_data = {
  album_id : int;
  labels : string list;
  songs : song_data list;
}

(* formats the query string to be used *)
let make_query (a : known_album_data) = 
  let sanitize_lucene str =
    String.filter str ~f:(fun c -> 
    not (String.contains "():\"+-!~*?{}[]^\\" c)
  ) in
  
  let clean_title = sanitize_lucene a.album_name in
  
  let artist_query = 
    a.artists
    |> List.map ~f:sanitize_lucene
    |> List.filter ~f:(fun name -> not (String.is_empty name))
    |> String.concat ~sep:" OR "
  in

  if String.is_empty artist_query then
    sprintf {|release:(%s)|} clean_title
  else
    sprintf {|release:(%s) AND artist:(%s)|} clean_title artist_query

(* creates a query uri string from album data *)
let mb_uri_string (a : known_album_data) = 
  let mb_query (a : known_album_data) = 
  [
    ("query", [make_query a]);
    ("fmt", ["json"]);
    ("limit", ["1"]);
  ] in Uri.make
  ~scheme:"https"
  ~host:"musicbrainz.org"
  ~path:"/ws/2/release"
  ~query:(mb_query a)
  () |> Uri.to_string

(* gets the music brainz album id from the json string *)
let id_from_json json_str = 
  let open Yojson.Basic.Util in 
  let root = Yojson.Basic.from_string json_str in
  
  (* Convert to a list so we can check if it's empty safely *)
  let releases = root |> member "releases" |> to_list in
  match releases with
  | [] -> failwith "No results found on MusicBrainz for this query"
  | first_release :: _ -> 
      first_release |> member "id" |> to_string

let safe_id_from_json json_str = 
  try Ok (id_from_json json_str)
  with 
  | Yojson.Json_error msg -> Error (sprintf "Malformed JSON: %s" msg)
  | Yojson.Basic.Util.Type_error (msg, _) -> Error (sprintf "Missing/Invalid ID field: %s" msg)
  | Failure msg -> Error msg
  | e -> 
      let snippet = if String.length json_str > 200 then String.sub json_str ~pos:0 ~len:200 ^ "..." else json_str in
      Error (sprintf "Unknown error: %s | JSON snippet: %s" (Exn.to_string e) snippet)

let user_agent_header = 
  ("User-Agent", "Music-data-scraper (github.com/dante-ipp/country-music-data-project)")

let rec get_with_retry ?(attempts=3) url =
  let result = Ezcurl.get ~headers:[user_agent_header] ~url () in
  match result with 
  | Ok response -> 
      if response.code = 200 then Ok response.body 
      else if response.code = 503 && attempts > 1 then begin
        printf "  [~] MusicBrainz busy (503). Retrying in 5 seconds... (%d attempts left)\n%!" (attempts - 1);
        Core_unix.sleep 5;
        get_with_retry ~attempts:(attempts - 1) url
      end
      else Error (sprintf "HTTP Error: %d - %s" response.code response.body)
  | Error (_, error_msg) -> 
      if attempts > 1 then begin
        printf "  [~] Network error. Retrying in 5 seconds... (%d attempts left)\n%!" (attempts - 1);
        Core_unix.sleep 5;
        get_with_retry ~attempts:(attempts - 1) url
      end
      else Error (sprintf "Curl failed: %s" error_msg)

let get_album_json (a : known_album_data) = 
  get_with_retry (mb_uri_string a)

let mb_uri_from_id mb_id_str = 
  let lookup_query = [
    ("inc",["labels+recordings"]);
    ("fmt", ["json"]); 
  ] in
  Uri.make
  ~scheme:"https"
  ~host:"musicbrainz.org"
  ~path:("/ws/2/release/" ^ mb_id_str)
  ~query:lookup_query
  () |> Uri.to_string

let get_track_data mb_id = 
  get_with_retry (mb_uri_from_id mb_id)

let parse_track track_json =  
  let open Yojson.Basic.Util in
  {
    title = track_json |> member "title" |> to_string;
    duration_ms = (match track_json |> member "length" |> to_int_option with 
    | Some v -> v | None -> 0);
    track_number = track_json |> member "position" |> to_int;
  }

let parse_album album_id json_str =
  let json = Yojson.Basic.from_string json_str in
  let open Yojson.Basic.Util in

  let labels = 
    json 
    |> member "label-info" 
    |> to_list
    |> List.map ~f:(fun l -> 
      l |> member "label" |> member "name" |> to_string |> String.strip
    )
  in

  let songs = json 
    |> member "media" 
    |> to_list
    |> List.concat_map ~f:(fun media -> 
         media |> member "tracks" |> to_list |> List.map ~f:parse_track
       )
  in 
  {album_id; labels; songs;}

let safe_parse_album album_id json_str = 
  try Ok (parse_album album_id json_str)
  with
  | Yojson.Json_error msg -> Error (sprintf "Malformed JSON: %s" msg)
  | Yojson.Basic.Util.Type_error (msg, _) -> Error (sprintf "Missing/Invalid album field: %s" msg)
  | _ -> Error "Unknown error parsing track data" 

let ( let* ) r f = Result.bind r ~f

let get_album_data (a : known_album_data) : (album_data, string) result = 
  let* search_json = get_album_json a in
  (* Gotta respect musicbrainz.org *)
  let () = Core_unix.sleep 2 in
  let* mb_id = safe_id_from_json search_json in
  let* track_json = get_track_data mb_id in
  let () = Core_unix.sleep 2 in
  safe_parse_album a.album_id track_json

let check_rc rc =
  let open Sqlite3 in match rc with
  | Rc.OK | Rc.DONE -> ()
  | _ -> failwith (Rc.to_string rc)

let get_known_album_data db ~status = 
  let open Sqlite3 in
  let sql = "
    SELECT a.id, a.title, a.release_year, GROUP_CONCAT(ar.name, '|')
    FROM album a
    LEFT JOIN album_artist aa ON a.id = aa.album_id
    LEFT JOIN artist ar ON aa.artist_id = ar.id
    WHERE a.scrape_status = ?
    GROUP BY a.id" in 
  let stmt = prepare db sql in
  
  try 
    check_rc (bind stmt 1 (Data.INT (Int64.of_int status)));
    
    let rec helper acc = 
      match step stmt with 
      | Rc.ROW -> 
        let album_id = match column stmt 0 with Data.INT i -> Int64.to_int_exn i | _ -> 0 in
        let album_name = match column stmt 1 with Data.TEXT str -> str | _ -> "" in
        let year = match column stmt 2 with Data.INT i -> Int64.to_int_exn i | _ -> 0 in
        let artists = match column stmt 3 with Data.TEXT str -> String.split str ~on:'|' | _ -> [] in
        let album = {album_id; album_name; artists; year} in
        helper (album :: acc)
      | Rc.DONE -> acc
      | rc -> failwith (Rc.to_string rc)
    in 
    let results = helper [] in
    check_rc (finalize stmt);
    List.rev results
  with e -> 
    ignore (finalize stmt); 
    raise e

let write_album_data db data = 
  let open Sqlite3 in
  check_rc (exec db "BEGIN TRANSACTION");

  let song_sql = "INSERT INTO song (title, track_number, duration_ms, album_id) VALUES (?, ?, ?, ?)" in
  let song_artist_sql = "
    INSERT OR IGNORE INTO song_artist (song_id, artist_id) 
    SELECT ?, artist_id FROM album_artist WHERE album_id = ?" in

  let label_insert_sql = "INSERT OR IGNORE INTO label (name) VALUES (?)" in
  let label_select_sql = "SELECT id FROM label WHERE name = ?" in
  let album_label_sql = "INSERT OR IGNORE INTO album_label (album_id, label_id) VALUES (?, ?)" in

  let song_stmt = prepare db song_sql in
  let sa_stmt = prepare db song_artist_sql in
  let lbl_ins_stmt = prepare db label_insert_sql in
  let lbl_sel_stmt = prepare db label_select_sql in
  let alb_lbl_stmt = prepare db album_label_sql in

  try 
    List.iter ~f:(fun song -> 
      let title = song.title in
      let duration_ms = Int64.of_int song.duration_ms in
      let track_num = Int64.of_int song.track_number in
      let a_id = Int64.of_int data.album_id in 

      check_rc (bind song_stmt 1 (Data.TEXT title));
      check_rc (bind song_stmt 2 (Data.INT track_num));
      check_rc (bind song_stmt 3 (Data.INT duration_ms));
      check_rc (bind song_stmt 4 (Data.INT a_id));
      check_rc (step song_stmt);
      check_rc (reset song_stmt);

      let new_song_id = last_insert_rowid db in

      check_rc (bind sa_stmt 1 (Data.INT new_song_id));
      check_rc (bind sa_stmt 2 (Data.INT a_id));
      check_rc (step sa_stmt);
      check_rc (reset sa_stmt);

    ) data.songs;

    List.iter ~f:(fun label_name -> 
      check_rc (bind lbl_ins_stmt 1 (Data.TEXT label_name));
      
      check_rc (step lbl_ins_stmt);
      check_rc (reset lbl_ins_stmt);
      
      check_rc (bind lbl_sel_stmt 1 (Data.TEXT label_name));
      
      let label_id = match step lbl_sel_stmt with
        | Rc.ROW -> 
            let id = match column lbl_sel_stmt 0 with
              | Data.INT i -> i
              | _ -> failwith "Label ID not an integer" 
            in
            check_rc (reset lbl_sel_stmt);
            id
        | rc -> failwith (Rc.to_string rc)
      in

      let a_id = Int64.of_int data.album_id in 
      check_rc (bind alb_lbl_stmt 1 (Data.INT a_id));
      check_rc (bind alb_lbl_stmt 2 (Data.INT label_id));
      
      check_rc (step alb_lbl_stmt);
      check_rc (reset alb_lbl_stmt);
    ) data.labels;

    check_rc (finalize song_stmt);
    check_rc (finalize sa_stmt);
    check_rc (finalize lbl_ins_stmt);
    check_rc (finalize lbl_sel_stmt);
    check_rc (finalize alb_lbl_stmt);

    check_rc (exec db "COMMIT TRANSACTION")
  with e -> 
    ignore (exec db "ROLLBACK TRANSACTION");
    ignore (finalize song_stmt);
    ignore (finalize sa_stmt);
    ignore (finalize lbl_ins_stmt);
    ignore (finalize lbl_sel_stmt);
    ignore (finalize alb_lbl_stmt);
    raise e

let set_album_scrape_status db ~album_id ~status =
  let open Sqlite3 in
  let sql = "UPDATE album SET scrape_status = ? WHERE id = ?" in
  let stmt = prepare db sql in
  
  try
    check_rc (bind stmt 1 (Data.INT (Int64.of_int status)));
    check_rc (bind stmt 2 (Data.INT (Int64.of_int album_id)));
    
    check_rc (step stmt);
    check_rc (finalize stmt)
  with e -> 
    ignore (finalize stmt);
    raise e