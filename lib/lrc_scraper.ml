open Core

type song_data = {
  id : int;
  title : string;
  artists : string list;
  album_name : string;
  duration_ms : int;
}

type lyrics = {
  song_id : int;
  lyrics : string option;
}

let lrc_uri ~artist (s : song_data) = 
  let query = 
    [
      ("artist_name", [artist]);
      ("album_name", [s.album_name]);
      ("duration", [Int.to_string (s.duration_ms / 1000)]);
      ("track_name", [s.title])
    ] 
  in Uri.make
  ~scheme:"https"
  ~host:"lrclib.net"
  ~path:"api/get"
  ~query
  () |> Uri.to_string

let user_agent_header = 
  ("User-Agent", "Music-data-scraper (github.com/dante-ipp/country-music-data-project)")

let rec get_with_retry ?(attempts=3) url =
  let results = Ezcurl.get ~headers:[user_agent_header] ~url () in
  match results with
  | Ok response -> 
    if response.code = 200 then Ok response.body
    else if response.code = 503 && attempts > 0 then begin
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

let parse_lyrics_json ~song_id json_str = 
  let json = Yojson.Basic.from_string json_str in
  let open Yojson.Basic.Util in
  let lyrics = match json |> member "instrumental" |> to_bool with 
  | false -> 
    Some (json |> member "plainLyrics" |> to_string)
  | true -> None in
  {song_id; lyrics}

let safe_parse_lyrics_json ~song_id json_str = 
  try Ok (parse_lyrics_json ~song_id json_str)
  with
  | Yojson.Json_error msg -> Error (sprintf "Malformed JSON: %s" msg)
  | Yojson.Basic.Util.Type_error (msg, _) -> Error (sprintf "Missing/Invalid lyrics field: %s" msg)
  | _ -> Error "Unknown error parsing track data" 

let rec fetch_for_artist ~artists (s : song_data) = 
  match artists with 
  | [] -> Error "No artist found"
  | curr_artist :: others -> 
    let url = lrc_uri ~artist:curr_artist s in

    match get_with_retry url with
    | Ok result -> 
      let () = Core_unix.sleep 2 in
      safe_parse_lyrics_json ~song_id:s.id result
    | Error _err_msg ->
          let () = Core_unix.sleep 2 in
          fetch_for_artist ~artists:others s

let fetch_lyrics s =
  fetch_for_artist ~artists:s.artists s

let check_rc rc =
  let open Sqlite3 in match rc with
  | Rc.OK | Rc.DONE -> ()
  | _ -> failwith (Rc.to_string rc)

let song_data_from_db db ~status=
  let open Sqlite3 in
  let sql = "
  SELECT s.id, s.title, GROUP_CONCAT(ar.name, '|'), a.title, s.duration_ms
  FROM song s
  LEFT JOIN album a ON a.id = s.album_id
  LEFT JOIN song_artist sa ON s.id = sa.song_id
  LEFT JOIN artist ar ON sa.artist_id = ar.id
  WHERE s.scrape_status = ?
  GROUP BY s.id" in
  let stmt = prepare db sql in
  check_rc (bind stmt 1 (Data.INT (Int64.of_int status)));
  let rec helper acc = 
    match step stmt with
    | Rc.ROW -> 
      let id = match column stmt 0 with 
      | Data.INT i -> Int64.to_int_exn i | _ -> 0 in

      let title = match column stmt 1 with 
      | Data.TEXT str -> str | _ -> "" in 

      let artists = match column stmt 2 with 
      | Data.TEXT str -> str |> String.split ~on:'|' 
      | _ -> [] in

      let album_name = match column stmt 3 with 
      | Data.TEXT str -> str | _ -> "" in

      let duration_ms = match column stmt 4 with 
      | Data.INT i -> Int64.to_int_exn i | _ -> 0 in
      helper ({id; title; artists; album_name; duration_ms} :: acc) 
    | Rc.DONE -> acc
    | _ -> failwith "database error during read"
  in (try let results = helper [] in
    ignore (finalize stmt); 
    List.rev results
  with e -> 
    ignore (finalize stmt);
    raise e)

let write_lyrics db l =
  let open Sqlite3 in
  check_rc (exec db "BEGIN TRANSACTION");

  let sql = "UPDATE song SET lyrics = ? WHERE id = ?" in
  let stmt = prepare db sql in

  try 
    let song_id = Int64.of_int l.song_id in
    let lyrics = match l.lyrics with 
    | Some l -> Data.TEXT l
    | None -> Data.NULL
    in

    check_rc (bind stmt 1 lyrics);
    check_rc (bind stmt 2 (Data.INT song_id));

    check_rc (step stmt);
    check_rc (finalize stmt);
    check_rc (exec db "COMMIT TRANSACTION");

with e -> 
  ignore (exec db "ROLLBACK TRANSACTION");
  ignore (finalize stmt);
  raise e

let set_lyric_scrape_status db ~song_id ~status =
  let open Sqlite3 in
  let sql = "UPDATE song SET scrape_status = ? WHERE id = ?" in
  let stmt = prepare db sql in

  try 
    check_rc (bind stmt 1 (Data.INT (Int64.of_int status)));
    check_rc (bind stmt 2 (Data.INT (Int64.of_int song_id)));

    check_rc (step stmt);
    check_rc (finalize stmt);
  with e -> 
    ignore (finalize stmt);
    raise e

