open Base
open Stdio
open Sqlite3

type album_data = {
  album_name : string;
  artist_names : string list;
  year : int;
}

let read_html_file filepath =
  try
    Ok (In_channel.read_all filepath)
  with e ->
    Error (Printf.sprintf "Failed to read %s: %s" filepath (Exn.to_string e))

let parse_html html = 
  let soup = Soup.parse html in
  Soup.($$) soup ".page_section_charts_item_wrapper"
  |> Soup.to_list
  |> List.map ~f:(fun node -> 
    let album_name =
      Soup.(node $ ".page_charts_section_charts_item_title .ui_name_locale_original")
      |> Soup.R.leaf_text
      |> String.strip 
    in

    let artist_names =
      match Soup.(node $? ".page_charts_section_charts_item_credited_links_primary .ui_name_locale_original") with
      | Some span ->
          Soup.R.leaf_text span
          |> String.substr_replace_all ~pattern:"," ~with_:"&"
          |> String.split ~on:'&'
          |> List.map ~f:String.strip
      | None -> 
          ["Unknown Artist"]
    in

    let year = 
      Soup.(node $ ".page_charts_section_charts_item_date span")
      |> Soup.R.leaf_text
      |> String.strip 
      |> String.split ~on:' ' 
      |> List.last_exn         
      |> Int.of_string
    in
    { album_name; artist_names; year;}  
  )

let check_rc (rc : Rc.t) : unit =
  match rc with
  | Rc.OK | Rc.DONE -> ()
  | _ -> failwith (Rc.to_string rc)

let get_id db sql param =
  let stmt = prepare db sql in
  check_rc (bind stmt 1 (Data.TEXT param));
  let id =
    match step stmt with
    | Rc.ROW -> (
        match column stmt 0 with
        | Data.INT i -> Int64.to_int_exn i
        | _ -> failwith "Expected INT ID"
      )
    | _ -> failwith "Failed to find ID"
  in
  check_rc (finalize stmt);
  id

let insert_artist db name =
  let stmt = prepare db "INSERT OR IGNORE INTO artist (name) VALUES (?)" in
  check_rc (bind stmt 1 (Data.TEXT name));
  check_rc (step stmt);
  check_rc (finalize stmt);
  get_id db "SELECT id FROM artist WHERE name = ?" name

let insert_album db data =
  let stmt = prepare db "INSERT INTO album (title, release_year) VALUES (?, ?)" in
  check_rc (bind stmt 1 (Data.TEXT data.album_name));
  check_rc (bind stmt 2 (Data.INT (Int64.of_int data.year)));
  check_rc (step stmt);
  check_rc (finalize stmt);
  Int64.to_int_exn (last_insert_rowid db) 

let insert_album_artist db album_id artist_id =
  let stmt = prepare db "INSERT OR IGNORE INTO album_artist (album_id, artist_id) VALUES (?, ?)" in
  check_rc (bind stmt 1 (Data.INT (Int64.of_int album_id)));
  check_rc (bind stmt 2 (Data.INT (Int64.of_int artist_id)));
  check_rc (step stmt);
  check_rc (finalize stmt)

let insert_album_data db data =
  check_rc (exec db "BEGIN TRANSACTION;");
  try 
    let album_id = insert_album db data in
    
    List.iter data.artist_names ~f:(fun artist_name ->
      let artist_id = insert_artist db artist_name in
      insert_album_artist db album_id artist_id
    );
    
    check_rc (exec db "COMMIT;");
    album_id
  with e ->
    ignore (exec db "ROLLBACK;");
    raise e