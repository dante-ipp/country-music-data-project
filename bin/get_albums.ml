open Base
open Stdio
open Music_scaper_lib.Rym_scraper

let targets = [
  "data/html_pages/2020s.html";
  "data/html_pages/2010s.html";
  "data/html_pages/2000s.html";
  "data/html_pages/1990s.html";
  "data/html_pages/1980s.html";
  "data/html_pages/1970s.html";
  "data/html_pages/1960s.html";
]

let albums filepath = 
  match read_html_file filepath with
  | Ok html_body -> parse_html html_body
  | Error msg -> failwith msg

let () = 
  let db = Sqlite3.db_open "data/music_data.db" in

  printf "Starting RYM Country Pop scrape from local files...\n%!";

  List.iter targets ~f:(fun filepath ->
    printf "Processing: %s\n%!" filepath;
    
    if not (Stdlib.Sys.file_exists filepath) then
      printf "  [!] File not found, skipping: %s\n%!" filepath
    else
      let scraped_albums = albums filepath in
      printf "Found %d albums. Inserting into database...\n%!" (List.length scraped_albums);
      
      List.iter scraped_albums ~f:(fun album ->
        try 
          let id = insert_album_data db album in
          printf "  [+] Inserted: %s (ID: %d)\n%!" album.album_name id
        with e ->
          printf "  [!] Error inserting %s: %s\n%!" album.album_name (Exn.to_string e)
      );
  );

  printf "Scrape complete. Closing database.\n%!";
  ignore (Sqlite3.db_close db)