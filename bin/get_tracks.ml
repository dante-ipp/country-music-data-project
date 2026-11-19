open Core
open Stdio
open Music_scaper_lib.Mb_scraper

let db = Sqlite3.db_open "data/music_data.db"

let () = 
  let lst = get_known_album_data db ~status:2 in
  
  List.iter ~f:(fun album -> 
      match get_album_data album with
      | Ok data -> 
        (try 
          write_album_data db data;
          set_album_scrape_status db ~album_id:album.album_id ~status:1;
          printf "  [+] Inserted: %s\n%!" album.album_name
        with e -> 
          printf "  [!] Exception inserting %s: %s\n%!" 
          album.album_name (Exn.to_string e))
               
      | Error err_msg ->
          set_album_scrape_status db ~album_id:album.album_id ~status:2;
          printf "  [!] Error scraping %s: %s\n%!" album.album_name err_msg;
    ) lst;

printf "Done\n";
ignore (Sqlite3.db_close db);