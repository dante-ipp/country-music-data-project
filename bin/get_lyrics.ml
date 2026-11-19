open Core
open Stdio 
open Music_scaper_lib.Lrc_scraper

let db = Sqlite3.db_open "data/music_data.db"

let () = 
  let lst = song_data_from_db db ~status:0 in

  List.iter ~f:(fun song -> 
    match fetch_lyrics song with
    | Ok result -> 
      (try
        write_lyrics db result;
        set_lyric_scrape_status db ~song_id:song.id ~status:1;
        printf " [+] Inserted: %s\n%!" song.title
      with e -> 
        printf " [!] Exception inserting %s: %s\n%!"
        song.title (Exn.to_string e))
    | Error err_msg ->
      set_lyric_scrape_status db ~song_id:song.id ~status:2;
      printf " [!] Error scraping %s: %s\n" song.title err_msg
    ) lst;

printf "Done!\n";
ignore (Sqlite3.db_close db);