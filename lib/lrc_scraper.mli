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


(** fetches the lyrics for song *)
val fetch_lyrics : song_data -> (lyrics, string) result

(** gets songs data from db *)
val song_data_from_db : Sqlite3.db -> status : int -> song_data list

(** writes lyrics to db *)
val write_lyrics : Sqlite3.db -> lyrics -> unit

(** updates the scrape status *)
val set_lyric_scrape_status : Sqlite3.db -> song_id : int -> status : int -> unit