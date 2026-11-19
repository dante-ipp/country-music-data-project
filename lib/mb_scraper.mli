(** Album data from database *)
type known_album_data = {
  album_id : int;
  album_name : string;
  artists : string list;
  year : int;
}

(** song data to be collected from musicbrainz *)
type song_data = {
  title : string;
  duration_ms : int;
  track_number : int;
}

(** album data to be collected from musicbrainz *)
type album_data = {
  album_id : int;
  labels : string list;
  songs : song_data list;
}

(** gets the album data from known album data*)
val get_album_data : known_album_data -> (album_data, string) result

(** gets the known album data from the sqlite3 db *)
val get_known_album_data : Sqlite3.db -> status:int-> known_album_data list

(** writes the new album data to the database *)
val write_album_data : Sqlite3.db -> album_data -> unit

(** sets the scrape status to *)
val set_album_scrape_status : Sqlite3.db -> album_id:int -> status:int -> unit 