open Base
open Yojson
open Sqlite3

(** Album data from database *)
type album_data = {
  album_name : string;
  artist_names : string list;
  release_year : int;
}

(** song data to be collected from musicbrainz *)
type song_data = {
  song_name : string;
  artist_names : string list;
  track_number : int;
  duration_sec : int;
}

