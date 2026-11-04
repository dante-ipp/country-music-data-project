open Base
open Yojson
open Sqlite3

type album_data = {
  album_name : string;
  artist_names : string list;
  release_year : int;
}

type song_data = {
  song_name : string;
  artist_names : string list;
  track_numer : int;
  duration_sec : int;
}

