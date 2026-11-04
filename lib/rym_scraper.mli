open Base
open Sqlite3

(** data type to be collected from website *)
type album_data = {
  album_name : string;
  artist_names : string list;
  year : int;
}

val read_html_file : string -> (string, string) Result.t

(** parses html *)
val parse_html : string -> album_data list

(** [insert_album_data] inserts album_data into the database [db] *)
val insert_album_data : db -> album_data -> int