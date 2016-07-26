(** Slacklogger *)


(** {1 Real Time Messaging Stream} *)

type rtm_stream
(** The type of RTM stream. *)

val get_rtm_uri: token:string -> Uri.t Lwt.t
(** Fetch the RTM websocket URL. *)

val mk_rtm_stream : uri:Uri.t -> rtm_stream Lwt.t
(** Open an RTM stream for the given RTM websocket URL *)

type info =
  { id : string;
    name : string; }

val get_channels_info : token:string -> info list Lwt.t
(** Fetch information for all the channels visible to the token. *)

val get_users_info : token:string -> info list Lwt.t
(** Fetch information for all the users in the team. *)

(** {1 Database access} *)

val create_tables : Sqlite3.db -> unit
(** Create tables for storing the logs. The tables created are:

{[
  TABLE channels(
    id text primary key,
    name text);

  TABLE users(
    id text primary key,
    name text);

  TABLE messages(
    id integer,
    ts integer,
    msg text,
    channel_id text,
    user_id text,
    primary key (id,ts),
    foreign key(channel_id) references channels(id),
    foreign key(user_id) references users(id));
]}

    Raise [Failure "Tables exist"] if the tables already exist. *)

val populate_db : Sqlite3.db -> token:string -> unit Lwt.t
(** Populate the [channels] and [messages] tables.
    Raise [Failure "Tables absent"] if the tables do not exist. *)

(** {1 Logging} *)

val log_to_cmdline : rtm_stream -> unit Lwt.t
(** Log to standard output. The function does not return. Terminating the
    process closes the RTM stream. *)

val log_to_db : Sqlite3.db -> rtm_stream -> unit Lwt.t
(** Log to database. The function does not return. Terminating the process
    closes the RTM stream. It is expected that the database has tables with the
    schema described in {!create_tables} and is populated with information about
    channels and messages using {!populate_db}. *)

val query_db : Sqlite3.db -> Ezjsonm.t
(** Returns a human readable version of the message log. *)
