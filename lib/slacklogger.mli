(** Slacklogger

    Log and query Slack conversations using the Real Time Messaging (RTM) API.
    The logs are stored in a version controlled SQLite database. *)


(** {1 Real Time Messaging Stream} *)

type rtm_stream
(** The type of RTM stream. *)

val rtm_uri_of_token: token:string -> Uri.t Lwt.t
(** Fetch the RTM websocket URI. *)

val rtm_stream_of_rtm_uri: uri:Uri.t -> rtm_stream Lwt.t
(** Open an RTM stream for the given RTM websocket URI *)

type info =
  { id : string;
    name : string; }

val get_channels_info : token:string -> info list Lwt.t
(** Fetch information for all the channels visible to the token. *)

val get_users_info : token:string -> info list Lwt.t
(** Fetch information for all the users in the team. *)

(** {1 Database access} *)

val create_tables : database:string -> unit
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

val populate_db : database:string -> token:string -> unit Lwt.t
(** Populate the [channels] and [messages] tables with already existing
    information. Raise [Failure "Tables absent"] if the tables do not exist. *)

(** {1 Logging} *)

val log_to_cmdline : rtm_stream -> unit Lwt.t
(** Log to standard output. *)

val log_to_db : [`Git of string | `Database of string] -> rtm_stream -> unit Lwt.t
(** Log to database. The database can either be a stand-alone SQLite database
    file or a git repository. In the former case, the first argument is
    [`Database path] where [path] is the path to the SQLite database on the
    local file system.

    In the latter case, the first argument is [`Git repo], where the [repo] is
    the location of the git repository in the local file system. The function
    attempts to read the SQLite database file named [db] at the root of the
    git repository. with each incoming message, the database is updated and the
    new state is committed into the master branch of the repository.

    In either case, if the database does not exist, then it is created. It is
    expected that the database has tables with the schema described in
    {!create_tables} and is populated with information about channels and
    messages using {!populate_db}. *)

val query_db : database:string -> Ezjsonm.t
(** Returns a human readable version of the message log. *)
