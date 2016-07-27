open Lwt.Infix

type rtm_stream =
  { msg_stream       : string Lwt_stream.t;
    close_connection : unit -> unit;
    token            : string }

type info =
  {id   : string;
   name : string}

let section = Lwt_log.Section.make "slacklogger"

let human_of_timestamp ts =
  match Ptime.(int_of_string ts |> Span.of_int_s |> add_span epoch) with
  | None -> failwith "impossible"
  | Some t -> Format.asprintf "%a" Ptime.pp t

let stream_of_token ~token =
  let url = "https://slack.com/api/rtm.start?token=" ^ token in
  let open Ezjsonm in
  let open Websocket_lwt in
  let open Frame in

  (* Fetch the websocket uri *)
  Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (resp, body) ->
  let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  Lwt_log.debug_f ~section "rtm_start: Response code: %d\n" code >>= fun () ->
  body |> Cohttp_lwt_body.to_string >>= fun body ->
  Lwt_log.debug_f ~section "rtm_start: Body of length: %d\n" (String.length body) >>= fun () ->
  let json = from_string body in
  begin
    if not (get_bool (find json ["ok"])) then
      failwith "rtm.start: unexpected response"
    else
      let uri_str = get_string (find json ["url"]) in
      Lwt_log.debug_f ~section "get_ws_uri: uri=%s\n" uri_str >|= fun () ->
      Uri.of_string uri_str
  end >>= fun uri ->

  (* Stream for sending data to consumer *)
  let msg_stream, push_data = Lwt_stream.create () in

  (* Stream for closing the connection *)
  let wait_for_close, close_connection =
    let close_stream, push_close = Lwt_stream.create () in
    (Lwt_stream.get close_stream, fun () -> push_close None)
  in

  (* See: https://github.com/vbmithr/ocaml-websocket/issues/59 *)
  let https_uri =
    match Uri.scheme uri with
    | Some "wss" -> Uri.with_scheme uri (Some "https")
    | Some "ws" -> Uri.with_scheme uri (Some "http")
    | _ -> uri
  in

  Resolver_lwt.resolve_uri ~uri:https_uri Resolver_lwt_unix.system >>= fun endp ->
  Conduit_lwt_unix.(endp_to_client ~ctx:default_ctx endp >>= fun client ->
  with_connection ~ctx:default_ctx client uri) >>= fun (recv, send) ->

  Lwt_log.debug_f ~section "client: connected" >>= fun () ->
  let react fr =
    match fr.opcode with
    | Opcode.Ping ->
        Lwt_log.debug_f ~section "client: received ping\n" >>= fun () ->
        send @@ Frame.create ~opcode:Opcode.Pong ()

    | Opcode.Pong ->
        Lwt_log.debug_f ~section "client: received pong\n" >>= fun () ->
        Lwt.return_unit

    | Opcode.Close ->
        Lwt_log.debug_f ~section "client: received close\n" >>= fun () ->
        (* Immediately echo and pass this last message to the user *)
        (if String.length fr.content >= 2 then
          send @@ Frame.create ~opcode:Opcode.Close
            ~content:(String.sub fr.content 0 2) ()
        else send @@ Frame.close 1000) >|= fun () ->
        push_data None

    | Opcode.Text
    | Opcode.Binary ->
        Lwt_log.debug_f ~section "client: received Text | Binary\n" >|= fun () ->
        push_data (Some fr.content)

    | _ ->
        Lwt_log.debug_f ~section "client: received Unknown\n" >>= fun () ->
        send @@ Frame.close 1002 >|= fun () ->
        push_data None
  in
  let rec react_forever () = recv () >>= fun fr -> react fr >>= react_forever in
  let close =
      wait_for_close >>= fun _ ->
      Lwt_log.debug ~section "client: Sending a close frame." >>= fun () ->
      send @@ Frame.close 1000
  in
  Lwt.async (fun () -> close <?> react_forever ());
  Lwt.return {msg_stream; close_connection; token}

let rec log_to_cmdline {msg_stream; close_connection; _} =
  Lwt_io.printf "Logging Slack activity. Press Ctrl+d to quit\n%!" >>= fun () ->
  let rec react_forever () =
    Lwt_stream.get msg_stream >>= fun data ->
    match data with
    | None -> Lwt.return @@ close_connection ()
    | Some data ->
    Lwt_io.printf "\n> %s%!" data >>=
    react_forever
  in
  let rec watch_stdin () =
    Lwt_io.(read_line_opt stdin) >>= function
    | None ->
      Lwt_log.debug ~section "Got EOF. Sending a close frame." >>= fun () ->
      Lwt.return @@ close_connection ()
    | Some content -> watch_stdin ()
  in
  react_forever () <?> watch_stdin ()

open Irmin_unix

module Store = Irmin_git.FS(Irmin.Contents.Cstruct)(Irmin.Ref.String)(Irmin.Hash.SHA1)
module View = Irmin.View(Store)
module IO = Git_unix.FS.IO

let create_tables ~database =
  let open Sqlite3 in
  let open Sqlite3.Rc in

  let db = db_open database in
  begin
    match exec db "CREATE TABLE channels( \
                     id text primary key, \
                     name text)"
    with
    | OK -> ()
    | _ as e -> failwith @@ "create table channels failed: " ^ to_string e
  end;

  begin
    match exec db "CREATE TABLE users( \
                     id text primary key, \
                     name text)"
    with
    | OK -> ()
    | _ as e -> failwith @@ "create table users failed: " ^ to_string e
  end;

  begin
    match exec db "CREATE TABLE messages(
                     id integer,
                     ts integer,
                     msg text,
                     channel_id text,
                     user_id text,
                     primary key (id,ts),
                     foreign key(channel_id) references channels(id),
                     foreign key(user_id) references users(id));"
    with
    | OK -> ignore @@ db_close db
    | _ as e -> failwith @@ "create table channels failed: " ^ to_string e
  end

let get_channels_info, get_users_info =
  let fetch_info fetch_function kind token =
    let open Ezjsonm in
    let token = Slacko.token_of_string token in
    fetch_function token >|= fun result ->
    match result with
    | `Success json ->
        let json_string = Yojson.Basic.to_string json in
        let json = from_string json_string in
        get_list (fun j ->
          let id = get_string (find j ["id"]) in
          let name = get_string (find j ["name"])
          in {id; name}) (find json [kind])
    | _ -> failwith "populate_channels_and_users: channels_list"
  in
  (fun ~token -> fetch_info Slacko.channels_list "channels" token),
  (fun ~token -> fetch_info Slacko.users_list "members" token)

let populate_db ~database ~token =
  let open Sqlite3 in
  let open Sqlite3.Rc in
  let db = db_open database in

  get_channels_info token >>= fun channels_info ->
    List.iter (fun {id; name} ->
    let query = Printf.sprintf "INSERT INTO channels VALUES (\"%s\",\"%s\")" id name in
    match exec db query with
    | OK | CONSTRAINT -> ()
    | _ as e -> failwith @@ "populate channels failed: " ^ to_string e) channels_info;

  get_users_info token >|= fun users_info ->
    List.iter (fun {id; name} ->
    let query = Printf.sprintf "INSERT INTO users VALUES (\"%s\",\"%s\")" id name in
    match exec db query with
    | OK | CONSTRAINT -> ()
    | _ as e -> failwith @@ "populate users failed: " ^ to_string e) users_info;

  ignore @@ db_close db

let rec log_to_db git_or_db {msg_stream; close_connection; token} =
  let channels_map = Hashtbl.create 17 in
  let users_map = Hashtbl.create 17 in

  let db_file =
    match git_or_db with
    | `Git root -> Filename.concat root "db"
    | `Database db_file -> db_file
  in

  let init_hashtbls () =
    let open Sqlite3 in
    let open Sqlite3.Rc in
    let db = db_open db_file in

    begin match exec_no_headers db "select * from channels" ~cb:(function
      | [| Some id; Some name |] -> Hashtbl.add channels_map id name
      | _ -> failwith "impossible")
    with
    | OK -> ()
    | _ as e -> failwith @@ "select from channels failed: " ^ to_string e
    end;

    match exec_no_headers db "select * from users" ~cb:(function
      | [| Some id; Some name |] -> Hashtbl.add users_map id name
      | _ -> failwith "impossible")
    with
    | OK -> ()
    | _ as e -> failwith @@ "select from users failed: " ^ to_string e
  in

  let git_commit commit_msg = function
    | None -> Lwt.return ()
    | Some t ->
        IO.read_file db_file >>= fun contents ->
        Irmin.with_hrw_view (module View) (t commit_msg) `Merge ~path:[] (fun view ->
          View.update view ["db"] contents) >>=
        Irmin.Merge.exn
  in

  Lwt_io.printf "Logging Slack activity. Press Ctrl+d to quit\n%!" >>= fun () ->

  let rec react_forever store =
    Lwt_stream.get msg_stream >>= fun data ->
    let open Ezjsonm in
    let open Sqlite3 in
    let open Sqlite3.Rc in
    let db = db_open db_file in

    begin match data with
    | None ->
        close_connection ();
        Lwt.fail Exit
    | Some data ->
      let json = from_string data in
      let typ = get_string @@ find json ["type"] in

      match typ with
      | "message" ->
          let channel_id = get_string @@ find json ["channel"] in
          let user_id = get_string @@ find json ["user"] in
          let ts_id = Str.split (Str.regexp "\\.") @@
                        get_string @@ find json ["ts"]
          in
          let ts, id =
            match ts_id with
            | ts::id::_ -> ts,id
            | _ -> failwith "unexpected ts format in message"
          in
          let text = get_string @@ find json ["text"] in
          let q =
            Printf.sprintf
              "insert into messages values (\"%s\",\"%s\",\"%s\",\"%s\",\"%s\")"
              id ts text channel_id user_id
          in
          begin match exec db q with
          | OK ->
              let user = Hashtbl.find users_map user_id in
              let hum_ts = human_of_timestamp ts in
              let channel = Hashtbl.find channels_map channel_id in
              let fmt_msg = Printf.sprintf "message | %s on %s in %s\n%s"
                              user hum_ts channel text
              in
              git_commit fmt_msg store >>= fun () ->
              Lwt_log.info ~section q
          | _ as e -> failwith @@ "insert into messages failed: " ^ to_string e
          end

      | "channel_created" ->
          let channel_id = get_string @@ find json ["channel"; "id"] in
          let channel_name = get_string @@ find json ["channel"; "name"] in
          let q =
            Printf.sprintf
              "insert into channels values (\"%s\",\"%s\")"
              channel_id channel_name
          in
          begin match exec db q with
          | OK ->
              Hashtbl.add channels_map channel_id channel_name;
              let fmt_msg = Printf.sprintf "channel created | %s" channel_name
              in
              git_commit fmt_msg store >>= fun () ->
              Lwt_log.info ~section q
          | _ -> failwith "insert into channels failed"
          end

      | "team_join" ->
          let user_id = get_string @@ find json ["user"; "id"] in
          let user_name = get_string @@ find json ["user"; "name"] in
          let q =
            Printf.sprintf
              "insert into users values (\"%s\",\"%s\")"
              user_id user_name
          in
          begin match exec db q with
          | OK ->
              Hashtbl.add users_map user_id user_name;
              let fmt_msg = Printf.sprintf "user joined | %s" user_name in
              git_commit fmt_msg store >>= fun () ->
              Lwt_log.info ~section q
          | _ -> failwith "insert into users failed"
          end

      (* Ignore all other messages for now *)
      | _ -> Lwt.return ()

      end >>= fun () ->
      ignore @@ db_close db;
      react_forever store
  in
  let rec watch_stdin () =
    Lwt_io.(read_line_opt stdin) >>= function
    | None ->
      Lwt_log.debug ~section "Got EOF. Sending a close frame." >>= fun () ->
      Lwt.return @@ close_connection ()
    | Some content -> watch_stdin ()
  in

  (* Main *)
  populate_db ~database:db_file ~token >>= fun () ->
  init_hashtbls();
  begin
    match git_or_db with
    | `Database _ -> Lwt.return None
    | `Git root ->
        let config = Irmin_fs.config ~root () in
        Store.Repo.create config >>=
        Store.master task >|= fun t -> Some t
  end >>= fun store ->
  (react_forever store) <?> watch_stdin ()


let query_db ~database =
  let open Sqlite3 in
  let open Sqlite3.Rc in
  let db = db_open database in
  let query = "select messages.ts as timestamp, channels.name as channel, users.name as user, messages.msg as text \
               from messages \
               left join channels on messages.channel_id = channels.id \
               left join users on messages.user_id = users.id"
  in
  let rows = ref [] in
  match exec_no_headers db query ~cb:(function
    | [| Some ts; Some channel; Some user; Some text |] ->
        let hum_ts = human_of_timestamp ts in
        let json = Ezjsonm.(dict [("ts",string hum_ts);("channel",string channel);
                                  ("user",string user);("text",string text)])
        in
        rows := json::!rows
    | _ -> failwith "impossible")
  with
  | OK ->
      ignore @@ db_close db;
      `A (List.rev !rows)
  | _ as e -> failwith (to_string e)
