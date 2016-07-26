open Lwt.Infix

let section = Lwt_log.Section.make "slacklogger"

let get_websocket_uri ~token =
  let url = "https://slack.com/api/rtm.start?token=" ^ token in
  let open Ezjsonm in
  Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (resp, body) ->
  let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  Lwt_log.debug_f ~section "rtm_start: Response code: %d\n" code >>= fun () ->
  body |> Cohttp_lwt_body.to_string >>= fun body ->
  Lwt_log.debug_f ~section "rtm_start: Body of length: %d\n" (String.length body) >>= fun () ->
  let json = from_string body in
  if not (get_bool (find json ["ok"])) then
    failwith "rtm.start: unexpected response"
  else
    let uri_str = get_string (find json ["url"]) in
    Lwt_log.debug_f ~section "get_ws_uri: uri=%s\n" uri_str >|= fun () ->
    Uri.of_string uri_str

let mk_rtm_stream ~uri =
  let open Lwt.Infix in
  let open Websocket_lwt in
  let open Frame in

  (* Stream for sending data to consumer *)
  let data_stream, push_data = Lwt_stream.create () in

  (* Stream for closing the connection *)
  let wait_for_close, close_connection =
    let close_stream, push_close = Lwt_stream.create () in
    (Lwt_stream.get close_stream, fun () -> push_close None)
  in

  let https_uri =
    match Uri.scheme uri with
    | Some "wss" -> Uri.with_scheme uri (Some "https")
    | Some "ws" -> Uri.with_scheme uri (Some "http")
    | _ -> uri
  in
  (* See: https://github.com/vbmithr/ocaml-websocket/issues/59 *)

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
  Lwt.return (data_stream, close_connection)

let rec log_to_cmdline stream close_connection =
  Lwt_io.printf "Logging Slack activity. Press Ctrl+d to quit\n%!" >>= fun () ->
  let rec react_forever () =
    Lwt_stream.get stream >>= fun data ->
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

let rec log_to_db db stream close_connection =
  Lwt_io.printf "Logging Slack activity. Press Ctrl+d to quit\n%!" >>= fun () ->
  let rec react_forever () =
    Lwt_stream.get stream >>= fun data ->
    begin match data with
    | None -> Lwt.return @@ close_connection ()
    | Some data ->
    let open Ezjsonm in
    let open Sqlite3 in
    let open Sqlite3.Rc in
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
        | OK -> Lwt_log.info ~section q
        | _ -> failwith "message insert error"
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
        | OK -> Lwt_log.info ~section q
        | _ -> failwith "user insert error"
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
        | OK -> Lwt_log.info ~section q
        | _ -> failwith "user insert error"
        end

    (* Ignore all other messages for now *)
    | _ -> Lwt.return ()

    end >>= react_forever
  in
  let rec watch_stdin () =
    Lwt_io.(read_line_opt stdin) >>= function
    | None ->
      Lwt_log.debug ~section "Got EOF. Sending a close frame." >>= fun () ->
      Lwt.return @@ close_connection ()
    | Some content -> watch_stdin ()
  in
  react_forever () <?> watch_stdin ()

let create_tables db =
  let open Sqlite3 in
  let open Sqlite3.Rc in

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
    | OK -> ()
    | _ as e -> failwith @@ "create table channels failed: " ^ to_string e
  end

type info =
  {id   : string;
   name : string}

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

let populate_db db ~token =
  let open Sqlite3 in
  let open Sqlite3.Rc in

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
    | _ as e -> failwith @@ "populate channels failed: " ^ to_string e) users_info

let query_db db =
  let open Sqlite3 in
  let open Sqlite3.Rc in
  let query = "select messages.ts as timestamp, channels.name as channel, users.name as user, messages.msg as text \
               from messages \
               left join channels on messages.channel_id = channels.id \
               left join users on messages.user_id = users.id"
  in
  print_string "[";
  let first_done = ref false in
  begin
    match exec_no_headers db query ~cb:(fun row ->
      match row with
      | [| Some ts; Some channel; Some user; Some text |] ->
          let json = Ezjsonm.(dict [("ts",string ts);("channel",string channel);
                                    ("user",string user);("text",string text)])
          in
          if not !first_done then first_done := true else print_endline ",";
          Ezjsonm.to_channel stdout json
      | _ -> failwith "impossible");
    with
    | OK -> ()
    | _ as e -> failwith (to_string e)
  end;
  print_endline "]"
