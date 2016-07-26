open Lwt.Infix

let token =
  let doc = "Slack API access token." in
  Cmdliner.Arg.(value & opt (some string) None & info ["t"; "token"] ~docv:"TOKEN" ~doc)

let db =
  let doc = "Sqlite3 database file." in
  Cmdliner.Arg.(value & opt (some string) None & info ["d"; "database"] ~docv:"DATABASE" ~doc)

let git =
  let doc = "Git repo containing Sqlite3 database file in the root directory." in
  Cmdliner.Arg.(value & opt (some string) None & info ["g"; "git"] ~docv:"GIT_REPO" ~doc)

let query =
  let doc = "Query the database." in
  Cmdliner.Arg.(value & flag & info ["q"; "query"] ~doc)

let init_tables =
  let doc = "Initialize database with tables." in
  Cmdliner.Arg.(value & flag & info ["i"; "init-tables"] ~doc)

let verbose =
  let doc = "Verbose mode." in
  Cmdliner.Arg.(value & flag & info ["v"; "verbose"] ~doc)

let info =
  let doc = "Log Slack messages" in
  Cmdliner.Term.info "slack-logger" ~doc

let execute token db init_tables verbose query git =
  let open Slacklogger in
  Lwt_main.run begin
  if verbose then Lwt_log.add_rule "*" Lwt_log.Info;
  if query then
    match db with
    | None -> failwith "Database file required for querying!"
    | Some db_file ->
        let json = query_db db_file in
        Ezjsonm.to_channel ~minify:false stdout json;
        Lwt.return ()
  else
  match token with
  | None -> failwith "Token is required for connecting to Slack."
  | Some token ->
  get_rtm_uri token >>= fun uri ->
  mk_rtm_stream ~uri >>= fun rtm_stream ->
  let init_db db_file =
    if init_tables then begin
      try create_tables db_file
      with _ -> failwith "Database already has tables?"
    end;
    populate_db ~database:db_file ~token >>= fun () ->
    Lwt.return ()
  in
  match db with
  | Some db_file ->
      init_db db_file >>= fun () ->
      log_to_db (`Database db_file) rtm_stream
  | None ->
   begin match git with
     | Some git_repo ->
         let db_file = Filename.concat git_repo "db" in
         init_db db_file >>= fun () ->
         log_to_db (`Git git_repo) rtm_stream
     | None -> log_to_cmdline rtm_stream
   end
  end

let execute_t = Cmdliner.Term.(pure execute $ token $ db $ init_tables $ verbose
                  $ query $ git)

let () =
  match Cmdliner.Term.eval (execute_t, info) with
    | `Error _ -> exit 1
    | _ -> exit 0
