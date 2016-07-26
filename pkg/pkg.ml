#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "slacklogger" @@ fun c ->
  Ok [
    Pkg.lib ~exts:Exts.library "lib/slacklogger";
    Pkg.bin ~dst:"slacklogger" "lib/slacklogger_app";
  ]
