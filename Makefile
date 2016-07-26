all:
	ocamlbuild -pkgs lwt,cohttp,cohttp.lwt,ezjsonm,rresult,websocket \
		-pkgs websocket.lwt,sqlite3,slacko,cmdliner,str slacklogger_app.native

clean:
	rm -f *~
	ocamlbuild -clean
