all:
	ocaml pkg/pkg.ml build

clean:
	rm -rf _build slacklogger slacklogger.install
