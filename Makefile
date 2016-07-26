all:
	ocaml pkg/pkg.ml build

clean:
	find . -name *~ | xargs rm -f
	rm -rf _build slacklogger slacklogger.install
