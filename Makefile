all:
	(cd src;$(MAKE))

xref: all
	@erl -noshell -pz ebin deps/*/ebin -eval "io:format(\"~p~n\", [xref:d(\"ebin\")]), c:q()."

clean:
	@rm -rf playii-core.tar.gz
	(cd src;$(MAKE) clean)

release: clean
	(cd src;$(MAKE) release=1)

dist: release
	find . -type f \
	| egrep "^./priv|^./ebin" \
	| egrep ".js$$|.beam$$|.app$$" \
	| egrep -v "_keygen" \
	| xargs tar -cvzf playii-core.tar.gz

