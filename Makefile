all:
	@rebar -C rebar_release.config compile escriptize

debug:
	@rebar compile escriptize

travis: all

clean:
	@rm -f retest
	@rebar clean
