all:
	@rebar -C rebar_release.config compile escriptize

debug:
	@rebar compile escriptize

clean:
	@rm -f retest
	@rebar clean
