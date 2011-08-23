all:
	@rebar compile escriptize

clean:
	@rm -f retest
	@rebar clean
