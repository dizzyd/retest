-module(long_test_custom_timeout_rt).

-include_lib("eunit/include/eunit.hrl").

files() -> [].

run(_Dir) ->
    ok = timer:sleep(60000).
