-module(replace_rt).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "in"},
     {replace, "in", "placeholder", "value"}].

run(Dir) ->
    ?assertMatch({ok, [{value, "value"}]},
                 file:consult(Dir ++ "/" ++ "in")),
    ok.
