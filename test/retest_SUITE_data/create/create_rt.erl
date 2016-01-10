-module(create_rt).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{create, "file1", file1()}].

run(Dir) ->
    ?assertMatch({ok, [{contents, "12345ABCDEF"}]},
                 file:consult(Dir ++ "/file1")),
    ok.

file1() ->
    "{contents, \"12345ABCDEF\"}.\n".
