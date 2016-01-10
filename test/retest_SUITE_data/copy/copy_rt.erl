-module(copy_rt).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "file1"},
     {copy, "file1", "file2"}].

run(Dir) ->
    ?assertMatch({ok, [{contents, "12345ABCDEF"}]},
                 file:consult(Dir ++ "/file1")),
    ?assertMatch({ok, [{contents, "12345ABCDEF"}]},
                 file:consult(Dir ++ "/file2")),
    ok.
