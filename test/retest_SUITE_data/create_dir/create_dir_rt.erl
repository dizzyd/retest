-module(touch_rt).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{create_dir, "dir1"}].

run(Dir) ->
    true = filelib:is_dir("dir1"),
    ok.
