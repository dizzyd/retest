-module(touch_rt).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "in"},
     {touch, "in"}].

run(Dir) ->
    {_, {_H, _M, S0}} = filelib:last_modified("in"),
    ok = timer:sleep(1000),
    {_, {_H, _M, S1}} = calendar:local_time(),
    1 = S1 - S0,
    ok.
