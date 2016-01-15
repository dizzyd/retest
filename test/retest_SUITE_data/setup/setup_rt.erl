-module(setup_rt).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "file1"}].

setup([Target]) ->
    {ok, Data} = file:consult(Target ++ "/" ++ "file1"),
    erlang:put(data, Data),
    ok.

run(Dir) ->
    {ok, Data0} = file:consult(Dir ++ "/" ++ "file1"),
    Data1 = erlang:get(data),
    ?assertMatch(Data0, Data1),
    ok.
