-module(template_rt).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "in"},
     {template, "in", "out",
      dict:from_list([{val1, "value1"},
                      {val2, "value2"}])}].

run(Dir) ->
    ?assertMatch({ok, [[{value1, "value1"}, {value2, "value2"}]]},
                 file:consult(Dir ++ "/" ++ "out")),
    ok.
