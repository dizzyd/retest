%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% retest: Testing Framework
%%
%% Copyright (c) 2010 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(retest_sh).

-export([run/2,
         stop/1,
         stop_all/0,
         expect/2, expect/3,
         send/2]).

-include("retest.hrl").

-record(sh, {pid, port, opts}).

%% ====================================================================
%% API for test writers
%% ====================================================================

run(Cmd, Opts) ->
    Dir = proplists:get_value(dir, Opts, retest_utils:get_cwd()),
    Env = proplists:get_value(env, Opts, []),

    %% Try to be smart about the provided command. If it's got any && or ; in it,
    %% it's a multi-statement bit of shell, so there's no good way for use to track
    %% the PID.
    case re:run(Cmd, "(&&|;)") of
        nomatch ->
            ActualCmd = ?FMT("/bin/sh -c \"echo $$; exec ~s\"", [Cmd]);
        _ ->
            ActualCmd = Cmd,
            case proplists:get_bool(async, Opts) of
                true ->
                    ?ABORT("Async option not allowed with retest_sh:run/2 when command"
                           "contains '&&' or ';': ~s\n", [Cmd]);
                false ->
                    ok
            end
    end,

    Port = open_port({spawn, ActualCmd}, [{cd, Dir}, {env, Env},
                                          exit_status, {line, 16384},
                                          use_stdio, stderr_to_stdout]),
    case proplists:get_bool(async, Opts) of
        true ->
            Pid = read_pid(Port),
            Ref = make_ref(),
            erlang:put(Ref, #sh{ pid = Pid, port = Port, opts = Opts}),
            Ref;
        false ->
            acc_loop(Port, [])
    end.

stop(Ref) ->
    #sh { pid = Pid, port = Port } = erlang:get(Ref),
    _ = os:cmd(?FMT("kill ~s", [Pid])),
    exit_loop(Port).

stop_all() ->
    _ = [ {ok, _} = stop(Ref) || {Ref, Sh} <- erlang:get(),
                                 is_record(Sh, sh)],
    ok.

expect(Ref, Regex) ->
    expect(Ref, Regex, []).

expect(Ref, Regex, RegexOpts) ->
    #sh{ port = Port } = erlang:get(Ref),
    expect_loop(Port, Regex, RegexOpts).

send(Ref, Line) ->
    #sh { port = Port } = erlang:get(Ref),
    port_command(Port, Line).



%% ====================================================================
%% Internal functions
%% ====================================================================

read_pid(Port) ->
    receive
        {Port, {data, {eol, Pid}}} ->
            Pid;
        {Port, {exit_status, Rc}} ->
            {error, {stopped, Rc}}
    end.

exit_loop(Port) ->
    receive
        {Port, {data, {_, Line}}} ->
            ?DEBUG("~p: ~s\n", [erlang:get(retest_module), Line]),
            exit_loop(Port);

        {Port, {exit_status, Rc}} ->
            {ok, Rc}
    end.


acc_loop(Port, Acc) ->
    receive
        {Port, {data, {_, Line}}} ->
            ?DEBUG("~p: ~s\n", [erlang:get(retest_module), Line]),
            acc_loop(Port, [Line | Acc]);
        {Port, {exit_status, 0}} ->
            {ok, lists:reverse(Acc)};
        {Port, {exit_status, Rc}} ->
            {error, {stopped, {Rc, lists:reverse(Acc)}}};
        {Port, Other} ->
            ?DEBUG("Other: ~p\n", [Other]),
            acc_loop(Port, Acc)
    end.


expect_loop(Port, Regex, RegexOpts) ->
    receive
        {Port, {data, {Unknown, Line}}} ->
            ?DEBUG("~p ~p: ~s\n", [erlang:get(retest_module), Unknown, Line]),
            case re:run(Line, Regex, RegexOpts) of
                {match, Captured} ->
                    {ok, Captured};
                match ->
                    {ok, Line};
                nomatch ->
                    expect_loop(Port, Regex, RegexOpts)
            end;
        {Port, {exit_status, Rc}} ->
            {error, {stopped, Rc}}
    end.

