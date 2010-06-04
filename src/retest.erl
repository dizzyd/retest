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
-module(retest).

-export([main/1]).

%% API for test writers
-export([sh/1, sh/3,
         log/2, log/3]).

-include("retest.hrl").

%% ====================================================================
%% Script entry point
%% ====================================================================

main(Args) ->
    case catch(retest_core:run(Args)) of
        ok ->
            ok;
        {error, failed} ->
            halt(1);
        Error ->
            %% Nothing should percolate up from retest_core; dump this error to console
            io:format("Uncaught error in retest_core: ~p\n", [Error]),
            halt(1)
    end.


%% ====================================================================
%% API for test writers
%% ====================================================================

sh(Cmd) ->
    sh(Cmd, [], retest_utils:get_cwd()).

sh(Cmd, Env, Dir) ->
    Port = open_port({spawn, Cmd}, [{cd, Dir}, {env, Env}, exit_status, {line, 16384},
                                    use_stdio, stderr_to_stdout]),
    sh_loop(Port, []).


log(Level, Str) ->
    retest_log:log(Level, Str, []).

log(Level, Str, Args) ->
    retest_log:log(Level, Str, Args).


%% ====================================================================
%% Internal functions
%% ====================================================================

sh_loop(Port, Acc) ->
    receive
        {Port, {data, {_, Line}}} ->
            ?DEBUG("~p: ~s\n", [erlang:get(retest_module), Line]),
            sh_loop(Port, [Line | Acc]);
        {Port, {exit_status, 0}} ->
            {ok, lists:reverse(Acc)};
        {Port, {exit_status, Rc}} ->
            {error, Rc}
    end.
