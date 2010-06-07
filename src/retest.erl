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
-export([sh/1, sh/2,
         sh_expect/2, sh_expect/3,
         sh_send/2,
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
    retest_sh:run(Cmd, []).

sh(Cmd, Opts) ->
    retest_sh:run(Cmd, Opts).

sh_expect(Ref, Regex) ->
    retest_sh:expect(Ref, Regex, []).

sh_expect(Ref, Regex, RegexOpts) ->
    retest_sh:expect(Ref, Regex, RegexOpts).

sh_send(Ref, Line) ->
    retest_sh:send(Ref, Line).

log(Level, Str) ->
    retest_log:log(Level, Str, []).

log(Level, Str, Args) ->
    retest_log:log(Level, Str, Args).





%% ====================================================================
%% Internal functions
%% ====================================================================
