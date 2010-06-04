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
-module(retest_config).

-export([new/1,
         get/1, get/2,
         set/2]).

-include("retest.hrl").

-record(config, { dir,
                  opts }).


%% ===================================================================
%% Public API
%% ===================================================================

new(Dir) ->
    %% Load terms from retest.config, if it exists
    ConfigFile = filename:join([Dir, "retest.config"]),
    case file:consult(ConfigFile) of
        {ok, Terms} ->
            Opts = Terms;
        {error, enoent} ->
            Opts = [];
        Other ->
            Opts = undefined, % Keep erlc happy
            ?ABORT("Failed to load ~s: ~p\n", [ConfigFile, Other])
    end,
    #config { dir = Dir, opts = Opts }.


get(Key) ->
    case application:get_env(retest, Key) of
        undefined ->
            undefined;
        {ok, Value} ->
            Value
    end.

get(Config, Key) ->
    case proplists:get_value(Key, Config#config.opts) of
        undefined ->
            ?MODULE:get(Key);
        Value ->
            Value
    end.

set(Key, Value) ->
    application:set_env(retest, Key, Value).

%% ===================================================================
%% Internal functions
%% ===================================================================
