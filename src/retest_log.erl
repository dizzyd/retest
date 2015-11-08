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
-module(retest_log).

-export([log/3]).

%% ===================================================================
%% Public API
%% ===================================================================

log(Level, Str, Args) ->
    LogLevel = case application:get_env(retest, log_level) of
                    {ok, Val} -> Val;
                    undefined -> undefined
               end,
    case should_log(LogLevel, Level) of
        true ->
            io:format(timestamp() ++ log_prefix(Level) ++ Str, Args),
            ok;
        false ->
            ok
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

should_log(_, console)   -> true;
should_log(debug, _)     -> true;
should_log(info, debug)  -> false;
should_log(info, _)      -> true;
should_log(warn, debug)  -> false;
should_log(warn, info)   -> false;
should_log(warn, _)      -> true;
should_log(error, error) -> true;
should_log(error, _)     -> false;
should_log(_, _)         -> false.

log_prefix(console) -> "";
log_prefix(debug) -> "DEBUG: " ;
log_prefix(info)  -> "INFO: ";
log_prefix(warn)  -> "WARN: ";
log_prefix(error) -> "ERROR: ".

timestamp() ->
    "[" ++ format_time(localtime_ms()) ++ "] ".

localtime_ms() ->
    {_, _, Micro} = Now = os:timestamp(),
    {Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    {Date, {Hours, Minutes, Seconds, Micro div 1000 rem 1000}}.

format_time({{Y, M, D}, {H, Mi, S, Ms}}) ->
    io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
        io_lib:format("~2..0b:~2..0b:~2..0b.~3..0b", [H, Mi, S, Ms]);
format_time({{Y, M, D}, {H, Mi, S}}) ->
    io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
        io_lib:format("~2..0b:~2..0b:~2..0b", [H, Mi, S]).
