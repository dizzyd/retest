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
-module(retest_core).

-export([run/1]).

-include("retest.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

run(Args) ->
    %% Pre-load the retest app so that we get default configuration
    ok = case application:load(retest) of
            ok -> ok;
            {error,{already_loaded,retest}} -> ok
         end,

    %% Parse out command line arguments -- what's left is a list of
    %% files or directories to process
    case getopt:parse(options(), Args) of
        {ok, {Options, Targets}} ->

            %% Merge options into application env
            merge_options(Options),

            %% Make sure crypto is running
            ok = case crypto:start() of
                    ok -> ok;
                    {error, {already_started, crypto}} -> ok
                 end,

            %% Scan the list of targets and identify the specific test
            %% files
            case scan_targets(Targets, []) of
                [] ->
                    ?ABORT("No test files (*_rt.erl) found in these targets!\n", []);

                TestFiles ->
                    ?DEBUG("Test files: ~p\n", [TestFiles]),

                    %% Create our working directory for this run; link it to
                    %% out_dir/current
                    RunId = retest_utils:now_id_str(),
                    OutDir = filename:absname(filename:join(retest_config:get(out_dir), RunId)),
                    OutDirLink = filename:join(filename:dirname(OutDir), "current"),
                    ok = filelib:ensure_dir(OutDir ++ "/dummy"),
                    ok = symlink_target(os:type(), OutDirLink, OutDir),
                    retest_config:set(run_dir, OutDir),
                    retest_config:set(run_id, RunId),

                    run_tests(TestFiles, Targets)
            end;

        {error, {Reason, Data}} ->
            ?ABORT("getopt error: ~s ~p~n~n", [Reason, Data])
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%%
%% options accepted via getopt
%%
options() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",     boolean, "Show the program options"},
     {verbose,  $v, "verbose",  boolean, "Use debug level output"},
     {outdir,   $o, "outdir",   string,  "Directory to use for all test output"},
     {loglevel, $l, "loglevel", atom,    "Log output level: error, warn, info, debug"},
     {timeout,  $t, "timeout",  integer, "Timeout value to apply to all tests (default: 30 seconds)"}
    ].

merge_options([]) ->
    ok;
merge_options([{verbose, true} | Rest]) ->
    application:set_env(retest, log_level, debug),
    merge_options(Rest);
merge_options([{outdir, Dir} | Rest]) ->
    application:set_env(retest, out_dir, Dir),
    merge_options(Rest);
merge_options([{loglevel, Level} | Rest]) ->
    case lists:member(Level, [error, warn, info, debug]) of
        true ->
            application:set_env(retest, log_level, Level);
        false ->
            ok
    end,
    merge_options(Rest);
merge_options([{timeout, Timeout} | Rest]) ->
    application:set_env(retest, timeout, Timeout),
    merge_options(Rest);
merge_options([_Option | Rest]) ->
    merge_options(Rest).


is_test_file(Filename) ->
    filelib:is_regular(Filename) andalso lists:suffix("_rt.erl", Filename).

test_files(Dir) ->
    filelib:wildcard(filename:join(Dir, "*_rt.erl")) ++
        filelib:wildcard(filename:join(Dir, "*/*_rt.erl")).

scan_targets([], Acc) ->
    lists:reverse(Acc);
scan_targets([Target | Rest], Acc) ->
    case is_test_file(Target) of
        true ->
            scan_targets(Rest, [filename:absname(Target) | Acc]);
        false ->
            case filelib:is_dir(Target) of
                true ->
                    NewRest = test_files(Target) ++ Rest,
                    scan_targets(NewRest, Acc);
                false ->
                    ?INFO("Ignoring ~p target; no tests found.\n", [Target]),
                    scan_targets(Rest, Acc)
            end
    end.

run_tests([], _) ->
    ok;
run_tests([TestFile | Rest], Targets) ->
    %% Load retest.config if it exists
    Config = retest_config:new(filename:dirname(TestFile)),

    %% Compile/load the module
    Module = load_test(TestFile),
    ?DEBUG("Compiled ~p\n", [Module]),

    %% Create working directory for test; ensure clean
    Dir = filename:join(retest_config:get(run_dir), atom_to_list(Module)),
    ok = filelib:ensure_dir(Dir ++ "/dummy"),

    Timeout = retest_config:get(Config, timeout),
    ?DEBUG("Running ~p\n", [Module]),
    {Pid, Mref} = spawn_monitor(fun() ->
                                    run_test(Config, Module, TestFile, Dir, Targets)
                                end),
    receive
        {'DOWN', Mref, process, Pid, normal} ->
            %% Test completed successfully, move on to the next one
            ?DEBUG("Completed ~p\n", [Module]),
            erlang:yield(),
            run_tests(Rest, Targets);

        {'DOWN', Mref, process, Pid, Reason} ->
            ?ABORT("Test ~p failed: ~p\n", [Module, Reason])

    after Timeout ->
            ?ABORT("Test ~p timed out.\n", [Module])
    end.

run_test(_Config, Module, TestFile, TargetDir, Targets) ->
    BaseDir = filename:dirname(TestFile),

    %% Set the module name in the pdict so that calls back into\\\
    %% the API can have some context
    erlang:put(retest_module, Module),

    %% Invoke setup/1 (optional)
    case erlang:function_exported(Module, setup, 1) of
        true ->
            ?DEBUG("setup args: ~p\n", [Targets]),
            case catch Module:setup(Targets) of
                ok ->
                    ?DEBUG("Test ~p invoked setup callback successfully\n", [Module]),
                    ok;
                Reason0 ->
                    ?ABORT("Test ~p failed to setup: ~p\n",
                        [Module, Reason0])
            end;
        false -> ok
    end,

    %% Invoke files/0
    case (catch Module:files()) of
        List when is_list(List) ->
            case retest_utils:execute_cmds(List, BaseDir, TargetDir) of
                ok ->
                    ok;
                {error, Reason1} ->
                    ?ABORT("Test ~p failed to install: ~p\n",
                        [Module, Reason1])
            end;

        Error1 ->
            ?ABORT("Test ~p failed when invoking ~p:files/1:\n~p\n",
                   [Module, Module, Error1])
    end,

    OldCwd = retest_utils:get_cwd(),
    ok = file:set_cwd(TargetDir),

    %% Invoke run/1
    case (catch Module:run(TargetDir)) of
        ok ->
            ?INFO("Test ~p successful.\n", [Module]),
            cleanup_sh(),
            ok = file:set_cwd(OldCwd),
            ok;
        Error2 ->
            cleanup_sh(),
            ok = file:set_cwd(OldCwd),
            ?ABORT("Test ~p failed when invoking ~p:run/1:\n~p\n",
                   [Module, Module, Error2])
    end.



cleanup_sh() ->
    ?DEBUG("Cleaning up: ~p\n", [erlang:get()]),
    retest_sh:stop_all().


load_test(TestFile) ->
    Opts = [binary, report, export_all],
    case compile:file(TestFile, Opts) of
        {ok, Module, Binary} ->
            case code:load_binary(Module, TestFile, Binary) of
                {module, Module} ->
                    Module;
                {error, Reason} ->
                    ?ABORT("Failed load test code ~p: ~p\n",
                           [TestFile, Reason])
            end;
        Error ->
            ?ABORT("Failed to compile test: ~p\n", [Error])
    end.

symlink_target({win32, nt}, OutDirLink, OutDir) ->
    os:cmd(?FMT("rmdir \"~s\" & mklink /d \"~s\" \"~s\"",
          [OutDirLink, OutDirLink, OutDir])),
    ok;
symlink_target(_, OutDirLink, OutDir) ->
    [] = os:cmd(?FMT("rm -f ~s; ln -sf ~s ~s",
            [OutDirLink, OutDir, OutDirLink])),
    ok.
