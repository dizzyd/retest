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
    ok = application:load(retest),

    %% Parse out command line arguments -- what's left is a list of
    %% files or directories to process
    case getopt:parse(options(), Args) of
        {ok, {Options, Targets}} ->

            %% Merge options into application env
            merge_options(Options),

            %% Make sure crypto is running
            crypto:start(),

            %% Scan the list of targets and identify the specific test
            %% files
            case scan_targets(Targets, []) of
                [] ->
                    ?ABORT("No test files found in these targets!\n", []);

                TestFiles ->
                    ?DEBUG("Test files: ~p\n", [TestFiles]),

                    %% Create our working directory for this run; link it to
                    %% out_dir/current
                    RunId = retest_utils:now_id_str(),
                    OutDir = filename:absname(filename:join(retest_config:get(out_dir), RunId)),
                    OutDirLink = filename:join(filename:dirname(OutDir), "current"),
                    ok = filelib:ensure_dir(OutDir ++ "/dummy"),
                    [] = os:cmd(?FMT("rm -f ~s; ln -sf ~s ~s", [OutDirLink, OutDir,
                                                                OutDirLink])),
                    retest_config:set(run_dir, OutDir),
                    retest_config:set(run_id, RunId),

                    run_tests(TestFiles)
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
     {loglevel, $l, "loglevel", atom,    "Log output level: error, warn, info, debug"}
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


run_tests([]) ->
    ok;
run_tests([TestFile | Rest]) ->
    %% Load retest.config if it exists
    Config = retest_config:new(filename:dirname(TestFile)),

    %% Compile/load the module
    Module = load_test(TestFile),
    ?DEBUG("Compiled ~p\n", [Module]),

    %% Create working directory for test; ensure clean
    Dir = filename:join(retest_config:get(run_dir), Module),
    ok = filelib:ensure_dir(Dir ++ "/dummy"),

    ?DEBUG("Running ~p\n", [Module]),
    {Pid, Mref} = spawn_monitor(fun() -> run_test(Config, Module, TestFile, Dir) end),
    receive
        {'DOWN', Mref, process, Pid, normal} ->
            %% Test completed successfully, move on to the next one
            ?DEBUG("Completed ~p\n", [Module]),
            erlang:yield(),
            run_tests(Rest);

        {'DOWN', Mref, process, Pid, Reason} ->
            ?ABORT("Test ~p failed: ~p\n", [Module, Reason])

    after 30000 ->
            ?ABORT("Test ~p timed out.\n", [Module])
    end.


run_test(Config, Module, TestFile, TargetDir) ->
    BaseDir = filename:dirname(TestFile),

    %% Set the module name in the pdict so that calls back into\\\
    %% the API can have some context
    erlang:put(retest_module, Module),

    %% Invoke files/0
    case (catch Module:files()) of
        List when is_list(List) ->
            case execute_install(List, BaseDir, TargetDir) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?ABORT("Test ~p failed to install: ~p\n", [Reason])
            end;

        Error1 ->
            ?ABORT("Test ~p failed when invoking ~p:files/1: ~p\n",
                   [Module, Module, Error1])
    end,

    OldCwd = retest_utils:get_cwd(),
    ok = file:set_cwd(TargetDir),

    %% Invoke run/1
    case (catch Module:run(TargetDir)) of
        ok ->
            ?INFO("Test ~p successful.\n", [Module]),
            cleanup_sh(),
            file:set_cwd(OldCwd),
            ok;
        Error2 ->
            cleanup_sh(),
            file:set_cwd(OldCwd),
            ?ABORT("Test ~p failed when invoking ~p:run/1: ~p\n",
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


execute_install([], _BaseDir, _TargetDir) ->
    ok;
execute_install([{copy, In} | Rest], BaseDir, TargetDir) ->
    execute_install([{copy, In, ""} | Rest], BaseDir, TargetDir);
execute_install([{copy, In, Out} | Rest], BaseDir, TargetDir) ->
    InFile = filename:join(BaseDir, In),
    OutFile = filename:join(TargetDir, Out),
    case filelib:is_dir(InFile) of
        true ->
            ok;
        false ->
            ok = filelib:ensure_dir(OutFile)
    end,
    Cmd = ?FMT("cp -R ~p ~p", [filename:join(BaseDir, In),
                               filename:join(TargetDir, Out)]),
    retest_utils:sh(Cmd, []),
    execute_install(Rest, BaseDir, TargetDir);
execute_install([{template, In, Out, Ctx} | Rest], BaseDir, TargetDir) ->
    {ok, InFileData} = file:read_file(filename:join(BaseDir, In)),
    OutFile = filename:join(TargetDir, Out),
    ok = filelib:ensure_dir(OutFile),
    case file:write_file(OutFile, render(InFileData, Ctx)) of
        ok ->
            ?DEBUG("Templated ~p\n", [OutFile]),
            execute_install(Rest, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to template ~p: ~p\n", [OutFile, Reason])
    end;
execute_install([{create, Out, Contents} | Rest], BaseDir, TargetDir) ->
    OutFile = filename:join(TargetDir, Out),
    ok = filelib:ensure_dir(OutFile),
    case file:write_file(OutFile, Contents) of
        ok ->
            ?DEBUG("Created ~p\n", [OutFile]),
            execute_install(Rest, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to create ~p: ~p\n", [OutFile, Reason])
    end;
execute_install([{replace, Out, Regex, Replacement} | Rest],
                BaseDir, TargetDir) ->
    execute_install([{replace, Out, Regex, Replacement, []} | Rest], BaseDir, TargetDir);
execute_install([{replace, Out, Regex, Replacement, Opts} | Rest],
                BaseDir, TargetDir) ->
    Filename = filename:join(TargetDir, Out),
    {ok, OrigData} = file:read_file(Filename),
    Data = re:replace(OrigData, Regex, Replacement, [global, {return, binary}] ++ Opts),
    case file:write_file(Filename, Data) of
        ok ->
            ?DEBUG("Edited ~s: s/~s/~s/\n", [Filename, Regex, Replacement]),
            execute_install(Rest, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to edit ~p: ~p\n", [Filename, Reason])
    end;
execute_install([Other | _Rest], _BaseDir, _TargetDir) ->
    {error, {unsupported_operation, Other}}.



%%
%% Render a binary to a string, using mustache and the specified context
%%
render(Bin, Context) ->
    %% Be sure to escape any double-quotes before rendering...
    Str = re:replace(Bin, "\"", "\\\\\"", [global, {return,list}]),
    mustache:render(Str, Context).


