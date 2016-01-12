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
-module(retest_utils).

-export([get_cwd/0,
         sh/2, sh/3,
         find_files/2,
         now_str/0,
         now_id_str/0,
         abort/2,
         load_module/1,
         execute_cmds/3]).

-include("retest.hrl").

%% ====================================================================
%% Public API
%% ====================================================================

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

sh(Command, Env) ->
    sh(Command, Env, get_cwd()).

sh(Command, Env, Dir) ->
    ?DEBUG("sh: ~s\n~p\n", [Command, Env]),
    Port = open_port({spawn, Command}, [{cd, Dir}, {env, Env}, exit_status, {line, 16384},
                                        use_stdio, stderr_to_stdout]),
    case sh_loop(Port) of
        ok ->
            ok;
        {error, Rc} ->
            ?ABORT("~s failed with error: ~w\n", [Command, Rc])
    end.

find_files(Dir, Regex) ->
    filelib:fold_files(Dir, Regex, true, fun(F, Acc) -> [F | Acc] end, []).

now_str() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b",
				[Year, Month, Day, Hour, Minute, Second])).


%%
%% Construct a string suitable for use as a unique ID
%%
now_id_str() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    ?FMT("~w~2..0w~2..0w_~2..0w~2..0w~2..0w", [Y, M, D, H, Min, S]).



abort(String, Args) ->
    ?ERROR(String, Args),
    throw(abort).

load_module(File) ->
    Opts = [binary, report, export_all],
    case compile:file(File, Opts) of
        {ok, Module, Binary} ->
            case code:load_binary(Module, File, Binary) of
                {module, Module} ->
                    {ok, Module};
                {error, Reason} ->
                    ?ABORT("Failed load code ~p: ~p\n",
                           [File, Reason])
            end;
        Error ->
            ?ABORT("Failed to compile: ~p\n", [Error])
    end.

execute_cmds([], _BaseDir, _TargetDir) ->
    ok;
execute_cmds([{copy, In} | Rest], BaseDir, TargetDir) ->
    execute_cmds([{copy, In, In} | Rest], BaseDir, TargetDir);
execute_cmds([{copy, In, Out} | Rest], BaseDir, TargetDir) ->
    InFile = filename:join(BaseDir, In),
    OutFile = filename:join(TargetDir, Out),
    Cmd = copy_command(os:type(), InFile, OutFile),
    retest_utils:sh(Cmd, []),
    execute_cmds(Rest, BaseDir, TargetDir);
execute_cmds([{template, In, Out, Ctx} | Rest], BaseDir, TargetDir) ->
    {ok, InFileData} = file:read_file(filename:join(BaseDir, In)),
    OutFile = filename:join(TargetDir, Out),
    ok = filelib:ensure_dir(OutFile),
    case file:write_file(OutFile, render(InFileData, Ctx)) of
        ok ->
            ?DEBUG("Templated ~p\n", [OutFile]),
            execute_cmds(Rest, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to template ~p: ~p\n", [OutFile, Reason])
    end;
execute_cmds([{create, Out, Contents} | Rest], BaseDir, TargetDir) ->
    OutFile = filename:join(TargetDir, Out),
    ok = filelib:ensure_dir(OutFile),
    case file:write_file(OutFile, Contents) of
        ok ->
            ?DEBUG("Created ~p\n", [OutFile]),
            execute_cmds(Rest, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to create ~p: ~p\n", [OutFile, Reason])
    end;
execute_cmds([{replace, Out, Regex, Replacement} | Rest],
                BaseDir, TargetDir) ->
    execute_cmds([{replace, Out, Regex, Replacement, []} | Rest], BaseDir, TargetDir);
execute_cmds([{replace, Out, Regex, Replacement, Opts} | Rest],
                BaseDir, TargetDir) ->
    Filename = filename:join(TargetDir, Out),
    {ok, OrigData} = file:read_file(Filename),
    Data = re:replace(OrigData, Regex, Replacement, [global, {return, binary}] ++ Opts),
    case file:write_file(Filename, Data) of
        ok ->
            ?DEBUG("Edited ~s: s/~s/~s/\n", [Filename, Regex, Replacement]),
            execute_cmds(Rest, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to edit ~p: ~p\n", [Filename, Reason])
    end;
execute_cmds([{touch, Filename0} | Rest], BaseDir, TargetDir) ->
    Filename1 = filename:join(TargetDir, Filename0),
    case file:change_time(Filename1, calendar:local_time()) of
        ok ->
            ?DEBUG("Touched ~s\n", [Filename1]),
            execute_cmds(Rest, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to touch ~p: ~p\n", [Filename1, Reason])
    end;
execute_cmds([Other | _Rest], _BaseDir, _TargetDir) ->
    {error, {unsupported_operation, Other}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

sh_loop(Port) ->
    receive
        {Port, {data, {_, Line}}} ->
            ?CONSOLE("~s\n", [Line]),
            sh_loop(Port);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Rc}} ->
            {error, Rc}
    end.

copy_command({win32, nt}, InFile, OutFile) ->
    case filelib:is_dir(InFile) of
        true ->
            ?FMT("cmd /q /c xcopy /y /i /s \"~s\" \"~s\"",
                [filename:nativename(InFile),
                 filename:nativename(OutFile)]);
        false ->
            ok = filelib:ensure_dir(OutFile),
            ?FMT("cmd /q /c copy /y \"~s\" \"~s\"",
                [filename:nativename(InFile),
                 filename:nativename(OutFile)])
    end;
copy_command(_, InFile, OutFile) ->
    case filelib:is_dir(InFile) of
        true ->
            ok;
        false ->
            ok = filelib:ensure_dir(OutFile)
    end,
    ?FMT("cp -R ~p ~p", [InFile, OutFile]).

%%
%% Render a binary to a string, using mustache and the specified context
%%
render(Bin, Context) ->
    %% Be sure to escape any double-quotes before rendering...
    Str = re:replace(Bin, "\"", "\\\\\"", [global, {return,list}]),
    mustache:render(Str, Context).
