-module(retest_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
suite() -> [].

all() ->
    [basic_run, basic_run_all_args, directory_does_not_exist,
     wrong_arguments, test_exceeds_timeout,
     create, copy, template, replace, touch, create_dir,
     logging, shell_api, shell_async_api, shell_async_api].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

retest_run(Dir, Config) ->
    retest_run(Dir, [], Config).

retest_run(Dir, Args, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    retest_core:run(Args ++ [DataDir ++ "/" ++ Dir]).

basic_run(doc) -> ["Does retest run?"];
basic_run(suite) -> [];
basic_run(Config) when is_list(Config) ->
    ok = retest_run("basic", Config).

basic_run_all_args(doc) -> ["Does retest run?"];
basic_run_all_args(suite) -> [];
basic_run_all_args(Config) when is_list(Config) ->
    ok = retest_run("basic", ["--verbose", "--outdir", "out_dir",
                              "--loglevel", "debug"], Config).

directory_does_not_exist(doc) -> ["Does retest fail when provided with non existing test files?"];
directory_does_not_exist(suite) -> [];
directory_does_not_exist(Config) when is_list(Config) ->
    ?assertException(throw, abort, retest_run("non_existent", Config)).

wrong_arguments(doc) -> ["Does retest correctly handle wrong arguments"];
wrong_arguments(suite) -> [];
wrong_arguments(Config) when is_list(Config) ->
    ?assertException(throw, abort, retest_run("basic", ["--verbos"], Config)).

test_exceeds_timeout(doc) -> ["Does retest correctly tests that exceed the configured timeout"];
test_exceeds_timeout(suite) -> [];
test_exceeds_timeout(Config) when is_list(Config) ->
    ?assertException(throw, abort, retest_run("long_test", Config)).

copy(doc) -> ["Test copy directive"];
copy(suite) -> [];
copy(Config) when is_list(Config)->
    ok = retest_run("copy", Config).

create(doc) -> ["Test create directive"];
create(suite) -> [];
create(Config) when is_list(Config)->
    ok = retest_run("create", Config).

template(doc) -> ["Test template directive"];
template(suite) -> [];
template(Config) when is_list(Config)->
    ok = retest_run("template", Config).

replace(doc) -> ["Test replace directive"];
replace(suite) -> [];
replace(Config) when is_list(Config)->
    ok = retest_run("replace", Config).

touch(doc) -> ["Test touch directive"];
touch(suite) -> [];
touch(Config) when is_list(Config)->
    ok = retest_run("touch", Config).

create_dir(doc) -> ["Test create_dir directive"];
create_dir(suite) -> [];
create_dir(Config) when is_list(Config)->
    ok = retest_run("create_dir", Config).

logging(doc) -> ["Test logging"];
logging(suite) -> [];
logging(Config) when is_list(Config)->
    ok = retest:log(debug, "debug message"),
    ok = retest:log(debug, "debug message: ~p", ["args"]),
    ok = retest:log(info, "info message"),
    ok = retest:log(info, "info message: ~p", ["args"]),
    ok = retest:log(warn, "warn message"),
    ok = retest:log(warn, "warn message: ~p", ["args"]),
    ok = retest:log(error, "error message"),
    ok = retest:log(error, "error message: ~p", ["args"]).

shell_api(doc) -> ["Test Shell API"];
shell_api(suite) -> [];
shell_api(Config) when is_list(Config)->
    {ok, [_Pid, "test"]} = retest:sh("echo test").

shell_async_api(doc) -> ["Test Shell async API"];
shell_async_api(suite) -> [];
shell_async_api(Config) when is_list(Config)->
    Ref1 = retest:sh("echo test1", [async]),
    {ok, [{0,5}]} = retest:sh_expect(Ref1, "test1", []),
    _Ref2 = retest:sh("sleep 5", [async]),
    timer:sleep(1000),
    ok = retest_sh:stop_all().
