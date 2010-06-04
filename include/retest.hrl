
-define(ABORT(Str, Args), retest_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), retest_log:log(console, Str, Args)).
-define(DEBUG(Str, Args), retest_log:log(debug, Str, Args)).
-define(INFO(Str, Args), retest_log:log(info, Str, Args)).
-define(WARN(Str, Args), retest_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), retest_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

