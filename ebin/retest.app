{application, retest,
 [
  {description, "retest: Testing Framework"},
  {vsn, "1"},
  {modules, [
             getopt,
             mustache,
             retest,
             retest_core,
             retest_config,
             retest_log,
             retest_sh,
             retest_utils
             ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, [
         {out_dir, "rt.work"},
         {log_level, error},
         {timeout, 30000}
        ]}
 ]}.
