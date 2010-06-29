{application, local132,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             local132_app, local132_sup, local132, local132_worker
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { local132_app, []}},
  {env, []}
 ]}.
