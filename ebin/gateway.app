{application, gateway,
 [{description, "Gateway Server"},
  {vsn, "0.1.0"},
  {modules, [gw_app]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {gw_app, []}},
  {env, [{port, 8000}]}]}.
