%% _*_ mode: erlang _*_
{application, tinyerl,
 [{description, "An OTP application that does URL shortening"},
  {vsn, "0.1.0"},
  {modules, [base62, tinyerl, tinyerl_app, sherl_db, tinyerl_sup]},
  {registered, [tinyerl, tinyerl_sup]},
  {applications, [kernel, stdlib, mnesia]},
  {mod, {tinyerl_app, []}},
  {start_phases, []}
 ]}.
