{application, termchat,
 [{vsn, "1.0.0"},
  {modules, [termchat, database]},
  {registered, [termchat]},
  {mod, {termchat, []}},
  {applications, [mnesia]},
  {env,
   [{port, 31031},
    {delimiter, 0}]}
 ]}.
