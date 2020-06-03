{application, termchat_sup,
 [{vsn, "1.0.0"},
  {modules, [termchat_sup, termchat_serv, database]},
  {registered, [termchat_sup]},
  {mod, {termchat_sup, []}},
  {applications, [mnesia]},
  {env,
   [{port, 31031},
    {delimiter, 0}]}
 ]}.
