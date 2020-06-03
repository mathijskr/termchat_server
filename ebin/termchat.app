{application, termchat,
 [{vsn, "1.0.0"},
  {modules, [termchat, termchat_sup, termchat_serv, database]},
  {registered, [termchat]},
  {mod, {termchat, []}},
  {applications, [mnesia]},
  {env,
   [{port, 31031},
    {delimiter, 0},
    {nworkers, 10}
   ]}
 ]}.
