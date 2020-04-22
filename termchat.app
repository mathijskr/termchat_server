{application, termchat,
 [{vsn, "0.1.0"},
  {description, "Server backend for the termchat program"},
  {modules, [termchat, database]},
  {applications, [stdlib, kernel]}
 ]}.
