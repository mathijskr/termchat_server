%%%----------------------------------------------------------------------
%%%
%%%----------------------------------------------------------------------

-module(termchat_sup).
-behaviour(supervisor).
-export([init/1, spawn_listeners/1]).

%%----------------------------------------------------------------------
%% Function:    init/1
%% Description: Starts the mnesia database and opens a tcp socket.
%%              Starts the workers.
%% Args:        No arguments.
%% Returns:
%%----------------------------------------------------------------------
init([]) ->
    {ok, PORT} = application:get_env(port),
    {ok, Listen} = gen_tcp:listen(PORT, [{active, once}, binary]),
    spawn_link(fun() -> spawn_listeners(1) end),
    {ok, {{simple_one_for_one, 60, 3600},
         [{socket,
          {termchat_serv, start_link, [Listen]},
          temporary, 1000, worker, [?MODULE]}
    ]}}.

%%----------------------------------------------------------------------
%% Function:    spawn_listeners/1
%% Description: Spawns N listeners, ready to accept a connection on a socket.
%% Args:        The number of listeners.
%% Returns:     ok.
%% ----------------------------------------------------------------------
spawn_listeners(N) ->
    [supervisor:start_child(?MODULE, []) || _ <- lists:seq(1, N)],
    ok.
