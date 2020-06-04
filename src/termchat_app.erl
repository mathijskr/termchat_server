-module(termchat_app).
-behaviour(application).

-export([try_install/0, start/2, stop/1]).

%%----------------------------------------------------------------------
%% Function:    start/2
%% Description:
%% Args:
%% Returns:
%% ----------------------------------------------------------------------
start(normal, _) ->
    {ok, _} = try_install(),
    supervisor:start_link({local, termchat_sup}, termchat_sup, []).

stop(_) ->
    init:stop(),
    ok.

%%----------------------------------------------------------------------
%% Function:    install/1
%% Description: Installs the necessary mnesia database tables.
%% Args:        No arguments.
%% Returns:     ok, or {error, table_exists}
%%----------------------------------------------------------------------
try_install() ->
    case database:install(node()) of
        {atomic, ok}                   -> {ok, db_created};
        {aborted, {already_exists, _}} -> {ok, already_exists}
    end.
