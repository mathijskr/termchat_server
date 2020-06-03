-module(termchat).
-behaviour(application).

-export([install/0, start/2, stop/1]).

%%----------------------------------------------------------------------
%% Function:    start/2
%% Description:
%% Args:
%% Returns:
%% ----------------------------------------------------------------------
start(normal, _) ->
    supervisor:start_link({local, termchat_sup}, termchat_sup, []).

stop(_) ->
    ok.

%%----------------------------------------------------------------------
%% Function:    install/1
%% Description: Installs the necessary mnesia database tables.
%% Args:        No arguments.
%% Returns:     ok, or {error, table_exists}
%%----------------------------------------------------------------------
install() ->
    case database:install(node()) of
        {atomic, ok}                   -> ok;
        {aborted, {already_exists, _}} -> {error, table_exists}
    end.
