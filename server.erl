%%%----------------------------------------------------------------------
%%%
%%%----------------------------------------------------------------------

-module(server).
-export([start/0, install/0, create_account/2]).

-define(PORT, 31031).

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

%%----------------------------------------------------------------------
%% Function:    start/1
%% Description: Starts the mnesia database and opens a tcp socket.
%% Args:        No arguments.
%% Returns:     Nothing.
%%
start() ->
    mnesia:start(),
    {ok, Listen} = gen_tcp:listen(?PORT, [{active, false}, binary]),
    listen(Listen).

%%----------------------------------------------------------------------
%% Function:    listen/1
%% Description: Creates a listener socket, calls the read socket procedure and closes the socket.
%% Args:        An opened tcp socket.
%% Returns:
%%----------------------------------------------------------------------
listen(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    read_socket(Socket),
    gen_tcp:close(Listen).

%%----------------------------------------------------------------------
%% Function:    login/2
%% Description: Checks if the credentials of a user are correct.
%% Args:        The username and password of a user.
%% Returns:     ok, or undefined
%%----------------------------------------------------------------------
login(Username, Pass) ->
    case database:get_password(Username) =:= Pass of
        true  -> ok;
        false -> undefined
    end.

%%----------------------------------------------------------------------
%% Function:    read_name/1
%% Description: Waits for the socket to receive name=<username>, replies with ok.
%% Args:        The socket to read from.
%% Returns:     The read username.
%%----------------------------------------------------------------------
read_name(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"name=", Name/binary>>} ->
            gen_tcp:send(Socket, "ok\n"),
            Name;
        {tcp, Socket, _} ->
            read_name(Socket)
    end.

%%----------------------------------------------------------------------
%% Function:    read_pass/1
%% Description: Waits for the socket to receive pass=<password>, replies with ok.
%% Args:        The socket to read from.
%% Returns:     The read password.
%%----------------------------------------------------------------------
read_pass(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"pass=", Pass/binary>>} ->
            gen_tcp:send(Socket, "ok\n"),
            Pass;
        {tcp, Socket, _} ->
            read_pass(Socket)
    end.

%%----------------------------------------------------------------------
%% Function:    create_account/2
%% Description: Checks if a user exists, if not: inserts a new account for a user into the database.
%% Args:        The username and password of the user.
%% Returns:     ok, or {error, user_exists}
%%----------------------------------------------------------------------
create_account(Name, Pass) ->
    case database:get_password(Name) of
        {error, unknown_user} -> database:insert_user(Name, Pass);
        _                     -> {error, user_exists}
    end.

%%----------------------------------------------------------------------
%% Function:    read_socket/1
%% Description: Reads commands: quit, login or signup from a socket.
%% Args:        The socket to read from.
%% Returns:     ok.
%%----------------------------------------------------------------------
read_socket(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            gen_tcp:close(Socket);

        {tcp, Socket, <<"login", _/binary>>} ->
            gen_tcp:send(Socket, "ok\n"),
            Name = read_name(Socket),
            Pass = read_pass(Socket),
            case login(Name, Pass) of
                ok -> gen_tcp:send(Socket, "logged in\n");
                _ -> gen_tcp:send(Socket, "bad credentials\n")
            end,
            read_socket(Socket);

        {tcp, Socket, <<"signup", _/binary>>} ->
            gen_tcp:send(Socket, "ok\n"),
            Name = read_name(Socket),
            Pass = read_pass(Socket),
            case create_account(Name, Pass) of
                ok -> gen_tcp:send(Socket, "signed up\n");
                _ -> gen_tcp:send(Socket, "username in use\n")
            end,
            read_socket(Socket);

        {tcp, Socket, _} ->
            read_socket(Socket)
    end.
