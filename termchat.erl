%%%----------------------------------------------------------------------
%%%
%%%----------------------------------------------------------------------

-module(termchat).
-export([install/0, start_link/0, init/1, listen/1, spawn_listeners/1]).

-define(PORT, 31031).
-define(ASCII_DELIMITER, 0).

-behaviour(supervisor).

%%----------------------------------------------------------------------
%% Function:    start_link/0
%% Description:
%% Args:
%% Returns:
%% ----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Function:    init/1
%% Description: Starts the mnesia database and opens a tcp socket.
%%              Starts the workers.
%% Args:        No arguments.
%% Returns:
%%----------------------------------------------------------------------
init([]) ->
    mnesia:start(),
    {ok, Listen} = gen_tcp:listen(?PORT, [{active, once}, binary]),
    {ok, {{simple_one_for_one, 60, 3600},
         [{socket,
          {?MODULE, listen, [Listen]},
          temporary, 1000, worker, [?MODULE]}
    ]}}.

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
%% Function:    spawn_listeners/1
%% Description: Spawns N listeners, ready to accept a connection on a socket.
%% Args:        The number of listeners.
%% Returns:     ok.
%% ----------------------------------------------------------------------
spawn_listeners(N) ->
    [supervisor:start_child(?MODULE, []) || _ <- lists:seq(1, N)],
    ok.

%%----------------------------------------------------------------------
%% Function:    listen/1
%% Description: Creates a listener socket, calls the read socket procedure and
%%              closes the socket.
%% Args:        An opened tcp socket.
%% Returns:
%%----------------------------------------------------------------------
listen(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    io:fwrite("JO"),
    gen_tcp:send(Socket, "hallo\n"),
    read_socket(Socket),
    gen_tcp:close(Listen),
    supervisor:start_child(?MODULE, []).

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
%% Function:    parse_message/2
%% Description: Parses the first message in a bitstring delimited by ?ASCII_DELIMITER.
%% Args:        A bitstring delimited by ?ASCII_DELIMITER.
%% Returns:     A tuple containing the first message in the bitstring and
%%              the rest of the bitstring.
%%----------------------------------------------------------------------
parse_message(<<H, T/binary>>, Acc) when H =:= ?ASCII_DELIMITER ->
    {Acc, T};
parse_message(<<H, T/binary>>, Acc) when T =:= <<>> ->
    {<<Acc/binary, H>>, <<>>};
parse_message(<<H, T/binary>>, Acc) ->
    parse_message(T, <<Acc/binary, H>>).

%%----------------------------------------------------------------------
%% Function:    read_credentials/1
%% Description: Reads the username and password from a bitstring.
%% Args:        A bitstring containing name=<username>?ASCII_DELIMITERpass=<password>.
%% Returns:     A tuple containing username and password and the rest of the bitstring.
%%----------------------------------------------------------------------
read_credentials(Credentials) ->
    {Name, NameRest} = read_name(Credentials),
    {Pass, PassRest} = read_pass(NameRest),
    {Name, Pass, PassRest}.

%%----------------------------------------------------------------------
%% Function:    read_name/1
%% Description: Reads the username from a bitstring.
%% Args:        A bistring beginning with name=<username>.
%% Returns:     A tuple containing a username and the rest of the bitstring.
%%----------------------------------------------------------------------
read_name(<<"name=", Bitstring/binary>>) ->
    parse_message(Bitstring, <<>>).

%%----------------------------------------------------------------------
%% Function:    read_pass/1
%% Description: Reads the password from a bitstring.
%% Args:        A bitstring beginning with pass=<password>.
%% Returns:     A tuple containing a password and the rest of the bitstring.
%%----------------------------------------------------------------------
read_pass(<<"pass=", Bitstring/binary>>) ->
    parse_message(Bitstring, <<>>).

%%----------------------------------------------------------------------
%% Function:    read_body/1
%% Description: Reads the body of a message from a bitstring.
%% Args:        A bitstring beginning with body=<body>.
%% Returns:     A tuple containing the body of a message and the rest of the bitstring.
%%----------------------------------------------------------------------
read_body(<<"body=", Bitstring/binary>>) ->
    parse_message(Bitstring, <<>>).

%%----------------------------------------------------------------------
%% Function:    read_receiver/1
%% Description: Reads the receiver of a message from a bitstring.
%% Args:        A bitstring beginning with receiver=<receiver>.
%% Returns:     A tuple containing the receiver of a message and the rest of the bitstring.
%%----------------------------------------------------------------------
read_receiver(<<"receiver=", Bitstring/binary>>) ->
    parse_message(Bitstring, <<>>).

%%----------------------------------------------------------------------
%% Function:    save_message/1
%% Description: Saves a message between two users in the database.
%% Args:        A bitstring containing the sender's credentials, the receiver's name and
%%              the message.
%% Returns:
%%----------------------------------------------------------------------
save_message(Message) ->
    {Name, Pass, RestCredentials} = read_credentials(Message),
    {Receiver, RestReceiver}      = read_receiver(RestCredentials),
    {Body, _}                     = read_body(RestReceiver),
    case login(Name, Pass) of
        ok        -> database:insert_message(Receiver, Name, Body),
                     ok;
        undefined -> {error, invalid_credentials}
    end.

%%----------------------------------------------------------------------
%% Function:    create_account/2
%% Description: Checks if a user exists, if not: inserts a new account for a user
%%              into the database.
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
        {tcp, Socket, <<"test", _/binary>>} ->
            gen_tcp:send(Socket, "jojo\n");
        {tcp, Socket, <<"quit:", _/binary>>} ->
            ok;

        {tcp, Socket, <<"send:", Message/binary>>} ->
            case save_message(Message) of
                ok                           -> gen_tcp:send(Socket, "sent\n");
                {error, invalid_credentials} -> gen_tcp:send(Socket, "invalid_credentials\n")
            end,
            read_socket(Socket);

        {tcp, Socket, <<"signup:", Credentials/binary>>} ->
            {Name, Pass, _} = read_credentials(Credentials),
            case create_account(Name, Pass) of
                ok -> gen_tcp:send(Socket, "account_created\n");
                _  -> gen_tcp:send(Socket, "username_in_use\n")
            end,
            read_socket(Socket);

        {tcp, Socket, _} ->
            read_socket(Socket)
    end.
