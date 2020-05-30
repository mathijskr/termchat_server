%%%----------------------------------------------------------------------
%%%
%%%----------------------------------------------------------------------

-module(termchat).
-export([install/0, start_link/0, init/1, listen/1, spawn_listeners/1, read_chat/1]).

-define(PORT, 31031).
-define(DELIMITER, "\x00").
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
    spawn_link(fun() -> spawn_listeners(10) end),
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
    read_socket(Socket),
    spawn_link(fun() -> spawn_listeners(1) end).

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
%% Function:    parse_credentials/1
%% Description: Reads the username and password from a bitstring.
%% Args:        A bitstring containing name=<username>?ASCII_DELIMITERpass=<password>.
%% Returns:     A tuple containing username and password and the rest of the bitstring.
%%----------------------------------------------------------------------
parse_credentials(Credentials) ->
    {Name, NameRest} = parse_name(Credentials),
    {Pass, PassRest} = parse_pass(NameRest),
    {Name, Pass, PassRest}.

%%----------------------------------------------------------------------
%% Function:    parse_name/1
%% Description: Reads the username from a bitstring.
%% Args:        A bistring beginning with name=<username>.
%% Returns:     A tuple containing a username and the rest of the bitstring.
%%----------------------------------------------------------------------
parse_name(<<"name=", Bitstring/binary>>) ->
    parse_message(Bitstring, <<>>).

parse_pass(<<"pass=", Bitstring/binary>>) ->
    parse_message(Bitstring, <<>>).

parse_body(<<"body=", Bitstring/binary>>) ->
    parse_message(Bitstring, <<>>).

parse_contact(<<"contact=", Bitstring/binary>>) ->
    parse_message(Bitstring, <<>>).

parse_receiver(<<"receiver=", Bitstring/binary>>) ->
    parse_message(Bitstring, <<>>).

parse_timestamp(<<"timestamp=", Bitstring/binary>>) ->
    parse_message(Bitstring, <<>>).

%%----------------------------------------------------------------------
%% Function:    save_message/1
%% Description: Saves a message between two users in the database.
%% Args:        A bitstring containing the sender's credentials, the receiver's name and
%%              the message.
%% Returns:
%%----------------------------------------------------------------------
save_message(Message) ->
    {Name, Pass, RestCredentials} = parse_credentials(Message),
    {Receiver, RestReceiver}      = parse_receiver(RestCredentials),
    {Body, _}                     = parse_body(RestReceiver),
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
%% Function:    read_chat/1
%% Description:
%% Args:
%% Returns:
%% ----------------------------------------------------------------------
read_chat(Packet) ->
    {Name, Pass, RestCredentials} = parse_credentials(Packet),
    {Contact, RestContact}        = parse_contact(RestCredentials),
    {Timestamp, _}                = parse_timestamp(RestContact),
    case login(Name, Pass) of
        ok        -> Received = database:read_chat(Name, Contact, Timestamp),
                     Sent     = database:read_chat(Contact, Name, Timestamp),
                     Chat     = lists:merge(Received, Sent),
                     SortFun  = fun({_, _, _, T1}, {_, _, _, T2}) ->
                        T1 =< T2
                     end,
                     lists:sort(SortFun, Chat);
        undefined -> {error, invalid_credentials}
    end.

%%----------------------------------------------------------------------
%% Function:
%% Description:
%% Args:
%% Returns:
%% ----------------------------------------------------------------------
read_contacts(Credentials) ->
    {Name, Pass, _} = parse_credentials(Credentials),
    case login(Name, Pass) of
        ok        -> database:get_contacts(Name);
        undefined -> {error, invalid_credentials}
    end.

tcp_format_contacts(Contacts) ->
    Fun = fun(Msg) ->
       <<Msg/binary, "\x00">>
    end,
    Bin = list_to_binary(lists:map(Fun, Contacts)),
    <<Bin/binary, "\n">>.

tcp_format_chat(Chat) ->
    Fun = fun({Receiver, Sender, Body, Timestamp}) ->
        TimestampBin = integer_to_binary(Timestamp),
        <<"timestamp=", TimestampBin/binary, ?ASCII_DELIMITER,
          "receiver=", Receiver/binary, ?ASCII_DELIMITER,
          "sender=", Sender/binary, ?ASCII_DELIMITER,
          "body=", Body/binary, ?ASCII_DELIMITER>>
    end,
    Bin = list_to_binary(lists:map(Fun, Chat)),
    <<Bin/binary, "\n">>.

%%----------------------------------------------------------------------
%% Function:    read_socket/1
%% Description:
%% Args:        The socket to read from.
%% Returns:     ok.
%%----------------------------------------------------------------------
read_socket(Socket) ->
    io:fwrite("receiving"),
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit:", _/binary>>} ->
            gen_tcp:send(Socket, "quit\n"),
            ok;

        {tcp, Socket, <<"send:", Message/binary>>} ->
            case save_message(Message) of
                ok                           -> gen_tcp:send(Socket, "message_sent\n");
                {error, invalid_credentials} -> gen_tcp:send(Socket, "invalid_credentials\n")
            end,
            read_socket(Socket);

        {tcp, Socket, <<"contacts:", Credentials/binary>>} ->
            case read_contacts(Credentials) of
                {error, invalid_credentials} -> gen_tcp:send(Socket, "invalid_credentials\n");
                Contacts                     -> gen_tcp:send(Socket, tcp_format_contacts(Contacts))
            end,
            read_socket(Socket);

        {tcp, Socket, <<"read:", Packet/binary>>} ->
            case read_chat(Packet) of
                {error, invalid_credentials} -> gen_tcp:send(Socket, "invalid_credentials\n");
                Chat                         -> gen_tcp:send(Socket, tcp_format_chat(Chat))
            end,
            read_socket(Socket);

        {tcp, Socket, <<"signup:", Credentials/binary>>} ->
            {Name, Pass, _} = parse_credentials(Credentials),
            case create_account(Name, Pass) of
                ok -> gen_tcp:send(Socket, "account_created\n");
                _  -> gen_tcp:send(Socket, "username_in_use\n")
            end,
            read_socket(Socket)
    end.
