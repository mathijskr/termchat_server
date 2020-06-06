-module(termchat_serv).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%----------------------------------------------------------------------
%% Function:    start_link/1
%% Description: Creates a listener socket, calls the read socket procedure and
%%              spawns a new worker when done.
%% Args:        An open tcp socket.
%% Returns:
%%----------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, Socket}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(accept, ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    termchat_sup:spawn_listeners(1),
    {noreply, AcceptSocket}.

handle_info({tcp, Socket, <<"quit:", _/binary>>}, Socket) ->
    inet:setopts(Socket, [{active, once}]),
    gen_tcp:send(Socket, "quit\n"),
    {stop, normal, Socket};

handle_info({tcp, Socket, <<"send:", Message/binary>>}, Socket) ->
    inet:setopts(Socket, [{active, once}]),
    case save_message(Message) of
        ok                           -> gen_tcp:send(Socket, "message_sent\n");
        {error, invalid_credentials} -> gen_tcp:send(Socket, "invalid_credentials\n")
    end,
    {noreply, Socket};

handle_info({tcp, Socket, <<"contacts:", Credentials/binary>>}, Socket) ->
    inet:setopts(Socket, [{active, once}]),
    case read_contacts(Credentials) of
        {error, invalid_credentials} -> gen_tcp:send(Socket, "invalid_credentials\n");
        Contacts                     -> gen_tcp:send(Socket, tcp_format_contacts(Contacts))
    end,
    {noreply, Socket};

handle_info({tcp, Socket, <<"read:", Packet/binary>>}, Socket) ->
    inet:setopts(Socket, [{active, once}]),
    case read_chat(Packet) of
        {error, invalid_credentials} -> gen_tcp:send(Socket, "invalid_credentials\n");
        Chat                         -> gen_tcp:send(Socket, tcp_format_chat(Chat))
    end,
    {noreply, Socket};

handle_info({tcp, Socket, <<"signup:", Credentials/binary>>}, Socket) ->
    inet:setopts(Socket, [{active, once}]),
    {Name, Pass, _} = parse_credentials(Credentials),
    case create_account(Name, Pass) of
        ok -> gen_tcp:send(Socket, "account_created\n");
        _  -> gen_tcp:send(Socket, "username_in_use\n")
    end,
    {noreply, Socket}.

%%----------------------------------------------------------------------
%% Function:    login/2
%% Description: Checks if the credentials of a user are correct.
%% Args:        The username and password of a user.
%% Returns:     ok, or undefined
%%----------------------------------------------------------------------
login(Username, Pass) ->
    OriginalHash = database:get_password(Username),
    {ok, CheckHash} = bcrypt:hashpw(Pass, OriginalHash),
    case CheckHash =:= OriginalHash of
        true  -> ok;
        false -> undefined
    end.

%%----------------------------------------------------------------------
%% Function:    parse_message/2
%% Description: Parses the first message in a bitstring delimited by delimiter.
%% Args:        A bitstring delimited by delimiter.
%% Returns:     A tuple containing the first message in the bitstring and
%%              the rest of the bitstring.
%%----------------------------------------------------------------------
parse_message(<<H, T/binary>>, Acc) when T =:= <<>> ->
    {<<Acc/binary, H>>, <<>>};
parse_message(<<H, T/binary>>, Acc) ->
    {ok, DELIM} = application:get_env(delimiter),
    case H =:= DELIM of
        true  -> {Acc, T};
        false -> parse_message(T, <<Acc/binary, H>>)
    end.

%%----------------------------------------------------------------------
%% Function:    parse_credentials/1
%% Description: Reads the username and password from a bitstring.
%% Args:        A bitstring containing name=<username>delimiterpass=<password>.
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
        {error, unknown_user} ->
            {ok, Workfactor} = application:get_env(salt_workfactor),
            {ok, Salt} = bcrypt:gen_salt(Workfactor),
            {ok, Hash} = bcrypt:hashpw(Pass, Salt),
            database:insert_user(Name, Hash);
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
        {ok, DELIM} = application:get_env(delimiter),
        <<"timestamp=", TimestampBin/binary, DELIM,
          "receiver=", Receiver/binary, DELIM,
          "sender=", Sender/binary, DELIM,
          "body=", Body/binary, DELIM>>
    end,
    Bin = list_to_binary(lists:map(Fun, Chat)),
    <<Bin/binary, "\n">>.
