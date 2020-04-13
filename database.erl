%%%----------------------------------------------------------------------
%%% Database module. Stores user credentials and messages sent between users.
%%% Defines functions for inserting and retrieving information from the database.
%%%----------------------------------------------------------------------

-module(database).
-export([install/1, insert_user/2, insert_message/3, get_password/1, get_contacts/1, read_chat/2]).
-include_lib("stdlib/include/qlc.hrl").

-record(termchat_user, {username, password}).
-record(termchat_message, {receiver, sender, body, timestamp}).

%%----------------------------------------------------------------------
%% Function:    install/1
%% Description: Install the mnesia schema for the current node and
%%              create tables for the user credentials and the messages.
%% Args:        Node is the target node.
%% Returns:     ok, or {aborted, {already_exists, <table name>}}
%%----------------------------------------------------------------------
install(Node) ->
    mnesia:create_schema([Node]),
    mnesia:start(),
    mnesia:create_table(termchat_user,
    [{attributes, record_info(fields, termchat_user)},
     {disc_copies,[Node]},
     {type, set}]),
    mnesia:create_table(termchat_message,
    [{attributes, record_info(fields, termchat_message)},
     {disc_copies,[Node]},
     {type, bag}]).

%%----------------------------------------------------------------------
%% Function:    insert_user/1
%% Description: Adds a user to the database. The user gets overwritten if he already exists.
%% Args:        Username and password of the user.
%% Returns:     ok.
%%----------------------------------------------------------------------
insert_user(Username, Password) ->
    Store = fun() ->
        mnesia:write(#termchat_user{username=Username, password=Password})
    end,
    {atomic, Status} = mnesia:transaction(Store),
    Status.

%%----------------------------------------------------------------------
%% Function:    insert_message/3
%% Description: Adds a message from one user to another to the database.
%% Args:        The receiver and sender of the message, and the body of the message.
%% Returns:     ok, or {error, unknown_user}.
%%----------------------------------------------------------------------
insert_message(Receiver, Sender, Body) ->
    ReceiverExists = case get_password(Receiver) of
        undefined -> false;
        _         -> true
    end,
    SenderExists = case get_password(Sender) of
        undefined -> false;
        _         -> true
    end,
    Store = fun() ->
        mnesia:write(
          #termchat_message{
             receiver=Receiver,
             sender=Sender, body=Body,
             timestamp=erlang:system_time()
          }
        )
    end,
    case ReceiverExists and SenderExists of
        true  -> {atomic, Status} = mnesia:transaction(Store),
                 Status;
        false -> {error, unknown_user}
    end.

%%----------------------------------------------------------------------
%% Function:    get_password/1
%% Description: Read the password of a user.
%% Args:        The username of a user.
%% Returns:     The password of a user, or {error, unknown_user}.
%%----------------------------------------------------------------------
get_password(Username) ->
    Read = fun() ->
            case mnesia:read({termchat_user, Username}) of
                [#termchat_user{password=Password}] ->
                    Password;
                [] ->
                    undefined
            end
    end,
    case mnesia:transaction(Read) of
        {atomic, undefined} -> {error, unknown_user};
        {atomic, Password} -> Password
    end.

%%----------------------------------------------------------------------
%% Function:    get_contacts/1
%% Description: Read to who a user has send messages and from who a user has received messages.
%% Args:        The username of a user.
%% Returns:     A list of usernames.
%% ----------------------------------------------------------------------
get_contacts(Username) ->
    Receivers = get_sended_to(Username),
    Senders = get_received_from(Username),
    lists:append(Receivers, Senders).

%%----------------------------------------------------------------------
%% Function:    get_received_from/1
%% Description: Read from who a user has received messages.
%% Args:        The username of a users.
%% Returns:     A list of usernames.
%% ----------------------------------------------------------------------
get_received_from(Username) ->
    ReadSenders = fun() ->
        qlc:eval(qlc:q(
            [Receiver || #termchat_message{
                          receiver=Receiver,
                          sender=Sender,
                          body='_',
                          timestamp='_'
                        } <- mnesia:table(termchat_message),
                Sender =:= Username
            ]))
    end,
    mnesia:activity(transaction, ReadSenders).

%%----------------------------------------------------------------------
%% Function:    get_sended_to/1
%% Description: Read to who a user has sended messages.
%% Args:        The username of a user.
%% Returns:     A list of usernames.
%% ----------------------------------------------------------------------
get_sended_to(Username) ->
    ReadSenders = fun() ->
        qlc:eval(qlc:q(
            [Sender || #termchat_message{
                          receiver=Receiver,
                          sender=Sender,
                          body='_',
                          timestamp='_'
                        } <- mnesia:table(termchat_message),
                Receiver =:= Username
            ]))
    end,
    mnesia:activity(transaction, ReadSenders).

%%----------------------------------------------------------------------
%% Function:    read_chat/1
%% Description: Read the messages sent between two users.
%% Args:        The sender and receiver.
%% Returns:     All messages sent by sender and received by receiver, sorted by timestamp.
%%----------------------------------------------------------------------
read_chat(Receiver, Sender) ->
    Read = fun() ->
        mnesia:match_object(
          #termchat_message{
             receiver=binary_to_list(Receiver),
             sender=binary_to_list(Sender),
             body='_',
             timestamp='_'
          }
        )
    end,
    {atomic, Inbox} = mnesia:transaction(Read),
    SortFun = fun({termchat_message, _, _, _, T1}, {termchat_message, _, _, _, T2}) ->
        T1 =< T2
    end,
    lists:sort(SortFun, Inbox).
