-module(server).
-export([start/0, install/0, create_account/2]).

-define(PORT, 31031).

install() ->
    user_database:install(node()).

start() ->
    mnesia:start(),
    {ok, Listen} = gen_tcp:listen(?PORT, [{active, false}, binary]),
    listen(Listen).

listen(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    read_socket(Socket),
    gen_tcp:close(Listen).

login(Username, Pass) ->
    case user_database:get_password(Username) =:= Pass of
        true -> ok;
        false -> undefined
    end.

read_name(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"name=", Name/binary>>} ->
            gen_tcp:send(Socket, "ok\n"),
            Name;
        {tcp, Socket, _} ->
            read_name(Socket)
    end.

read_pass(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"pass=", Pass/binary>>} ->
            gen_tcp:send(Socket, "ok\n"),
            Pass;
        {tcp, Socket, _} ->
            read_pass(Socket)
    end.

create_account(Name, Pass) ->
    case user_database:get_password(Name) of
        undefined -> user_database:store(Name, Pass);
        _ -> duplicate
    end.

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
