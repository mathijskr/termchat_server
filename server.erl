-module(server).
-export([start/0]).

-define(PORT, 31031).

start() ->
    {ok, Listen} = gen_tcp:listen(?PORT, [{active, false}, binary]),
    listen(Listen).

listen(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    read_socket(Socket),
    gen_tcp:close(Listen).

login(<<"joe">>, <<"secret">>) ->
    ok.

read_name(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"name=", Name/binary>>} ->
            gen_tcp:send(Socket, "ok\n"),
            Name
    end.

read_pass(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"pass=", Pass/binary>>} ->
            gen_tcp:send(Socket, "ok\n"),
            Pass
    end.

read_socket(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
            {tcp, Socket, <<"quit", _/binary>>} ->
                io:fwrite("quitted"),
                gen_tcp:close(Socket);

            {tcp, Socket, <<"login", _/binary>>} ->
                io:fwrite("Login"),
                gen_tcp:send(Socket, "ok\n"),
                Name = read_name(Socket),
                Pass = read_pass(Socket),
                ok = login(Name, Pass),
                gen_tcp:send(Socket, "logged in\n"),
                read_socket(Socket);

            {tcp, Socket, _} ->
                io:fwrite("Geen geldig commando"),
                read_socket(Socket)
    end.
