-module(server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, add/1]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, []}.

handle_call(Msg, _From, State) ->
    NewState = State ++ [Msg],
    {reply, NewState, NewState}.
handle_cast(Msg, State) ->
    NewState = State ++ [Msg],
    {noreply, NewState}.

add(Msg) ->
    gen_server:call(?MODULE, Msg).
