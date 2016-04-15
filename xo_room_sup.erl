-module(xo_room_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).

init([]) ->
	MaxRestart = 3,
	MaxTime = 60,
	{ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_link() ->
	supervisor:start_link({global, xo_room_sup}, ?MODULE, []).