-module(xo_client).
-export([client_loop/1, connect_to_game/1]).

client_loop(Room) ->
	receive
		{room, RoomPid} ->
			client_loop(RoomPid);
		{get_status} ->
			Room ! {status, self()};
		{status, Field} ->
			{Row1, Other} = lists:split(3, Field),
			io:format("~s ~s ~s~n",Row1),
			{Row2, Row3} = lists:split(3, Other),
			io:format("~s ~s ~s~n",Row2),
			io:format("~s ~s ~s~n",Row3),
			client_loop(Room)
	end.

connect_to_game(ServerPid) ->
	Pid = spawn(xo_client, client_loop, [undefined]),
	ServerPid ! {connect_random, Pid}.

get_status(UserPid) ->
	UserPid ! {get_status}.
