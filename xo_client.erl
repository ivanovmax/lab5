-module(xo_client).
-export([client_loop/1, connect_to_game/1, get_status/1,turn/2]).

client_loop({Room, Sign}) ->
	receive
		{room, NewRoomState} ->
			client_loop(NewRoomState);
		{get_status} ->
			Room ! {status, self()},
			client_loop({Room, Sign});
		{status, Field} ->
			io:format("~s ~s ~s~n~s ~s ~s~n~s ~s ~s~n",Field),
			client_loop({Room, Sign});
		{turn, Turn} ->
			Room ! {turn, {self(), Sign}, Turn},
			client_loop({Room, Sign});
		{msg, Msg} ->
			io:format("~s~n",[Msg]),
			client_loop({Room, Sign})
	end.

connect_to_game(ServerPid) ->
	Pid = spawn(xo_client, client_loop, [{undefined,undefined}]),
	ServerPid ! {connect_random, Pid},
	Pid.

get_status(UserPid) ->
	UserPid ! {get_status}.

turn(UserPid,Turn) ->
	UserPid ! {turn, Turn}.

