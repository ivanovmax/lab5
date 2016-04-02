-module(xo_client).
-export([client_loop/1, create/0, get_status/1,turn/2]).

client_loop({Room, Sign}) ->
	receive
		{room, NewRoomState} ->
			client_loop(NewRoomState);
		{status} ->
			io:format("~s ~s ~s~n~s ~s ~s~n~s ~s ~s~n", gen_server:call(Room, status)),
			client_loop({Room, Sign});
		{turn, Turn} ->
			gen_server:call(Room, {turn, {self(), Sign}, Turn}),
			client_loop({Room, Sign});
		{msg, Msg} ->
			io:format("~s~n",[Msg]),
			client_loop({Room, Sign});
		{disconnect, Msg} ->
			io:format("~s~nYou disconnected~n", [Msg])
	end.

% connect_to_game(ServerPid) ->
% 	Pid = spawn(xo_client, client_loop, [{undefined,undefined}]),
% 	%ServerPid ! {connect_random, Pid},
% 	Pid.

create() ->
	spawn(xo_client, client_loop, [{undefined,undefined}]).

get_status(UserPid) ->
	UserPid ! {status}.

turn(UserPid,Turn) ->
	UserPid ! {turn, Turn}.

