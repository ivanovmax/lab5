-module(xo_client).
-export([client_loop/1, create/0, get_status/1,turn/2]).

client_loop(RoomState) ->
	receive
		status ->
			case RoomState of
				{game, {Room, _}} -> 
					io:format("~s ~s ~s~n~s ~s ~s~n~s ~s ~s~n", gen_server:call(Room, status)),
					client_loop(RoomState);
				{wait, Msg} ->
					io:format(Msg,[]),
					client_loop({wait, Msg});
				{error, Msg} ->
					io:format(Msg,[]),
					client_loop({wait, Msg})
			end;
		{state, State} ->
			case State of
				{game, _} -> 
					io:format("Game started~n",[]),
					client_loop(State);
				{wait, Msg} ->
					io:format(Msg,[]),
					client_loop(State);
				{error, Msg} ->
					io:format(Msg,[]),
					client_loop(State)
			end;
		{turn, Turn} ->
			case RoomState of
				{game, {Room, Sign}} -> 
					gen_server:call(Room, {turn, {self(), Sign}, Turn}),
					client_loop(RoomState);
				{wait, Msg} ->
					io:format(Msg,[]),
					client_loop(RoomState);
				{error, Msg} ->
					io:format(Msg,[]),
					client_loop(RoomState)
			end;
		{msg, Msg} ->
			io:format("~s~n",[Msg]),
			client_loop(RoomState);
		{disconnect, Msg} ->
			io:format("~s~nYou disconnected~n", [Msg])
	end.

create() ->
	Pid = spawn(xo_client, client_loop, [{undefined,undefined}]),
	case gen_server:call({global, xo_server}, {connect_random, Pid}) of
		{wait, Msg} -> 
			Pid ! {state, {wait, Msg}},
			Pid;
		{error, Msg} -> 
			Pid ! {state, {error, Msg}},
			Pid;
		{game} ->
			Pid
	end.

get_status(UserPid) ->
	UserPid ! status.

turn(UserPid,Turn) ->
	UserPid ! {turn, Turn}.

