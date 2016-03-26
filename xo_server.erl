-module(xo_server).
-export([main_loop/1, create_server/0]).

-record(free_room, {name, pid, user}).
-record(room, {user1, user2, field = lists:duplicate(9, "_"), turns = 1, last}).

main_loop(FreeRooms) ->
	receive
		{connect_random, UserPid} -> 
			case FreeRooms of
				[] ->
					Room = create_room(UserPid),
					UserPid ! { room, {Room#free_room.pid, x} },
					io:format("New room created~n"),
					main_loop([Room]);
				[Room|OtherRooms] ->
					connect_to_room(Room#free_room.pid, UserPid),
					UserPid ! { room, {Room#free_room.pid, o} },
					io:format("Game started~n"),
					main_loop(OtherRooms)
			end;
		{connect_direct, RoomName, UserPid} -> 
			case lists:keytake(RoomName, 1, FreeRooms) of
				{value, Room, OtherRooms} -> 
					connect_to_room(Room#free_room.pid, UserPid),
					UserPid ! { room, {Room#free_room.pid, o} },
					main_loop(OtherRooms);
				false ->
					Room = create_room(RoomName, UserPid),
					UserPid ! { room, {Room#free_room.pid, x} },
					main_loop([Room|FreeRooms])
			end
	end.

create_server() ->
	spawn(xo_server, main_loop, [[]]).

create_room(Name, UserPid) ->
	Pid = spawn(xo_room, room_loop, [ #room { user1 = UserPid } ]),
	#free_room { name = Name, pid = Pid, user = UserPid }.

create_room(UserPid) ->
	create_room(undefined, UserPid).

connect_to_room(RoomPid, UserPid) ->
	RoomPid ! {connect, UserPid}.