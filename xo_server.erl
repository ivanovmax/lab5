-module(xo_server).
-export([main_loop/1, room_loop/1]).

-record(free_room, {name, pid, user}).
-record(room, {name, user1, user2}).

main_loop(FreeRooms) ->
	receive
		{connect_random, UserPid} -> 
			case FreeRooms of
				[] ->
					main_loop([create_room(UserPid)]);
				[Room|OtherRooms] ->
					connect_to_room(Room#free_room.pid, UserPid),
					main_loop(OtherRooms)
			end;
		{connect_direct, RoomName, UserPid} -> 
			case lists:keytake(RoomName, 1, FreeRooms) of
				{value, Room, OtherRooms} -> 
					connect_to_room(Room#free_room.pid, UserPid),
					main_loop(OtherRooms);
				false ->
					main_loop([create_room(RoomName, UserPid)|FreeRooms])
					%io:format("Room '~s' created~n", [RoomName]),
			end
	end.

room_loop(RoomState) ->
	receive
		{connect, NewUserPid} -> 
			room_loop(#room { name = RoomState#room.name, user1 = RoomState#room.user1, user2 = NewUserPid })
	end.

create_room(Name, UserPid) ->
	Pid = spawn(xo_server, room_loop, [ #room { name = Name, user1 = UserPid } ]),
	#free_room { name = Name, pid = Pid, user = UserPid }.

create_room(UserPid) ->
	create_room(undefined, UserPid).

connect_to_room(RoomPid, UserPid) ->
	RoomPid ! {connect, UserPid}.


