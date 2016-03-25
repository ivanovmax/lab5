-module(xo_server).
-export([main_loop/1, room_loop/1, create_server/0]).

-record(free_room, {name, pid, user}).
-record(room, {name, user1, user2, field = lists:duplicate(9, undefined)}).

main_loop(FreeRooms) ->
	receive
		{connect_random, UserPid} -> 
			case FreeRooms of
				[] ->
					Room = create_room(UserPid),
					UserPid ! { room, Room#free_room.pid },
					main_loop([Room]);
				[Room|OtherRooms] ->
					connect_to_room(Room#free_room.pid, UserPid),
					UserPid ! { room, Room#free_room.pid },
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
		{connect, User} -> 
			room_loop(#room { name = RoomState#room.name, user1 = RoomState#room.user1, user2 = User });
		{turn, {UserPid,Sign}, {Row,Col}} ->
			%Sign = get_sign(UserPid, RoomState#room.user1, RoomState#room.user2);
			room_loop(#room { name = RoomState#room.name, user1 = RoomState#room.user1, user2 = RoomState#room.user2, field = setnth((Row-1)*3 + (Col-1), RoomState#room.field, Sign)});
		{status, UserPid} ->
			UserPid ! {status, RoomState#room.field},
			room_loop(RoomState)	
	end.

create_server() ->
	spawn(xo_server, main_loop, [[]]).

create_room(Name, UserPid) ->
	Pid = spawn(xo_server, room_loop, [ #room { name = Name, user1 = { UserPid, x } } ]),
	#free_room { name = Name, pid = Pid, user = UserPid }.

create_room(UserPid) ->
	create_room(undefined, UserPid).

connect_to_room(RoomPid, UserPid) ->
	RoomPid ! {connect, {UserPid, o}}.

%get_sign(Pid, {Pid, Sign}, User) -> Sign;
%get_sign(Pid, User, {Pid, Sign}) -> Sign.

setnth(1, [_|Tail], NewH) -> [NewH|Tail];
setnth(I, [H|Tail], NewH) -> [H|setnth(I-1, Tail, NewH)].