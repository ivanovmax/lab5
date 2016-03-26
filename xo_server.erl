-module(xo_server).
-export([main_loop/1, room_loop/1, create_server/0]).

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
					%io:format("Room '~s' created~n", [RoomName]),
			end
	end.

room_loop(RoomState) ->
	receive
		{connect, User} -> 
			room_loop(#room { user1 = RoomState#room.user1, user2 = User, last = x });
		{turn, {UserPid,Sign}, {Row,Col}} when Row >= 1, Row =< 3, Col >= 1, Col =< 3 ->
			Pos = (Row-1)*3 + Col,
			%Row >= 1 and Row =< 3 and Col >= 1 and Col =< 3, 
			case check_turn(RoomState#room.last /= Sign, lists:nth(Pos, RoomState#room.field) == "_") of
				0 -> 
					Turns = RoomState#room.turns + 1,
					case Turns =< 9 of
						true ->
							Field = setnth(Pos, RoomState#room.field, Sign),
							OtherUser = other_user(UserPid, RoomState#room.user1, RoomState#room.user2),
							case win(Field, Sign) of
								true ->
									UserPid ! {msg, "You win!"},
									OtherUser ! {msg, "You lose..."};
								false ->
									OtherUser ! {msg, "Your turn"},
									OtherUser ! {status, Field},
									UserPid ! {status, Field},
									Room = #room { user1 = RoomState#room.user1, user2 = RoomState#room.user2,
										turns = Turns, last = Sign, field = Field},
									%io:format("~s~n",[Room]),
									room_loop(Room)
							end;
						false ->
							broadcast(RoomState#room.user1, RoomState#room.user2, "Game over")
					end;
				1 ->
					UserPid ! {msg, "Not your turn"},
					room_loop(RoomState);
				2 ->
					UserPid ! {msg, "Incorrect position"},
					room_loop(RoomState)
			end;
		{turn, {UserPid,_}, _} ->
			UserPid ! {msg, "Wrong position"},
			room_loop(RoomState);
		{status, UserPid} ->
			UserPid ! {status, RoomState#room.field},
			room_loop(RoomState)	
	end.

create_server() ->
	spawn(xo_server, main_loop, [[]]).

create_room(Name, UserPid) ->
	Pid = spawn(xo_server, room_loop, [ #room { user1 = UserPid } ]),
	#free_room { name = Name, pid = Pid, user = UserPid }.

create_room(UserPid) ->
	create_room(undefined, UserPid).

connect_to_room(RoomPid, UserPid) ->
	RoomPid ! {connect, UserPid}.

win(Field, Sign) ->
	case Field of
		[Sign, Sign, Sign, _, _, _, _, _, _] -> true;
		[_, _, _, Sign, Sign, Sign, _, _, _] -> true;
		[_, _, _, _, _, _, Sign, Sign, Sign] -> true;
		[_, _, Sign, _, Sign, _, Sign, _, _] -> true;
		[Sign, _, _, _, Sign, _, _, _, Sign] -> true;
		[Sign, _, _, Sign, _, _, Sign, _, _] -> true;
		[_, Sign, _, _, Sign, _, _, Sign, _] -> true;
		[_, _, Sign, _, _, Sign, _, _, Sign] -> true;
		_ -> false
	end.

broadcast(User1, User2, Msg) ->
	User1 ! {msg, Msg},
	User2 ! {msg, Msg}.

other_user(User, User, OtherUser) -> OtherUser;
other_user(User, OtherUser, User) -> OtherUser.

%do_turn(CorrectUser, CorrectSign) ->
check_turn(true, true) -> 0;
check_turn(false, _) -> 1;
check_turn(true, false) -> 2.

%get_sign(Pid, {Pid, Sign}, User) -> Sign;
%get_sign(Pid, User, {Pid, Sign}) -> Sign.

setnth(1, [_|Tail], NewH) -> [NewH|Tail];
setnth(I, [H|Tail], NewH) -> [H|setnth(I-1, Tail, NewH)].