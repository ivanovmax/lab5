-module(xo_room).
% -export([room_loop/1]).

-behaviour(gen_server).

-export([create/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(room, {user1, user2, field = lists:duplicate(9, "_"), turns = 1, last}).

make_turn(RoomState, {UserPid,Sign}, {Row,Col}) ->
	Pos = (Row-1)*3 + Col,
	case check_turn(RoomState#room.last /= Sign, lists:nth(Pos, RoomState#room.field) == "_") of
		0 -> 
			make_correct_turn(RoomState, UserPid, Sign, Pos);
		1 ->
			UserPid ! {msg, "Not your turn"},
			{continue, RoomState};
		2 ->
			UserPid ! {msg, "Incorrect position"},
			{continue, RoomState}
	end.

make_correct_turn(RoomState, UserPid, Sign, Pos) -> 
	Turns = RoomState#room.turns + 1,
	case Turns =< 9 of
		true ->
			check_winner(RoomState, UserPid, Sign, Pos, Turns);
		false ->
			RoomState#room.user1 ! {disconnect, "Game over"},
			RoomState#room.user2 ! {disconnect, "Game over"},
			{break}
	end.

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

check_winner(RoomState, UserPid, Sign, Pos, Turns) ->
	Field = setnth(Pos, RoomState#room.field, Sign),
	OtherUser = other_user(UserPid, RoomState#room.user1, RoomState#room.user2),
	case win(Field, Sign) of
		true ->
			UserPid ! {disconnect, "You win!"},
			OtherUser ! {disconnect, "You lose..."},
			{break};
		false ->
			OtherUser ! {msg, "Your turn"},
			OtherUser ! {status, Field},
			UserPid ! {status, Field},
			Room = #room { user1 = RoomState#room.user1, user2 = RoomState#room.user2,
				turns = Turns, last = Sign, field = Field},
			{continue, Room}
	end.

other_user(User, User, OtherUser) -> OtherUser;
other_user(User, OtherUser, User) -> OtherUser.

check_turn(true, true) -> 0;
check_turn(false, _) -> 1;
check_turn(true, false) -> 2.

setnth(1, [_|Tail], NewH) -> [NewH|Tail];
setnth(I, [H|Tail], NewH) -> [H|setnth(I-1, Tail, NewH)].


create() ->
	gen_server:start_link(?MODULE, [], []).

init(_Args) ->
	{ok, #room{}}.

handle_call({start, User1, User2}, _From, _) ->
	User1 ! {state, {game, {self(), x}}},
	User2 ! {state, {game, {self(), o}}},
	NewRoomState = #room { user1 = User1, user2 = User2, last = x },
	{reply, ok, NewRoomState};

handle_call({turn, User, {Row,Col}}, _From, RoomState) -> 
	case make_turn(RoomState, User, {Row,Col}) of
		{continue, NewRoomState} ->
			{reply, ok, NewRoomState };
		{break} ->
			{stop, "Room closed", []}
	end;

handle_call(status, _From, RoomState) ->
	{reply, RoomState#room.field, RoomState}.

handle_cast(_Message, State) -> { noreply, State }.
handle_info(_Message, State) -> { noreply, State }.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> { ok, State }.