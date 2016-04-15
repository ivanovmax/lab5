-module(xo_server).

-behaviour(gen_server).

-record(free_room, {pid, user}).

-export([create_server/0, uuid/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

create_server() ->
	gen_server:start_link({global, xo_server}, ?MODULE, [], []).

create_room(UserPid) ->
	Start = {xo_room, create, []},
	Restart = transient,
	Shutdown = infinity,
	Type = worker,
	Modules = [xo_room],
	case supervisor:start_child({global, xo_room_sup}, {uuid(), Start, Restart, Shutdown, Type, Modules}) of
		{ok, Child} ->
			{ok, #free_room {pid = Child, user = UserPid } };
		Other -> Other
	end.

connect_to_room(Room, User2) ->
	gen_server:call(Room#free_room.pid, {start, Room#free_room.user, User2}).

init(_Args) ->
	xo_room_sup:start_link(),
    {ok, []}.

handle_call({connect_random, UserPid}, _From, FreeRooms) -> 
	case FreeRooms of
		[] ->
			case create_room(UserPid) of
				{ok, Room} -> 
					Reply = {wait, "Waiting for opponent"},
					io:format("New room created~n",[]),
					{reply, Reply, [Room]};
				{error, _} -> 
					Reply = {error, "Something wrong"},
					io:format("Error on game starting~n",[]),
					{reply, Reply, []}
			end;
		[Room|OtherRooms] ->
			case connect_to_room(Room, UserPid) of
				ok ->
					Reply = {game},
					io:format("Game started~n", []),
					{reply, Reply, OtherRooms};
				_Other ->
					Reply = {error, "Something wrong"},
					io:format("Error on game starting~n", []),
					{reply, Reply, OtherRooms}
			end
	end.

handle_cast(_Message, State) -> { noreply, State }.
handle_info(_Message, State) -> { noreply, State }.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> { ok, State }.

uuid() ->
	uuid:to_string(uuid:v4()).