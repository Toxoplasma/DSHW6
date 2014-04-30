%% Player program!
%% By Ravi Kumar and Alejandro Frias

-module(yahtzee_player1).
-export([main/1]).

-define(LOGINPID, 1).
-define(LOGINTICKET, 2).

main([NetName, Username, Password | YMNames]) ->
	%Boring erlang net stuff
	_ = os:cmd("epmd -daemon"),
    net_kernel:start([list_to_atom(NetName), shortnames]),
    %register(philosopher, self()),

	%Register with all managers
	Tickets = send_register_messages(YMNames, Username, Password, []),

	%Now just chill and listen
	plog("I am: {~p, ~p}.", [Username, self()], Username),
	plog("Successfully logged in, waiting for messages.", Username),
	listen(Username, Tickets),
	plog("Uh oh! We shouldn't really be here.", Username),
	hurray.

send_register_messages([], _Username, _Password, Tickets) ->
	Tickets;

send_register_messages([Name | YMNames], Username, Password, Tickets) ->
	%Send the manager a login message
	plog("Logging in with manager ~p", [Name], Username),
	Pid = {yahtzee_manager, list_to_atom(Name)},%whereis_name(Name),
	Pid ! {login, self(), Username, {Username, Password}},

	%receive the ticket from the login manager
	%When we get it, go on to the next guy
	receive
		{logged_in, LogoutPID, Username, LoginTicket} ->
			plog("Got login ticket from manager ~p", [Name], Username),
			send_register_messages(YMNames, Username, Password, [{LogoutPID, LoginTicket} | Tickets])
	end.

%Currently there is no system for logging out, so we don't actually need tickets
%We also don't keep track of what tournaments we're in
listen(Username, Tickets) ->
	receive
		%Start tournament message
		{start_tournament, ReplyPID, Username, TID} ->
			plog("Got message to start tournament with ID ~p", [TID], Username),
			%Find the ticket we logged in with to this YM and reply with that ticket
			{ReplyPID, LoginTicket} = lists:keyfind(ReplyPID, ?LOGINPID, Tickets),
			plog("Replying with acceptance.", Username),
			ReplyPID ! {accept_tournament, self(), Username, {TID, LoginTicket}};

		%End tournament message
		{end_tournament, _ReplyPID, _Username, TID} -> 
			plog("Received end tournament message with ID ~p", [TID], Username),
			we_dont_care;

		%Play request message! What we actually care about
		{play_request, ReplyPID, Username, GameState} ->
			plog("Received player request with game state: ~n  ~p", [GameState], Username),
			Action = computeAction(GameState),
			ReplyPID ! {play_action, self(), Username, Action}
	end,
	listen(Username, Tickets).


%Current action computation: Assign it to whatever first slot is free, no rerolls
computeAction({Ref, Tid, Gid, RollNumber, _Dice, Scorecard, _OppScorecard}) ->
	%Find first empty score
	EmptyScores = findEmptyScores(Scorecard),
	case EmptyScores of
		[] -> utils:log("Someone sent us a full scorecard! Morons...");
		_ ->
			{Ref, Tid, Gid, RollNumber, [true, true, true, true, true], hd(EmptyScores)}
	end.
	

%Compute action related helpers
findEmptyScores(Scorecard) -> findEmptyScores(Scorecard, [], 1).

findEmptyScores([], Empty, _Index) -> lists:reverse(Empty);
findEmptyScores([-1 | Rest], EmptySoFar, Index) ->
	findEmptyScores(Rest, [Index | EmptySoFar], Index + 1);
findEmptyScores([_ | Rest], EmptySoFar, Index) ->
	findEmptyScores(Rest, EmptySoFar, Index).

	
	


%Player log message. Essentially just prints the name as well
plog(Message, Name) ->
	utils:timestamp(),
 	io:format("~p: ~s~n", [Name, Message]).
plog(Message, Format, Name) ->
	S = io_lib:format(Message, Format),
	plog(S, Name).
