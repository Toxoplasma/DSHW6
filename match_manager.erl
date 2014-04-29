-module (match_manager).

-export ([init/5]).

-define(SCORECARDSIZE, 14).
-define(TIMEOUT, 10000).

%% TODO: Game manager is just a function of match_manager. Separate files only if its huge


%% PlayerX - {name, pid}
%% TMID - tournament manager id
init(PlayerOne, PlayerTwo, K, TMID, TID) ->
  %% run upto K games, tally results and send them back to tournament
  match(PlayerOne, PlayerTwo, K, TMID, TID, {0, 0}),
  match_done.




match(bye, PlayerTwo, K, TMID, TID, {P1Score, P2Score}) -> TMID ! {win, PlayerTwo, bye};
match(PlayerOne, bye, K, TMID, TID, {P1Score, P2Score}) -> TMID ! {win, PlayerOne, bye};

match(PlayerOne, PlayerTwo, K, TMID, TID, {P1Score, P2Score}) when P1Score > K/2 ->
	TMID ! {match_done, PlayerOne};

match(PlayerOne, PlayerTwo, K, TMID, TID, {P1Score, P2Score}) when P2Score > K/2 ->
	TMID ! {match_done, PlayerTwo};

match(PlayerOne, PlayerTwo, K, TMID, TID, {P1Score, P2Score}) ->
	Winner = game(PlayerOne, PlayerTwo, K, TID, 0),
	if
		Winner == PlayerOne -> match(PlayerOne, PlayerTwo, K, TMID, TID, {P1Score+1, P2Score});
		Winner == PlayerTwo -> match(PlayerOne, PlayerTwo, K, TMID, TID, {P1Score, P2Score+1});
		true -> utils:log("MM: Something went terribly wrong!")
	end.


%Num ties in a row
game({P1Name, P1PID}, {P2Name, P2PID}, K, TID, NumTies) when NumTies < K/2 ->
	GID = make_ref(),
	%Generate the numbers for the game, assuming numTimes < 6
	Dice = utils:rand_seq(6, 15),
	P1Card = [0 || _ <- lists:seq(1,?SCORECARDSIZE)],
	P2Card = [0 || _ <- lists:seq(1,?SCORECARDSIZE)],

	

	hi.
	%P1 round 1

%~13 rounds per game
round({P1Name, P1PID}, TID, GID, Dice, P1Card, P2Card) ->
	case turn({P1Name, P1PID}, TID, GID, 1, Dice, P1Card, P2Card) of


%3 turns per round
turn({P1Name, P1PID}, TID, GID, TurnNum, Dice, P1Card, P2Card) ->
	Play1Ref = make_ref(),
	P1PID ! {play_request, self(), P1Name, {Play1Ref, TID, GID, 1, lists:sublist(Dice, 1, 5), P1Card, P2Card}},
	receive
		{play_action, P1PID, P1Name, {Play1Ref, TID, GID, 1, DiceKept, ScorecardLine}} ->
			{response, {DiceKept, ScorecardLine}},
	after ?TIMEOUT
		{timeout, {P2Name, P2PID}}
	end.
