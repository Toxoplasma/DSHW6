-module (match_manager).

-export ([init/6]).

-define(SCORECARDSIZE, 14).
-define(GAMETIMEOUT, 10000).
-define(WAIT, 60000).

-define(DEBUG, true).

%% TODO: Game manager is just a function of match_manager. Separate files only if its huge


%% PlayerX - {name, pid}
%% TMID - tournament manager id
init(PlayerOne, PlayerTwo, K, TMID, TID, MatchRef) ->
  %% run upto K games, tally results and send them back to tournament
  utils:log("MM: Starting match between ~p and ~p.", [PlayerOne, PlayerTwo]),
  match(PlayerOne, PlayerTwo, K, TMID, TID, MatchRef, {0, 0}),
  utils:log("MM: Match between ~p and ~p has finished.", [PlayerOne, PlayerTwo]),
  match_done.




match(bye, PlayerTwo, _K, TMID, _TID, MatchRef, {_P1Score, _P2Score}) -> 
	utils:log("MM: Match winner is ~p", [PlayerTwo]),
	TMID ! {win, MatchRef, PlayerTwo, bye};
match(PlayerOne, bye, _K, TMID, _TID, MatchRef, {_P1Score, _P2Score}) -> 
	utils:log("MM: Match winner is ~p", [PlayerOne]),
	TMID ! {win, MatchRef, PlayerOne, bye};

match(PlayerOne, PlayerTwo, K, TMID, _TID, MatchRef, {P1Score, _P2Score}) when P1Score > K/2 ->
	utils:log("MM: Match winner is ~p", [PlayerOne]),
	TMID ! {win, MatchRef, PlayerOne, PlayerTwo};

match(PlayerOne, PlayerTwo, K, TMID, _TID, MatchRef, {_P1Score, P2Score}) when P2Score > K/2 ->
	utils:log("MM: Match winner is ~p", [PlayerTwo]),
	TMID ! {win, MatchRef, PlayerTwo, PlayerOne};

match(PlayerOne = {P1Name, _P1PID}, PlayerTwo = {P2Name, _P2PID}, K, TMID, TID, MatchRef, {P1Score, P2Score}) ->
	case game(PlayerOne, PlayerTwo, K, TID, 0) of
		{win, Winner, _Loser} ->
			if
				Winner == PlayerOne -> match(PlayerOne, PlayerTwo, K, TMID, TID, MatchRef, {P1Score+1, P2Score});
				Winner == PlayerTwo -> match(PlayerOne, PlayerTwo, K, TMID, TID, MatchRef, {P1Score, P2Score+1});
				true -> utils:log("MM: Something went terribly wrong! Winner was: ~p", [Winner])
			end;
		{timeout, Winner, Loser = {LoserName, _OldPID}} ->
			%Wait for the guy to come back
			receive
				{login, P1Name, LoserNewPID} when P1Name == LoserName -> match({P1Name, LoserNewPID}, PlayerTwo, K, TMID, TID, MatchRef, {P1Score, P2Score+1});
				{login, P2Name, LoserNewPID} when P2Name == LoserName  -> match(PlayerOne, {P2Name, LoserNewPID}, K, TMID, TID, MatchRef, {P1Score+1, P2Score})
			after ?WAIT ->
				utils:log("MM: Match winner is ~p by timeout", [PlayerTwo]),
				TMID ! {win, MatchRef, Winner, Loser}
			end
	end.



%% NumTies is number of ties in a row
%% Handle the first round in a game, meaning generate new scorecards too
game(P1, P2, K, TID, NumTies) ->
	GID = make_ref(),
	utils:log("MM: Starting game with ID ~p", [GID]),

	%make scorecards
	P1Card = [-1 || _ <- lists:seq(1,?SCORECARDSIZE-1)] ++ [0],
	P2Card = [-1 || _ <- lists:seq(1,?SCORECARDSIZE-1)] ++ [0],

	Winner = set(P1, P2, P1Card, P2Card, K, GID, TID, NumTies, 1),
	%utils:log("MM: (~p) Game finished! Winner: ~p", [GID, Winner]),
	Winner.
	

%% helper function for game
%% ie on the 14th we're done, figure out a winner
set(P1, P2, P1Card, P2Card, K, GID, TID, NumTies, 14) ->
	utils:log("MM: (~p) Game is over, final scorecards are ~n  ~p and ~n  ~p", [GID, P1Card, P2Card]),
	P1Score = cardScore(P1Card),
	P2Score = cardScore(P2Card),
	utils:log("MM: (~p) Final scores are: ~p and ~p", [GID, P1Score, P2Score]),

	if 
		P1Score > P2Score -> 
			utils:log("MM: (~p) Winner is: ~p", [GID, P1]),
			{win, P1, P2};
		P2Score > P1Score -> 
			utils:log("MM: (~p) Winner is: ~p", [GID, P2]),
			{win, P2, P1};
		true -> 
			utils:log("MM: (~p) Game was a tie, restarting with NumTies ~p", [GID, NumTies]),
			game(P1, P2, K, TID, NumTies + 1)
	end;

%% Ordinary set, i.e. not on round 14
set(P1, P2, P1Card, P2Card, K, GID, TID, NumTies, RoundNum) ->
	%% Generate the numbers for the set, assuming numTies < 6
	P1Dice = utils:rand_seq(6, 15),
	if  NumTies < K/2 -> P2Dice = P1Dice;
		true -> P2Dice = utils:rand_seq(6, 15)
	end,

	utils:log("MM: (~p) Dice for round are ~n  ~p and ~n  ~p", [GID, P1Dice, P2Dice]),

	%% Call first guy's round
	case round(P1, TID, GID, lists:sublist(P1Dice, 5), lists:sublist(P1Dice, 6, 10), P1Card, P2Card, 1) of
		timeout -> {timeout, P2};
		cheating -> {win, P2, P1};
		NewP1Card -> %P1 did his stuff for this round, so now we move on to p2
			case round(P2, TID, GID, lists:sublist(P2Dice, 5), lists:sublist(P2Dice, 6, 10), P1Card, P2Card, 1) of
				timeout -> {timeout, P1};
				cheating -> {win, P1, P2};
				NewP2Card -> %P2 did his stuff for this round, so now we recurse with new cards
					set(P1, P2, NewP1Card, NewP2Card, K, GID, TID, NumTies, RoundNum + 1)
			end
	end.


%% 13 rounds per game
round({P1Name, P1PID}, TID, GID, Dice, _RestDice, P1Card, P2Card, 3) ->
	case turn({P1Name, P1PID}, TID, GID, 3, Dice, P1Card, P2Card) of
		%If they shortcut to a slot, which they now must
		{response, {_DiceKept, ScoreSlot}} ->
			case cheating(ScoreSlot, P1Card) of
				true -> cheating;
				false ->
					NewCard = addScoreToCard(Dice, P1Card, ScoreSlot),
					utils:log("MM: (~p) ~p's new scorecard is ~p.", [GID, P1Name, P1Card]),
					NewCard
			end;
		timeout ->
			timeout
	end;
round({P1Name, P1PID}, TID, GID, Dice, RestDice, P1Card, P2Card, TurnNum) ->
	case turn({P1Name, P1PID}, TID, GID, TurnNum, Dice, P1Card, P2Card) of
		%Some dice, still working on it
		{response, {DiceKept, 0}} ->
			KeptDice = [Die || {Die, Keep} <- lists:zip(Dice, DiceKept), Keep == true],
			{NewDice, NewRestDice} = lists:split(5 - length(KeptDice), RestDice),
			round({P1Name, P1PID}, TID, GID, NewDice, NewRestDice, P1Card, P2Card, TurnNum + 1);
		%If they shortcut to a slot
		{response, {_DiceKept, ScoreSlot}} ->
			case cheating(ScoreSlot, P1Card) of
				true -> cheating;
				false ->
					NewCard = addScoreToCard(Dice, P1Card, ScoreSlot),
					NewCard
			end;
		timeout ->
			timeout
	end.

%% 3 turns per round
turn(P1 = {P1Name, P1PID}, TID, GID, TurnNum, Dice, P1Card, P2Card) ->
	Play1Ref = make_ref(),
	utils:log("MM: (~p) Messaging player ~p for action...", [GID, P1]),
	P1PID ! {play_request, self(), P1Name, {Play1Ref, TID, GID, TurnNum, Dice, P1Card, P2Card}},
	receive
		{play_action, P1PID, P1Name, {Play1Ref, TID, GID, TurnNum, DiceKept, ScorecardLine}} ->
			utils:log("MM: (~p) ~p gave us action ~p", [GID, P1Name, {DiceKept, ScorecardLine}]),
			{response, {DiceKept, ScorecardLine}}
	after ?GAMETIMEOUT ->
		timeout
	end.


%% TODO: Make it so it actually scores stuff properly
cheating(Slot, Card) ->
	lists:nth(Slot, Card) =/= -1.

addScoreToCard(Dice, Scorecard, Slot) ->
	utils:replace(Slot, lists:sum(Dice), Scorecard).

%% Scores a card, checking for bonuses. Returns the total sore
cardScore(Scorecard) ->
	{UpperSection, LowerSection} = lists:split(6, lists:sublist(Scorecard, 13)),
	case lists:sum(UpperSection) of
		Sum when Sum >= 63 ->
			UpperScore = 35 + Sum;
		Sum ->
			UpperScore = Sum
	end,
	LowerScore = lists:sum(LowerSection),
	TotalScore = UpperScore + LowerScore + (50 * lists:last(Scorecard)),
	TotalScore.


%% how each different slot is scored
score_small_straight(Dice) ->

score_large_straight(Dice) ->
	Sorted = lists:sort(Dice),
	CheckSeq = lists:seq(hd(Sorted), lists:last(Sorted)),
	case Sorted == CheckSeq of
		true ->
			40;
		false ->
			0
	end.

score_yahtzee(Dice) ->
	case lists:all(fun (Die) -> hd(Dice) == Die end, Dice) of
		true ->
			50;
		false ->
			0
	end.

score_chance(Dice) ->
	lists:sum(Dice).
