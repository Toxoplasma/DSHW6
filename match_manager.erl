-module (match_manager).

-export ([init/6]).

-define (SCORECARDSIZE, 14).
-define (DEBUG, true).
-define (GAMETIMEOUT, case ?DEBUG of true -> 60000; false -> 10000 end).

-define (WAIT, 60000).

-define (UPPERSECTION, 6).
-define (THREE_OF_A_KIND, 7).
-define (FOUR_OF_A_KIND, 8).
-define (FULL_HOUSE, 9).
-define (SMALL_STRAIGHT, 10).
-define (LARGE_STRAIGHT, 11).
-define (YAHTZEE, 12).
-define (CHANCE, 13).

%% TODO: Game manager is just a function of match_manager. Separate files only if its huge
%TODO: Do a seed

%% PlayerX - {name, pid}
%% TMID - tournament manager id
init(PlayerOne, PlayerTwo, K, TMID, TID, MatchRef) ->
  	%% run upto K games, tally results and send them back to tournament
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
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

match(PlayerOne, PlayerTwo = {P2Name, P2PID}, K, TMID, TID, MatchRef, {P1Score, _P2Score}) when P1Score > K/2 ->
	utils:log("MM: Match winner is ~p", [PlayerOne]),
	P2PID ! {end_tournament, self(), P2Name, TID}, %message the loser that they're done
	TMID ! {win, MatchRef, PlayerOne, PlayerTwo};

match(PlayerOne = {P1Name, P1PID}, PlayerTwo, K, TMID, TID, MatchRef, {_P1Score, P2Score}) when P2Score > K/2 ->
	utils:log("MM: Match winner is ~p", [PlayerTwo]),
	P1PID ! {end_tournament, self(), P1Name, TID}, %message the loser that they're done
	TMID ! {win, MatchRef, PlayerTwo, PlayerOne};

match(PlayerOne = {P1Name, _P1PID}, PlayerTwo = {P2Name, _P2PID}, K, TMID, TID, MatchRef, {P1Score, P2Score}) ->
	case game(PlayerOne, PlayerTwo, K, TID, 0) of
		{win, Winner, _Loser} ->
			if
				Winner == PlayerOne -> match(PlayerOne, PlayerTwo, K, TMID, TID, MatchRef, {P1Score+1, P2Score});
				Winner == PlayerTwo -> match(PlayerOne, PlayerTwo, K, TMID, TID, MatchRef, {P1Score, P2Score+1});
				true -> utils:log("MM: Something went terribly wrong! Winner was: ~p", [Winner])
			end;
		{timeout, bye, bye} ->
			%Wait for both guys to come back
			case waitForBoth(P1Name, P2Name) of
				{timeout, bye, bye} -> 
					utils:log("MM: Match winner is bye because both players timed out..."),
					TMID ! {win, MatchRef, bye, bye};
				{win, Winner, Loser} ->
					utils:log("MM: Match winner is ~p because other guy timed out...", [Winner]),
					TMID ! {win, MatchRef, Winner, Loser};
				{back, NewP1, NewP2} ->
					utils:log("MM: Both players are back!"),
					match(NewP1, NewP2, K, TMID, TID, MatchRef, {P1Score, P2Score})
			end;
		{timeout, Winner, Loser = {LoserName, _OldPID}} ->
			%Wait for the guy to come back
			receive
				{login, P1Name, LoserNewPID} when P1Name == LoserName -> 
					utils:log("MM: ~p has logged back in in time!", [P1Name]),
					match({P1Name, LoserNewPID}, PlayerTwo, K, TMID, TID, MatchRef, {P1Score, P2Score+1});
				{login, P2Name, LoserNewPID} when P2Name == LoserName  -> 
					utils:log("MM: ~p has logged back in in time!", [P2Name]),
					match(PlayerOne, {P2Name, LoserNewPID}, K, TMID, TID, MatchRef, {P1Score+1, P2Score})
			after ?WAIT ->
				utils:log("MM: Match winner is ~p by timeout", [PlayerTwo]),
				TMID ! {win, MatchRef, Winner, Loser}
			end
	end.


waitForBoth(P1Name, P2Name) ->
	utils:log("MM: Waiting for both players to log back in."),
	receive
		{login, P1Name, P1NewPid} -> 
			utils:log("MM: ~p has logged back in!", [P1Name])
	after ?WAIT ->
		P1NewPid = p1failed
	end,

	receive
		{login, P2Name, P2NewPid} ->
			utils:log("MM: ~p has logged back in!", [P2Name])
	after ?WAIT ->
		P2NewPid = p2failed
	end,

	case {P1NewPid, P2NewPid} of
		{p1failed, p2failed} -> {timeout, bye, bye};
		{P1PID, p2failed} -> {win, {P1Name, P1PID}, {P2Name, junk}};
		{p1failed, P2PID} -> {win, {P2Name, P2PID}, {P1Name, junk}};
		{P1PID, P2PID} -> {back, {P1Name, P1PID}, {P2Name, P2PID}}
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
			utils:log("MM: (~p) Game winner is: ~p", [GID, P1]),
			{win, P1, P2};
		P2Score > P1Score -> 
			utils:log("MM: (~p) Game winner is: ~p", [GID, P2]),
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
		timeout -> 
			case round(P2, TID, GID, lists:sublist(P2Dice, 5), lists:sublist(P2Dice, 6, 10), P2Card, P1Card, 1) of
				timeout ->
					utils:log("MM: (~p) Both players timed out! Aborting game.", [GID]),
					{win, bye, bye}; %If both players time out the game is done, bye wins
				_ -> 
					utils:log("MM: (~p) ~p timed out!", [GID, P1]),
					{timeout, P2, P1} %If the other guy is still there then he wins
			end;
		cheating -> 
			utils:log("MM: (~p) ~p is cheating!", [GID, P1]),
			{win, P2, P1};
		NewP1Card -> %%nP1 did his stuff for this round, so now we move on to p2
			case round(P2, TID, GID, lists:sublist(P2Dice, 5), lists:sublist(P2Dice, 6, 10), P2Card, P1Card, 1) of
				timeout -> 
					utils:log("MM: (~p) ~p timed out!", [GID, P2]),
					{timeout, P1, P2};
				cheating -> 
					utils:log("MM: (~p) ~p is cheating!", [GID, P2]),
					{win, P1, P2};
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
				true -> 
					utils:log("MM: (~p) ~p has been caught cheating!", [GID, P1Name]),
					cheating;
				false ->
					utils:log("MM (~p) ~p is adding dice ~p to scorecard slot ~p in card~n  ~p", [GID, P1Name, Dice, ScoreSlot, P1Card]),
					NewCard = addScoreToCard(Dice, P1Card, ScoreSlot),
					utils:log("MM: (~p) ~p's new scorecard is ~p.", [GID, P1Name, NewCard]),
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
			round({P1Name, P1PID}, TID, GID, KeptDice ++ NewDice, NewRestDice, P1Card, P2Card, TurnNum + 1);
		%If they shortcut to a slot
		{response, {_DiceKept, ScoreSlot}} ->
			case cheating(ScoreSlot, P1Card) of
				true -> 
					utils:log("MM: (~p) ~p has been caught cheating!", [GID, P1Name]),
					cheating;
				false ->
					utils:log("MM (~p) ~p is adding dice ~p to scorecard slot ~p in card~n  ~p", [GID, P1Name, Dice, ScoreSlot, P1Card]),
					NewCard = addScoreToCard(Dice, P1Card, ScoreSlot),
					utils:log("MM: (~p) ~p's new scorecard is ~p.", [GID, P1Name, NewCard]),
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


isYahtzee(Dice) ->
	lists:all(fun (Die) -> hd(Dice) == Die end, Dice).

%Assumes sorted dice
isThreeOfAKind([_, _]) -> false;
isThreeOfAKind([A, A, A | _]) -> true;
isThreeOfAKind([_ | Rest]) -> isThreeOfAKind(Rest).
	

scoreUpperSection(Dice, Slot) ->
	GoodDice = [Die || Die <- Dice, Die == Slot],
	lists:sum(GoodDice).



%Assumes sorted dice
isFourOfAKind([A, A, A, A, _]) -> true;
isFourOfAKind([_, A, A, A, A]) -> true;
isFourOfAKind(_) -> false.



isFullHouse([A, A, A, B, B]) -> true;
isFullHouse([B, B, A, A, A]) -> true;
isFullHouse(_) -> false.




hasYahtzee(Scorecard) -> lists:nth(12, Scorecard) == 50.

isYahtzeeJoker(Dice, Scorecard) ->
	isYahtzee(Dice) and hasYahtzee(Scorecard).

%Check for possible yahtzee bonuses then call the helper
addScoreToCard(Dice, Scorecard, Slot) ->
	case isYahtzee(Dice) and hasYahtzee(Scorecard) and (lists:nth(hd(Dice), Scorecard) =/= -1) of
		true ->
			YahtzeeBonuses = lists:nth(14, Scorecard),
			NewCard = utils:replace(14, YahtzeeBonuses + 1, Scorecard),
			addScoreToCardHelper(Dice, NewCard, Slot);
		false -> addScoreToCardHelper(Dice, Scorecard, Slot)
	end.

%Make it actually put in the score correctly
addScoreToCardHelper(Dice, Scorecard, Slot) when Slot =< ?UPPERSECTION ->
	utils:replace(Slot, scoreUpperSection(Dice, Slot), Scorecard);
addScoreToCardHelper(Dice, Scorecard, Slot) ->
	case Slot of
		?THREE_OF_A_KIND -> utils:replace(Slot, score_three_of_a_kind(Dice), Scorecard);
		?FOUR_OF_A_KIND -> utils:replace(Slot, score_four_of_a_kind(Dice), Scorecard);
		?FULL_HOUSE -> utils:replace(Slot, score_full_house(Dice, Scorecard), Scorecard);
		?SMALL_STRAIGHT -> utils:replace(Slot, score_small_straight(Dice, Scorecard), Scorecard);
		?LARGE_STRAIGHT -> utils:replace(Slot, score_large_straight(Dice, Scorecard), Scorecard);
		?YAHTZEE -> utils:replace(Slot, score_yahtzee(Dice), Scorecard);
		?CHANCE -> utils:replace(Slot, score_chance(Dice), Scorecard)
	end.

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
	TotalScore = UpperScore + LowerScore + (100 * lists:last(Scorecard)),
	TotalScore.

%% how each different slot is scored
score_three_of_a_kind(Dice) ->
	SortedDice = lists:sort(Dice),
	case isThreeOfAKind(SortedDice) of
		true -> lists:sum(Dice);
		false -> 0
	end.


score_four_of_a_kind(Dice) ->
	SortedDice = lists:sort(Dice),
	case isFourOfAKind(SortedDice) of
		true -> lists:sum(Dice);
		false -> 0
	end.

score_full_house(Dice, Scorecard) ->
	case isFullHouse(Dice) or isYahtzeeJoker(Dice, Scorecard) of
		true -> 25;
		false -> 0
	end.

score_small_straight(Dice, Scorecard) ->
	Sorted = lists:sort(sets:to_list(sets:from_list(Dice))),
	CheckSeq1 = lists:seq(hd(Sorted), lists:last(Sorted)),
	CheckSeq2 = lists:seq(lists:nth(2, Sorted), lists:last(Sorted)),
	case (CheckSeq1 == Sorted) or (CheckSeq2 == Sorted) or isYahtzeeJoker(Dice, Scorecard) of
		true ->
			30;
		false ->
			0
	end.

score_large_straight(Dice, Scorecard) ->
	Sorted = lists:sort(sets:to_list(sets:from_list(Dice))),
	CheckSeq = lists:seq(hd(Sorted), lists:last(Sorted)),
	case (Sorted == CheckSeq) or isYahtzeeJoker(Dice, Scorecard) of
		true ->
			40;
		false ->
			0
	end.

score_yahtzee(Dice) ->
	case isYahtzee(Dice) of
		true ->
			50;
		false ->
			0
	end.

score_chance(Dice) ->
	lists:sum(Dice).



%TODO: make it so if both players crash then we return a bye
