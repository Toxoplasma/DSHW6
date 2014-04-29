-module (match_manager).

-export ([init/5]).


%% TODO: Game manager is just a function of match_manager. Separate files only if its huge


%% PlayerX - {name, pid}
%% TMID - tournament manager id
init(PlayerOne, PlayerTwo, K, TMID, TID) ->
  %% run upto K games, tally results and send them back to tournament
  match(PlayerOne, PlayerTwo, K, TMID, TID, {0, 0}),
  match_done.



match(PlayerOne, PlayerTwo, K, TMID, TID, {P1Score, P2Score}) when P1Score > K/2 ->
	TMID ! {match_done, PlayerOne};

match(PlayerOne, PlayerTwo, K, TMID, TID, {P1Score, P2Score}) when P2Score > K/2 ->
	TMID ! {match_done, PlayerTwo};

match(PlayerOne, PlayerTwo, K, TMID, TID, {P1Score, P2Score}) ->
	Winner = game(PlayerOne, PlayerTwo, K, TID, 0),
	if
		Winner == PlayerOne -> match(PlayerOne, PlayerTwo, K, TMID, TID, {P1Score+1, P2Score});
		Winner == PlayerTwo -> match(PlayerOne, PlayerTwo, K, TMID, TID, {P1Score, P2Score+1});
		true -> utils:log("(MM): Something went terribly wrong!")
	end.


%Num ties in a row
game({P1Name, P1PID}, {P2Name, P2PID}, K, TID, NumTies) when NumTies < K/2 ->
	GID = make_ref(),
	%Generate the numbers for the game, assuming numTimes < 6
	Dice = utils:rand_seq(6, 15),
	P1PID ! {play_request, TID, GID, 1, lists:sublist(Dice, 1, 5), hi},
	hi.
	%P1 round 1

