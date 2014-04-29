-module (match_manager).

-export ([init/4]).

-import(utils).

%% TODO: Game manager is just a function of match_manager. Separate files only if its huge


%% PlayerX - {name, pid}
%% TMID - tournament manager id
init(PLayerOne, PlayerTwo, K, TMID) ->
  %% run upto K games, tally results and send them back to tournament
  match_done.



match(PlayerOne, PlayerTwo, K, TMID, {P1Score, P2Score}) when P1Score > K/2 ->
	TMID ! {match_done, PlayerOne};

match(PlayerOne, PlayerTwo, K, TMID, {P1Score, P2Score}) when P2Score > K/2 ->
	TMID ! {match_done, PlayerTwo};

match(PlayerOne, PlayerTwo, K, TMID, {P1Score, P2Score}) ->
	
