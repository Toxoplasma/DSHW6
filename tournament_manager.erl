-module (tournament_manager).

-export ([init/4]).

-define (TIMEOUT, 5000).
-define (WAIT, 60000).
-define (DEBUG, true).

%% Players - dictionary of {name, pid} of all currently logged in players. 
%% Part of starting a tournament is picking randomly which players will be part of the tournament and waiting for their accepts/rejects
%% Pid = Pid of the requester. send them a confirmation of the start of tournament
%% YMid = pid of yahtzee manager to give results of tournament to
init(Players, N = _NumberOfPlayers, K = _GamesPerMatch, YMid) ->
  %% TODO: determine if N or 2^N
  %% matches and stuff
  Size = trunc(utils:log2(K)) + 1,
  run_tournament(Players, Size, K, YMid).


run_tournament(TournamentPlayers, Size, K, YMid) ->
  %% TODO: actually run the tournament, set up brackets, start matches, etc.
  finish_me.
