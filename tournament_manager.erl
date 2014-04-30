-module (tournament_manager).

-export ([init/4]).

-define (TIMEOUT, 5000).
-define (WAIT, 60000).
-define (DEBUG, true).

%% Players - dictionary of {name, pid} of all currently logged in players. 
%% Part of starting a tournament is picking randomly which players will be part of the tournament and waiting for their accepts/rejects
%% Pid = Pid of the requester. send them a confirmation of the start of tournament
%% YMid = pid of yahtzee manager to give results of tournament to
init(Players, K = _GamesPerMatch, Tid, YMid) ->
  run_tournament(Players, K, Tid, YMid).

run_tournament([{Winner, _}], _K, Tid, YMid) ->
  utils:log("TM: Player ~p won tournament ~p.", [Winner, Tid]),
  YMid ! {tournament_result, Tid, Winner},
  Winner;
run_tournament(Players, K, Tid, YMid) ->
  %% Determine what the proper bracket size is for the number of players
  BracketSize = utils:pow2(utils:ceiling(utils:log2(length(Players)))),
  utils:log("TM: Bracket Size: ~p", [BracketSize]),

  %% Determine the number of byes in this bracket
  NumByes = BracketSize - length(Players),
  Bracket = insert_byes(Players, NumByes),
  utils:log("TM: Bracket = ~p", [Bracket]),
  MatchRefsAndPids = start_matches(Bracket, K, Tid, []),
  Winners = finish_matches(MatchRefsAndPids, YMid, []),
  run_tournament(Winners, K, Tid, YMid).

insert_byes(Players, 0) ->
  Players;
insert_byes(Players, NumByes) ->
  P = utils:insert(NumByes, bye, Players),
  insert_byes(P, NumByes - 1).

%% Inits all the matches for the bracket
%% returns the refs and Pids used to identify each match with
start_matches([], _K, Tid, MatchRefsAndPids) ->
  utils:log("TM: Started all matches for tournament ~p", [Tid]),
  MatchRefsAndPids;
start_matches(Bracket, K, Tid, MatchRefsAndPids) ->
  Ref = make_ref(),
  {[PlayerOne, PlayerTwo], RestOfBracket} = lists:split(2, Bracket),
  utils:log("TM: Starting match between ~p and ~p.", [PlayerOne, PlayerTwo]),
  MPid = spawn(match_manager, init, [PlayerOne, PlayerTwo, K, self(), Tid, Ref]),
  start_matches(RestOfBracket, K, Tid, [{Ref, MPid} | MatchRefsAndPids]).

%% Waits for matches to finish up by collecting the results
%% Also forwards each match result to the tournament manager for stat tracking
%% Also forwards login attempts to each match so they can reset the Pid of their crashed players
%% Returns a list of the winners to be used for the next bracket
finish_matches([], _YMid, Winners) ->
  utils:log("TM: All matches for bracket have finished. Winners: ~p", [Winners]),
  Winners;
finish_matches([{MRef, MPid} | MatchRefsAndPids], YMid, Winners) ->
  receive
    {win, MRef, bye, bye} ->
      utils:log("TM: Received a match result. Both players are byes."),
      finish_matches(MatchRefsAndPids, YMid, [bye | Winners]);
    {win, MRef, {Winner, _}, bye} ->
      utils:log("TM: Received a match result. Player ~p won against a bye.", [Winner]),
      finish_matches(MatchRefsAndPids, YMid, [Winner | Winners]);
    {win, MRef, {Winner, _}, {Loser, _}} ->
      utils:log("TM: Received a match result. Player ~p won against player ~p.", [Winner, Loser]),
      utils:log("TM: Sending match results to tournament manager for stat tracking."),
      %% TODO: decide if security is needed here. Also add this message to YM
      YMid ! {match_result, Winner, Loser},
      finish_matches(MatchRefsAndPids, YMid, [Winner | Winners]);
    {login, UserName, Pid} ->
      notify_matches([{MRef, MPid} | MatchRefsAndPids], UserName, Pid),
      finish_matches([{MRef, MPid} | MatchRefsAndPids], YMid, Winners)
  end.

notify_matches([], UserName, _Pid) ->
  utils:log("TM: All matches have been notified of login of ~p", [UserName]);
notify_matches([{MRef, MPid} | Ms], UserName, Pid) ->
  utils:log("TM: Notifying match ~p of ~p's login.", [MRef, UserName]),
  MPid ! {login, UserName, MPid},
  notify_matches(Ms, UserName, Pid).
