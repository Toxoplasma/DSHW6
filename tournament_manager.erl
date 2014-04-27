-module (tournament_manager).

-export ([init/5]).

-define (TIMEOUT, 5000).
-define (WAIT, 60000).
-define (DEBUG, true).

%% Players - dictionary of {name, pid} of all currently logged in players. 
%% Part of starting a tournament is picking randomly which players will be part of the tournament and waiting for their accepts/rejects
%% Pid = Pid of the requester. send them a confirmation of the start of tournament
%% YMid = pid of yahtzee manager to give results of tournament to
init(Players, N = _NumberOfPlayers, K = _GamesPerMatch, Pid, YMid) ->
  %% TODO: determine if N or 2^N
  %% matches and stuff

  %% Register up to N players in the tournament (randomly selected)
  {RandomSelection, Extra} = utils:rand_split(N, dict:to_list(Players)),

  send_start_tournament(RandomSelection),
  TournamentPlayers = receive_accept_tournament(RandomSelection, Extra, []),
  Pid ! {tournament_started, YMid, {self(), [P || {P, _LoginTicket} <- TournamentPlayers], no_value}},
  run_tournament(TournamentPlayers, K, Pid, YMid).

%% Sends a start_tournament message to each player
send_start_tournament([]) ->
  done;
send_start_tournament([{UserName, {Pid, _LoginTicket}} | Ps]) ->
  utils:log("TM: Sending start_tournament message to ~p", [UserName]),
  Pid ! {start_tournament, self(), self()},
  send_start_tournament(Ps).

%% Assemble the player list
receive_accept_tournament([], [], AcceptedPlayers) ->
  utils:log("TM: Player list has been generated. There are ~p players.", [length(AcceptedPlayers)]),
  utils:dlog("TM: Player list and pids: ~p", [AcceptedPlayers], ?DEBUG),
  AcceptedPlayers;
receive_accept_tournament([{UserName, {_OldPid, LoginTicket}} | Players], Extra, AcceptedPlayers) ->
  Tid = self(),
  receive
    {accept_tournament, Pid, UserName, {Tid, LoginTicket}} ->
      utils:log("TM: Received accept_tournament message from ~p.", [UserName]),
      %% TODO: monitor them
      receive_accept_tournament(Players, Extra, [{UserName, Pid} | AcceptedPlayers]);
    {reject_tournament, _Pid, UserName, {Tid, LoginTicket}} ->
      utils:log("TM: Received reject_tournament message from ~p", [UserName]),
      case Extra of
        [E | Es] ->
          send_start_tournament([E]),
          receive_accept_tournament([E | Players], Es, AcceptedPlayers);
        [] ->
          utils:log("No extra players to send a start_tournament message."),
          receive_accept_tournament(Players, [], AcceptedPlayers)
      end
  after ?TIMEOUT ->
    utils:log("TM: Timed out waiting for player ~p.", [UserName]),
    case Extra of
      [E | Es] ->
        send_start_tournament([E]),
        receive_accept_tournament([E | Players], Es, AcceptedPlayers);
      [] ->
        utils:log("No extra players to send a start_tournament message."),
        receive_accept_tournament(Players, [], AcceptedPlayers)
    end
  end.

run_tournament(TournamentPlayers, K, Pid, YMid) ->
  %% TODO: actually run the tournament, set up brackets, start matches, etc.
  finish_me.
