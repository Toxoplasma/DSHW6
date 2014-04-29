%%% -------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Alejandro Frias, Ravi Kumar
%%%
%%% The Main Manager where registration occurs and new tournaments are started
%%%--------------------------------------------------------------------

-module (yahtzee_manager).

-export ([main/1]).

-define (DEBUG, true).
-define (TIMEOUT, 5000).

main([Name]) ->
  init(Name);
main([Name, Seed]) ->
  init(Name, Seed);
main(_Other) ->
  utils:log("YM: Incorrect use. Usage: erl -noshell -run main <name> [<seed>]").

%% Start kernal, cmd daemon, register name
init(Name) ->
    Seed = now(),
    init(Name, Seed).

%% If you desire to init with a given seed. Good for reproducing tests
init(Name, Seed = {A1, A2, A3}) ->
    _ = os:cmd("epmd -daemon"),
    utils:dlog("YM: Seeding random with ~p", [Seed], ?DEBUG),
    random:seed(A1, A2, A3),
    net_kernel:start([list_to_atom(Name), shortnames]),
    register(yahtzee_manager, self()),
    utils:log("YM: Yahtzee Manager registered as {~p, ~w}", [yahtzee_manager, node()]),
    listen(dict:new(), dict:new(), []).

%% Listen for messages
%% R = a dictionary of usernames to {password, list-of-tourn-stats}
%% C = a dictionary of usernames to {pid, Ref}
listen(R = _RegisteredPlayersAndStats, C = _CurrentlyLoggedIn, O = _OngoingTournaments) ->
  utils:dlog("YM: All current users: ~p", [dict:to_list(C)], ?DEBUG),
  receive
    {login, Pid, UserName, PassWord} ->
      utils:log("YM: Received login message from ~p", [UserName]),
      case dict:is_key(UserName, R) of
        true -> 
          case dict:fetch(UserName, R) of
            {PassWord, _Stats} ->
              utils:log("YM: Sending logged_in message to returning player ~p", [UserName]),
              Ref = make_ref(),
              Pid ! {logged_in, self(), UserName, Ref},
              notify_tournaments(UserName, Pid, O),
              listen(R, dict:store(UserName, {Pid, Ref}, C), O);
            {_WrongPassword, _Stats} ->
              utils:log("YM: Incorrect password given for player ~p", [UserName]),
              listen(R, C, O)
          end;
        false ->
          utils:log("YM: Sending logged_in message to new player ~p", [UserName]),
          Ref = make_ref(),
          Pid ! {logged_in, self(), UserName, Ref},
          listen(dict:store(UserName, {PassWord, []}, R), dict:store(UserName, {Pid, Ref}, C), O)
      end;
    {logout, _Pid, UserName, Ref} ->
      utils:log("YM: Received logout message from ~p", [UserName]),
      case dict:is_key(UserName, C) of
        true ->
          case dict:fetch(UserName, C) of
            {_, Ref} ->
              utils:log("YM: Logging player ~p out safely.", [UserName]),
              notify_tournaments(UserName, no_value, O),
              listen(R, dict:erase(UserName, C), O);
            {_, _WrongRef} ->
              utils:log("YM: Invalid logout attempt by player ~p", [UserName])
          end;
        false ->
          utils:log("YM: Player ~p is not currently logged in.", [UserName])
      end;
    {request_tournament, Pid, {N = _NumberOfPlayers, K = _GamesPerMatch}} 
          when ((K > 0) and (K rem 2 == 1) and (N > 0))->
      utils:log("YM: Received a request_tournament of ~p players, best of ~p games per match.", [N, K]),
      
      %% Register up to N players in the tournament (randomly selected)
      {RandomSelection, Extra} = utils:rand_split(N, dict:to_list(C)),
      Tid = make_ref(),
      send_start_tournament(RandomSelection, Tid),
      TournamentPlayers = receive_accept_tournament(RandomSelection, Extra, [], Tid),
      utils:log("TM: Sending outside world confirmation of the tournament starting."),
      utils:dlog("Starting tournament with players: ~p", [TournamentPlayers], ?DEBUG),
      Pid ! {tournament_started, self(), {Tid, [P || {P, _LoginTicket} <- TournamentPlayers], no_value}},

      TMPid = spawn(tournament_manager, init, [TournamentPlayers, N, K, self()]),
      listen(R, C, [{Tid, TMPid} | O]);
    {tournament_status, Pid, Tid} -> 
      %% TODO: Return stats of tournament requested, possibly forward it along to the tournament
      huh;
    {user_status} ->
      %% TODO: decide what a user_info message looks like
      huh;
    Other ->
      utils:log("YM: ERROR - Message type not recognized: ~p", [Other]),
      listen(R, C, O)
  end.

%% Notifies tournaments when a user has logged in or out (out sets the Pid to 
%% no_value and in sets it to the new Pid)
notify_tournaments(UserName, _, []) ->
  utils:dlog("YM: Notified all tournaments of change of Pid (login or logout) of player ~p", [UserName], ?DEBUG);
notify_tournaments(UserName, Pid, {O, Os} = _OngoingTournaments) ->
  utils:dlog("YM: Notifying tournament ~p of user ~p's new Pid ~p.", [O, UserName, Pid], ?DEBUG),
  O ! {player_notification, UserName, Pid},
  notify_tournaments(UserName, Pid, Os).

%% Sends a start_tournament message to each player
send_start_tournament([], _Tid) ->
  done;
send_start_tournament([{UserName, {Pid, _LoginTicket}} | Ps], Tid) ->
  utils:log("TM: Sending start_tournament message to ~p", [UserName]),
  Pid ! {start_tournament, self(), UserName, Tid},
  send_start_tournament(Ps, Tid).

%% Assemble the player list
receive_accept_tournament([], [], AcceptedPlayers, _Tid) ->
  utils:log("TM: Player list has been generated. There are ~p players.", [length(AcceptedPlayers)]),
  utils:dlog("TM: Player list and pids: ~p", [AcceptedPlayers], ?DEBUG),
  AcceptedPlayers;
receive_accept_tournament([{UserName, {_OldPid, LoginTicket}} | Players], Extra, AcceptedPlayers, Tid) ->
  receive
    {accept_tournament, Pid, UserName, {Tid, LoginTicket}} ->
      utils:log("TM: Received accept_tournament message from ~p.", [UserName]),
      %% TODO: monitor them
      receive_accept_tournament(Players, Extra, [{UserName, Pid} | AcceptedPlayers], Tid);
    {reject_tournament, _Pid, UserName, {Tid, LoginTicket}} ->
      utils:log("TM: Received reject_tournament message from ~p", [UserName]),
      case Extra of
        [E | Es] ->
          send_start_tournament([E], Tid),
          receive_accept_tournament([E | Players], Es, AcceptedPlayers, Tid);
        [] ->
          utils:log("No extra players to send a start_tournament message."),
          receive_accept_tournament(Players, [], AcceptedPlayers, Tid)
      end
  after ?TIMEOUT ->
    utils:log("TM: Timed out waiting for player ~p.", [UserName]),
    case Extra of
      [E | Es] ->
        send_start_tournament([E], Tid),
        receive_accept_tournament([E | Players], Es, AcceptedPlayers, Tid);
      [] ->
        utils:log("No extra players to send a start_tournament message."),
        receive_accept_tournament(Players, [], AcceptedPlayers, Tid)
    end
  end.
