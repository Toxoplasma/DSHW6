%%% -------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Alejandro Frias, Ravi Kumar
%%%
%%% The Main Manager where registration occurs and new tournaments are started
%%%--------------------------------------------------------------------

-module (yahtzee_manager).

-export ([main/1]).

-define (DEBUG, true).
-define (TIMEOUT, case ?DEBUG of true -> 60000; false -> 5000 end).

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
    listen(dict:new(), dict:new(), dict:new(), dict:new()).

%% Listen for messages
%% R = a dictionary of usernames to {password, match-wins, match-losts, tournament-wins, tournament-losses, tournaments played}
%% C = a dictionary of usernames to {pid, Ref}
%% M = a dictionary of MonitorRef to UserName
%% T = a dictionary of Tid to 
listen(R = _RegisteredPlayersAndStats, C = _CurrentlyLoggedIn, M = _MonitorRefs, T = _Tournaments) ->
  utils:dlog("YM: All current users: ~p", [dict:to_list(C)], ?DEBUG),
  receive
    {login, Pid, UserName, PassWord} ->
      utils:log("YM: Received login message from ~p", [UserName]),
      case dict:find(UserName, R) of
        {ok, {PassWord, _Stats}} -> 
          %% demonitor the old pid of the user, if they're still logged in
          case dict:find(UserName, C) of
            {ok, {_OldPid, Ref}} ->
              utils:log("Demonitoring ~p's old Pid.", [UserName]),
              erlang:demonitor(Ref),
              NewM = dict:erase(Ref, M);

            error ->
              NewM = M
          end,
          %% Monitor the player's new Pid, using the monitor ref as the login ticket
          utils:log("Monitoring ~p.", [UserName]),
          MRef = erlang:monitor(process, Pid),
          
          utils:log("YM: Sending logged_in message to returning player ~p", [UserName]),
          Pid ! {logged_in, self(), UserName, MRef},

          notify_tournaments(UserName, Pid, dict:to_list(T)),
          listen(R, dict:store(UserName, {Pid, MRef}, C), dict:store(MRef, UserName, NewM), T);

        {ok, {_WrongPassword, _Stats}} ->
          utils:log("YM: Incorrect password given for player ~p", [UserName]),
          listen(R, C, M, T);

        error ->
          utils:log("YM: Sending logged_in message to new player ~p", [UserName]),
          %% Monitor new player
          MRef = monitor(process, Pid),
          Pid ! {logged_in, self(), UserName, MRef},
          listen(dict:store(UserName, {PassWord, {0, 0, 0, 0}}, R), dict:store(UserName, {Pid, MRef}, C), dict:store(MRef, UserName, M), T)
      end;

    {logout, _Pid, UserName, Ref} ->
      utils:log("YM: Received logout message from ~p", [UserName]),
      case dict:is_key(UserName, C) of
        true ->
          case dict:fetch(UserName, C) of
            {_, Ref} ->
              utils:log("YM: Logging player ~p out safely.", [UserName]),
              notify_tournaments(UserName, no_value, dict:to_list(T)),
              listen(R, dict:erase(UserName, C), dict:erase(Ref, M), T);
            {_, _WrongRef} ->
              utils:log("YM: Invalid logout attempt by player ~p", [UserName])
          end;

        false ->
          utils:log("YM: Player ~p is not currently logged in.", [UserName]),
          listen(R, C, M, T)
      end;

    {request_tournament, Pid, {N = _NumberOfPlayers, K = _GamesPerMatch}} 
          when ((K > 0) and (K rem 2 == 1) and (N > 0))->
      utils:log("YM: Received a request_tournament of ~p players, best of ~p games per match.", [N, K]),
      
      %% Register up to N players in the tournament (randomly selected)
      {RandomSelection, Extra} = utils:rand_split(N, dict:to_list(C)),
      Tid = make_ref(),
      send_start_tournament(RandomSelection, Tid),
      TournamentPlayers = receive_accept_tournament(RandomSelection, Extra, [], Tid),
      utils:log("YM: Sending outside world confirmation of the tournament starting."),
      utils:dlog("Starting tournament with players: ~p", [TournamentPlayers], ?DEBUG),
      Pid ! {tournament_started, self(), {Tid, [P || {P, _LoginTicket} <- TournamentPlayers], no_value}},

      TMPid = spawn(tournament_manager, init, [TournamentPlayers, K, Tid, self()]),
      NewR = update_tournament_wins(TournamentPlayers, R),
      listen(NewR, C, M, dict:store(Tid, {in_progress, TMPid}, T));
    {tournament_info, Pid, Tid} -> 
      case dict:find(Tid, T) of
        {ok, {in_progress, _}} ->
          utils:log("YM: sending tournament_status of in_progress tournament: ~p", [Tid]),
          Pid ! {tournament_status, self(), {Tid, in_progress, undefined, no_value}};
        {ok, {complete, Winner}} ->
          utils:log("YM: sending tournament_status of complete tournament: ~p. Winner: ~p", [Tid, Winner]),
          Pid ! {tournament_status, self(), {Tid, complete, Winner, no_value}};
        error ->
          utils:log("ERROR: Not a valid Tid for tournament_info message.")
      end,
      listen(R, C, M, T);

    {user_info, Pid, UserName} ->
      utils:log("YM: Received user_info for ~p", [UserName]),
      case dict:find(UserName, R) of
        {ok, {_Password, {MWins, MLosses, TWins, TPlayed}}} ->
          utils:log("YM: Sending user_info for ~p", [UserName]),
          Pid ! {user_status, self(), {UserName, MWins, MLosses, TPlayed, TWins}};
        error ->
          utils:log("ERROR: ~p is not a registered player.", [UserName])
      end,
      listen(R, C, M, T);

    {tournament_result, Tid, Winner} ->
      case dict:find(Winner, R) of
        {ok, {Password, {MWins, MLosses, TWins, TPlayed}}} ->
          utils:log("YM: Tournament ~p has finished with winner ~p. Storing stats.", [Tid, Winner]),
          Stats = {Password, {MWins, MLosses, TWins + 1, TPlayed}},
          listen(dict:store(Winner, Stats, R), C, M, dict:store(Tid, {complete, Winner}, T));

        error ->
          utils:log("~p is not a registered player.", [Winner]),
          listen(R, C, M, T)
      end;


    {match_result, {Winner, _}, {Loser, _} } ->
      utils:log("YM: Match between winner ~p and loser ~p being stored in stats.", [Winner, Loser]),
      case dict:find(Winner, R) of
        {ok, {WPassword, {WMWins, WMLosses, WTWins, WTPlayed}}} ->
          utils:log("YM: Storing match win for winner ~p ", [Winner]),
          WStats = {WPassword, {WMWins + 1, WMLosses, WTWins, WTPlayed}},
          WR = dict:store(Winner, WStats, R);
        error ->
          utils:log("~p is not a registered player.", [Winner]),
          WR = R
      end,
      case dict:find(Loser, R) of
        {ok, {LPassword, {LMWins, LMLosses, LTWins, LTPlayed}}} ->
          utils:log("YM: Storing match loss for loser ~p ", [Loser]),
          LStats = {LPassword, {LMWins, LMLosses + 1, LTWins, LTPlayed}},
          LR = dict:store(Loser, LStats, WR);
        error ->
          utils:log("~p is not a registered player.", [Loser]),
          LR = R
      end,
      listen(LR, C, M, T);

    {'DOWN', MonitorRef, _Type, _Object, Info} ->
      UserName = dict:fetch(MonitorRef, M),
      utils:log("YM: Player ~p crashed unexpectedly. Logging them out.", [UserName]),
      utils:log("YM: Reason for crash: ~p", [Info]),
      listen(R, dict:erase(UserName, C), dict:erase(MonitorRef, M), T);
    Other ->
      utils:log("YM: ERROR - Message type not handled: ~p", [Other]),
      listen(R, C, M, T)
  end.

%% Notifies tournaments when a user has logged in or out (out sets the Pid to 
%% no_value and in sets it to the new Pid)
notify_tournaments(UserName, _, []) ->
  utils:dlog("YM: Notified all tournaments of change of Pid (login or logout) of player ~p", [UserName], ?DEBUG);
notify_tournaments(UserName, Pid, [{_, {complete, _}} | Ts]) ->
  notify_tournaments(UserName, Pid, Ts);
notify_tournaments(UserName, Pid, [{Tid, {in_progress, TMPid}} | Ts] = _Tournaments) ->
  utils:dlog("YM: Notifying tournament ~p of user ~p's new login Pid ~p.", [Tid, UserName, Pid], ?DEBUG),
  TMPid ! {login, UserName, Pid},
  notify_tournaments(UserName, Pid, Ts).

%% Sends a start_tournament message to each player
send_start_tournament([], _Tid) ->
  done;
send_start_tournament([{UserName, {Pid, _LoginTicket}} | Ps], Tid) ->
  utils:log("YM: Sending start_tournament message to ~p", [UserName]),
  Pid ! {start_tournament, self(), UserName, Tid},
  send_start_tournament(Ps, Tid).

%% Assemble the player list
receive_accept_tournament([], _Extra, AcceptedPlayers, _Tid) ->
  utils:log("YM: Player list has been generated. There are ~p players.", [length(AcceptedPlayers)]),
  utils:dlog("YM: Player list and pids: ~p", [AcceptedPlayers], ?DEBUG),
  AcceptedPlayers;
receive_accept_tournament([{UserName, {_OldPid, LoginTicket}} | Players], Extra, AcceptedPlayers, Tid) ->
  receive
    {accept_tournament, Pid, UserName, {Tid, LoginTicket}} ->
      utils:log("YM: Received accept_tournament message from ~p.", [UserName]),
      receive_accept_tournament(Players, Extra, [{UserName, Pid} | AcceptedPlayers], Tid);
    {reject_tournament, _Pid, UserName, {Tid, LoginTicket}} ->
      utils:log("YM: Received reject_tournament message from ~p", [UserName]),
      case Extra of
        [E | Es] ->
          send_start_tournament([E], Tid),
          receive_accept_tournament([E | Players], Es, AcceptedPlayers, Tid);
        [] ->
          utils:log("No extra players to send a start_tournament message."),
          receive_accept_tournament(Players, [], AcceptedPlayers, Tid)
      end
  after ?TIMEOUT ->
    utils:log("YM: Timed out waiting for player ~p.", [UserName]),
    case Extra of
      [E | Es] ->
        send_start_tournament([E], Tid),
        receive_accept_tournament([E | Players], Es, AcceptedPlayers, Tid);
      [] ->
        utils:log("No extra players to send a start_tournament message."),
        receive_accept_tournament(Players, [], AcceptedPlayers, Tid)
    end
  end.

update_tournament_wins([], R) ->
  R;
update_tournament_wins([{P, _} | Ps], R) ->
  {Password, {MWins, MLosses, TWins, TPlayed}} = dict:fetch(P, R),
  Stats = {Password, {MWins, MLosses, TWins, TPlayed + 1}},
  update_tournament_wins(Ps, dict:store(P, Stats, R)).
