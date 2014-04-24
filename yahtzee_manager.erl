%%% -------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Alejandro Frias, Ravi Kumar
%%%
%%% The Main Manager where registration occurs and new tournaments are started
%%%--------------------------------------------------------------------

-module (yahtzee_manager).

-export ([main/1]).

main([Name]) ->
  init(Name);
main([Name, Seed]) ->
  init(Name, Seed);
main(_Other) ->
  utils:log("Incorrect use. Usage: erl -noshell -run main <name> [<seed>]").

%% Start kernal, cmd daemon, register name
init(Name) ->
    Seed = now(),
    init(Name, Seed).

%% If you desire to init with a given seed. Good for reproducing tests
init(Name, Seed = {A1, A2, A3}) ->
    _ = os:cmd("epmd -daemon"),
    utils:log("Seeding random with ~p", [Seed]),
    random:seed(A1, A2, A3),
    net_kernel:start([list_to_atom(Name), shortnames]),
    utils:log("Registered as ~w", [node()]),
    listen(dict:new(), [], []).

%% Listen for messages
listen(R = _RegisteredPlayersAndStats, C = _CurrentlyLoggedIn, O = _OngoingTournaments) ->
  receive
    Other ->
      utils:log("ERROR - Message type not recognized: ~p", [Other]),
      listen(R, C, O)
  end.


