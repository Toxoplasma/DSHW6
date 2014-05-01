%%% ----------------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Alejandro Frias, Ravi Kumar
%%%
%%% Tests and stuff.
%%% 
%%% Functions for by-hand testing as well as more built out tests are here.
%%%-----------------------------------------------------------------------------



-module (t).

-export([init/1,
         init/2,
         connect/1,
         login/3,
         request_tournament/3,
         get_stats/2]).

-define (TIMEOUT, 10000).

%% Alphabet (lower case and upper case) and space
-define (CHARS, lists:seq(97, 122) ++ lists:seq(65, 90) ++ [32]).


%% Only need to call once to the daemon running and kernel started.
%% Also seeds the random to now() and returns the seed used for repeating the 
%% test if desired
init(Node) ->
    Seed = now(),
    init(Node, Seed).

%% If you desire to init with a given seed. Good for reproducing tests
init(Node, Seed = {A1, A2, A3}) ->
    _ = os:cmd("epmd -daemon"),
    net_kernel:start([testy, shortnames]),
    utils:log("Seeding random with ~p", [Seed]),
    random:seed(A1, A2, A3),
    connect(Node),
    {yahtzee_manager, Node}.

%% Used to (re)connect to the original node for global registry access.
connect(Node) ->
    case net_kernel:connect_node(Node) of
        true ->
            utils:log("Connected Successfully to ~p", [Node]);
        _Other ->
            utils:log("Failed to Connect to ~p", [Node])
    end.

get_stats(Node, Username) ->
    {yahtzee_manager, Node} ! {user_info, self(), Username},
    receive
        {user_status, _ReplyPid, Stats = {Username, MWins, MLosses, TPlayed, TWins}} ->
            utils:log("Player ~p has ~p match wins, ~p match losses, ~p tournaments played, and ~p tournaments won", [Username, MWins, MLosses, TPlayed, TWins]),
            Stats
    end.

%% Fake logs a player in and returns the LoginTicket for that fake player
login(Pid, UserName, PassWord) ->
    Pid ! {login, self(), UserName, PassWord},
    receive
        Msg -> {logged_in, _PlayerPid, LoginTicket} = Msg
    end,
    LoginTicket.

request_tournament(Node, N, K) ->
    {yahtzee_manager, Node} ! {request_tournament, self(), {N, K}},
    receive
        {tournament_started, _ReplyPid, {Tid, Players, _}} ->
            utils:log("Players: ~p", [Players]),
            Tid
    end.


%TODO: test that multiple managers works