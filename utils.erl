%%% -------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Alejandro Frias, Ravi Kumar
%%%
%%% Helpful utility functions used for Homework 6.
%%%--------------------------------------------------------------------


-module (utils).

-export ([timestamp/0,
          log/1,
          log/2,
          first_n_elements/2,
          log2/1,
          logB/2,
          pow2/1,
          dlog/2,
          dlog/3,
          droplast/1,
          rand_seq/2,
          rand_split/2,
          replace/3,
          ceiling/1,
          insert/3]).

%% Prints a time stamp.
timestamp() ->
  {A, B, Milli} = now(),
  {{Y, Month, D}, {H, Min, S}} = calendar:now_to_local_time({A,B,0}),
  io:format("~p-~p-~p ~p:~p:~p.~p: ", [Y, Month, D, H, Min, S, Milli]).

%% Prints a message with a time stamped
log(Message) ->
  timestamp(),
  io:format("~s~n", [Message]).
log(Message, Format) ->
  S = io_lib:format(Message, Format),
  log(S).

%% Logs a message, but accepts a debug mode which turns it off or on
dlog(Message, Debug) ->
  case Debug of
    true ->
      timestamp(),
      io:format("~s~n", [Message]);
    _Other ->
      ok
  end.
dlog(Message, Format, Debug) ->
  S = io_lib:format(Message, Format),
  dlog(S, Debug).

%% Drops the last element of a list (takes linear time, not constant)
droplast(L) ->
  first_n_elements(length(L) - 1, L).

%% Grabs the first n elements from a list, or all the items if N > length(List)
first_n_elements(N, List) ->
  case length(List) > N of
    true ->
      {Result, _} = lists:split(N, List),
      Result;
    false ->
      List
  end.

%% Using bit shift left to return 2^M
pow2(M) ->
  1 bsl M.

%% Log with given base
logB(Num, Base) ->
  math:log(Num) / math:log(Base).

%% Log base 2
log2(Num) ->
  logB(Num, 2).

%% Generates a list of M random numbers, each between 1 and N
rand_seq(N, M) ->
  [random:uniform(N) || _X <- lists:seq(1,M)].

%% Splits the list into N randomly selected items on one side and the rest on the other.
rand_split(N, L) ->
  rand_split(N, [], L).

rand_split(_N, L, []) ->
  {L, []};
rand_split(0, L, R) ->
  {L, R};
rand_split(N, L, R) ->
  Item = lists:nth(random:uniform(length(R)), R),
  Rs = lists:delete(Item, R),
  rand_split(N - 1, [Item | L], Rs).

%% Replaces the Nth element with E in list L
replace(N, E, L) ->
  {L1, L2} = lists:split(N - 1, L),
  L1 ++ [E | tl(L2)].

%% Inserts element E just before the Nth element in list L
insert(N, E, L) ->
  {L1, L2} = lists:split(N - 1, L),
  L1 ++ [E | L2].

%% Finds ceiling of N
ceiling(N) ->
  case trunc(N) == N of
    true ->
      trunc(N);
    false ->
      trunc(N) + 1
  end.
