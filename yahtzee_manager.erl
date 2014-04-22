%%% -------------------------------------------------------------------
%%% CSCI182E - Distributed Systems
%%% @author Alejandro Frias, Ravi Kumar
%%%
%%% The Main Manager where registration occurs and new tournaments are started
%%%--------------------------------------------------------------------

-module (yahtzee_manager).

-export ([main/1]).

main([Name]) ->
  %% Start kernal, cmd daemon, register name

  utils:log("Registering yahtzee_manager under name ~p", [Name]),
  utils:log("yahtzee_manager is ready to go.");

  %% Go into listening state (or use gen_server) waiting for registration/logins, 
  %% tournament starts, and stats requests
main(Other) ->
  utils:log("Incorrect use. Usage: erl -noshell -run main <name>").

