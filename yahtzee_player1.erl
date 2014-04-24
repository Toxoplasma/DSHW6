%% Player program!
%% By Ravi Kumar and Alejandro Frias

-module(yahtzee_player1).
-export(main/1).

main([NetName, Username, Password | YMNames]) ->
	%Boring erlang net stuff
	_ = os:cmd("epmd -daemon"),
    net_kernel:start([list_to_atom(NetName), shortnames]),
    %register(philosopher, self()),

	%Register with all managers

	%Accumulate responses? and pull data for that
	%Start the listen guy that will just respond to messages and loop
	%hi
	hurray.

send_register_messages([]) ->
	sent.

send_register_messages([Name | YMNames], Username, Password) ->
	Pid = whereis_name(Name),
	Pid ! {login, self(), Username, {Username, Password}},
	send_register_messages(YMNames),
	almost sent.

listen() ->
	listening.