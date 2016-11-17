-module(casino).
-export([start_all/2, start_aggregator/0, aggregator/1, spin_machine/1]).

start_all(0, _) ->
  io:format("All machines are spawned~n");

start_all(N, Aggregator) ->
  spawn(casino, spin_machine, [Aggregator]),
  start_all(N - 1, Aggregator).

spin_machine(Aggregator) ->
  Aggregator ! {done, slot_machine:auto_spin(1000000)}.

start_aggregator() ->
  register(aggregator, spawn(casino, aggregator, [[]])).

aggregator(Acc) ->
  receive
    {done, Paid} ->
      aggregator([Paid | Acc]);

    {say_payout} ->
      io:format("Machines played: ~b Avg payout: ~f~n", [length(Acc), payout(Acc)]),
      aggregator(Acc);

    {flush} ->
      aggregator([]);

    _ ->
      io:format("Message received~n")
  end.

payout([]) ->
  0.0;

payout(L) ->
  Sum = lists:foldl(fun(X, Sum) -> X + Sum end, 0, L),
  Sum * 1.0 / length(L) / 1000000.
