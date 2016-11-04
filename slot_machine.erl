-module(slot_machine).
-export([spin/0, auto_spin/1]).

auto_spin(N) ->
  test(N, 0).

test(0, Acc) ->
  Acc;

test(N, Acc) ->
  {_, Payout} = spin(),
  test(N-1, Acc + Payout).

spin() ->
  Line = {take(), take(), take()},
  {Line, payout(Line)}.

take() ->
  Reel = [cherry, cherry, empty, empty, empty, empty, empty, bar, bar, bar, bar, bar, empty, empty, empty, empty, empty, seven, seven, seven, seven, seven, seven, seven, seven, empty, empty, empty, empty, empty, bar, bar, bar, bar, bar, empty, empty, empty, empty, empty, empty, cherry, cherry, empty, empty, empty, empty, empty, empty, bar_bar, bar_bar, bar_bar, bar_bar, bar_bar, bar_bar, bar_bar, empty, empty, empty, empty, empty, empty, cherry, empty, empty, empty, empty, empty, empty, bar_bar, bar_bar, bar_bar, bar_bar, bar_bar, bar_bar, empty, empty, empty, empty, empty, empty, bar, bar, bar, bar, bar, bar, empty, empty, empty, empty, empty, empty, bar_bar_bar, bar_bar_bar, bar_bar_bar, bar_bar_bar, bar_bar_bar, bar_bar_bar, bar_bar_bar, bar_bar_bar, bar_bar_bar, bar_bar_bar, bar_bar_bar, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, jackpot, jackpot, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
  lists:nth(rand:uniform(128), Reel).

payout({jackpot, jackpot, jackpot}) -> 1666;
payout({seven, seven, seven}) -> 300;
payout({bar_bar_bar,bar_bar_bar,bar_bar_bar}) -> 100;
payout({bar_bar,bar_bar,bar_bar}) -> 50;
payout({bar,bar,bar}) -> 25;
payout({cherry,cherry,cherry}) -> 12;
payout({_,cherry,cherry}) -> 6;
payout({cherry,_,cherry}) -> 6;
payout({cherry,cherry,_}) -> 6;
payout({cherry,_,_}) -> 3;
payout({_,cherry,_}) -> 3;
payout({_,_,cherry}) -> 3;
payout({A,B,C}) ->
  case {any_bar(A), any_bar(B), any_bar(C)} of
    {true, true, true} ->
          12;
    _ ->
      0
  end.

any_bar(A) ->
  (A == bar) or (A == bar_bar) or (A == bar_bar_bar).

