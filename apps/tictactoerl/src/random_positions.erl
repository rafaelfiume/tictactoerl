-module(random_positions).

-export([new/0, next/0, next/1]).

new() ->
    Pid = spawn_link(fun() -> loop([]) end),
    register(random_positions_gen, Pid).

next() ->
    random_positions_gen ! {next_number, self()},
    receive V -> V end.

% This will loop forever if ListOfNumbers has 9 numbers or more.
% Trusting the client this is not going to happen
next(ListOfNumbers) when is_list(ListOfNumbers) ->
    Number = rand:uniform(9),
    case Number =/= 0 andalso not lists:member(Number, ListOfNumbers) of
        true -> Number;
        false -> next(ListOfNumbers)
    end.

loop(ListOfNumbers) ->
    receive
        {next_number, From} ->
            RandomNumber = next(ListOfNumbers),
            From ! RandomNumber,
            loop([RandomNumber|ListOfNumbers])
    end.

% TODO : 26/11/2016 : Missing shutdown
