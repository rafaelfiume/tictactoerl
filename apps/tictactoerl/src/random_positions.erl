-module(random_positions).

-export([next/1]).

next(ListOfNumbers) ->
    Number = rand:uniform(9),
    case Number =/= 0 andalso not lists:member(Number, ListOfNumbers) of
        true -> [Number|ListOfNumbers];
        false -> next(ListOfNumbers)
    end.