-module(board_position).

-export([user_input/1]).

user_input(Input) when is_list(Input) -> user_input(iolist_to_binary(Input));
user_input(Input) when is_binary(Input) ->
    case re:run(Input, "^([1-9])(\\n)?$", [{capture, [1], list}]) of
        {match, Captured} ->
            [H|_] = Captured, 
            list_to_integer(H);
        nomatch ->
             unknown
    end.
