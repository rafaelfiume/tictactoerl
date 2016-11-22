-module(board_position).

-export([user_input/1]).

user_input(Input) when is_list(Input) -> user_input(iolist_to_binary(Input));
user_input(Input) when is_binary(Input) ->
    case re:run(Input, "^([1-9])$", [{capture, [1], list}]) of
        {match, Captured} ->
            [H|_] = Captured, 
            list_to_integer(H);
        nomatch ->
             unknown_position
    end;
% this was added due to get_input (in tictactoe_fsm) returning [eof] during its unit test (tictactoe_fsm_tests); 
% not sure this is the best approach, though
user_input(eof) -> 
    unknown_position.
