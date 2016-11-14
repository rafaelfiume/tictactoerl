-module(board_test).
-include_lib("eunit/include/eunit.hrl").

-include("src/board.hrl").

print_board_test() ->
    Board = #board_table{top_left = "X", center = "O", bottom_center ="X"},
    
    ?assertEqual(" X |   |   \n"
                 "---+---+---\n"
                 "   | O |   \n"
                 "---+---+---\n"
                 "   | X |   \n", tictactoerl_fsm:board_to_string(Board)).