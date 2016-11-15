-module(board_test).
-include_lib("eunit/include/eunit.hrl").

-include("src/board.hrl").

print_board_test() ->
    Board = #board_table{top_left = "X", center = "O", bottom_center ="X"},

    PrintedBoard = tictactoerl_fsm:board_to_string(Board),

    ?assertEqual(" X |   |   \n"
                 "---+---+---\n"
                 "   | O |   \n"
                 "---+---+---\n"
                 "   | X |   \n", PrintedBoard).

game_is_in_progress_test() ->
    EmptyBoard = #board_table{},

    GameStatus = tictactoerl_fsm:board_has_winner(EmptyBoard),

    game_on = GameStatus.

%%%%%%%%%%%%%%%%%%%%
%%%     Wins     %%%
%%%%%%%%%%%%%%%%%%%%

wins_with_a_vertical_row_in_the_left_test() ->
    Board = #board_table{top_left = "X", 
                         mid_left = "X", 
                         bottom_left ="X"},

    GameStatus = tictactoerl_fsm:board_has_winner(Board),

    game_over = GameStatus.

wins_with_a_vertical_row_in_the_center_test() ->
    Board = #board_table{top_center = "X", 
                         center = "X", 
                         bottom_center ="X"},

    GameStatus = tictactoerl_fsm:board_has_winner(Board),

    game_over = GameStatus.

wins_with_a_vertical_row_in_the_right_test() ->
    Board = #board_table{top_right = "X", mid_right = "X", bottom_right ="X"},

    GameStatus = tictactoerl_fsm:board_has_winner(Board),

    game_over = GameStatus.

wins_with_a_horizontal_row_in_the_top_test() ->
    Board = #board_table{top_left = "O", top_center = "O", top_right ="O"},

    GameStatus = tictactoerl_fsm:board_has_winner(Board),

    game_over = GameStatus.

wins_with_a_horizontal_row_in_the_center_test() ->
    Board = #board_table{mid_left = "O", center = "O", mid_right ="O"},

    GameStatus = tictactoerl_fsm:board_has_winner(Board),

    game_over = GameStatus.

wins_with_a_descendent_diagonal_row_test() ->
    Board = #board_table{top_left = "X", 
                                        center = "X", 
                                                     bottom_right ="X"},

    GameStatus = tictactoerl_fsm:board_has_winner(Board),

    game_over = GameStatus.

wins_with_a_ascendent_diagonal_row_test() ->
    Board = #board_table{bottom_left = "X", 
                                           center = "X", 
                                                        top_right ="X"},

    GameStatus = tictactoerl_fsm:board_has_winner(Board),

    game_over = GameStatus.