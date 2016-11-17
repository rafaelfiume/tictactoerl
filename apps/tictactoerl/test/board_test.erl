-module(board_test).
-include_lib("eunit/include/eunit.hrl").

-include("../src/board_table.hrl").

print_board_test() ->
    Board = #board_table{top_left = "X", center = "O", bottom_center ="X"},

    PrintedBoard = board:to_string(Board),

    ?assertEqual(" X |   |   \n"
                 "---+---+---\n"
                 "   | O |   \n"
                 "---+---+---\n"
                 "   | X |   \n", PrintedBoard).

game_is_in_progress_test() ->
    EmptyBoard = #board_table{},

    GameStatus = board:has_winner(EmptyBoard),

    game_on = GameStatus.

%%%%%%%%%%%%%%%%%%%%
%%%     Wins     %%%
%%%%%%%%%%%%%%%%%%%%

wins_with_a_vertical_row_in_the_left_test() ->
    Board = #board_table{top_left = "X", 
                         mid_left = "X", 
                         bottom_left ="X"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a_vertical_row_in_the_center_test() ->
    Board = #board_table{top_center = "X", 
                         center = "X", 
                         bottom_center ="X"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a_vertical_row_in_the_right_test() ->
    Board = #board_table{top_right = "X", mid_right = "X", bottom_right ="X"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a_horizontal_row_in_the_top_test() ->
    Board = #board_table{top_left = "O", top_center = "O", top_right ="O"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a_horizontal_row_in_the_center_test() ->
    Board = #board_table{mid_left = "O", center = "O", mid_right ="O"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a_descendent_diagonal_row_test() ->
    Board = #board_table{top_left = "X", 
                                        center = "X", 
                                                     bottom_right ="X"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a_ascendent_diagonal_row_test() ->
    Board = #board_table{bottom_left = "X", 
                                           center = "X", 
                                                        top_right ="X"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&
%%%       Board Position      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maps_user_input_1_to_correct_board_position_test() ->
    PreviousBoad = #board_table{},
    CurrentBoard = board:mark_position_if_available(PreviousBoad, 1, "X"),
    CurrentBoard = #board_table{top_left = "X"}.

maps_user_input_2_to_correct_board_position_test() ->
    PreviousBoad = #board_table{},
    CurrentBoard = board:mark_position_if_available(PreviousBoad, 2, "X"),
    CurrentBoard = #board_table{top_center = "X"}.

maps_user_input_3_to_correct_board_position_test() ->
    PreviousBoad = #board_table{},
    CurrentBoard = board:mark_position_if_available(PreviousBoad, 3, "X"),
    CurrentBoard = #board_table{top_right = "X"}.

maps_user_input_4_to_correct_board_position_test() ->
    PreviousBoad = #board_table{},
    CurrentBoard = board:mark_position_if_available(PreviousBoad, 4, "X"),
    CurrentBoard = #board_table{mid_left = "X"}.

maps_user_input_5_to_correct_board_position_test() ->
    PreviousBoad = #board_table{},
    CurrentBoard = board:mark_position_if_available(PreviousBoad, 5, "X"),
    CurrentBoard = #board_table{center = "X"}.

maps_user_input_6_to_correct_board_position_test() ->
    PreviousBoad = #board_table{},
    CurrentBoard = board:mark_position_if_available(PreviousBoad, 6, "X"),
    CurrentBoard = #board_table{mid_right = "X"}.

maps_user_input_7_to_correct_board_position_test() ->
    PreviousBoad = #board_table{},
    CurrentBoard = board:mark_position_if_available(PreviousBoad, 7, "X"),
    CurrentBoard = #board_table{bottom_left = "X"}.

maps_user_input_8_to_correct_board_position_test() ->
    PreviousBoad = #board_table{},
    CurrentBoard = board:mark_position_if_available(PreviousBoad, 8, "X"),
    CurrentBoard = #board_table{bottom_center = "X"}.

maps_user_input_9_to_correct_board_position_test() ->
    PreviousBoad = #board_table{top_left = "X", center = "O", top_right = "X"},
    CurrentBoard = board:mark_position_if_available(PreviousBoad, 9, "O"),
    CurrentBoard = #board_table{top_left = "X", center = "O", top_right = "X", bottom_right = "O"}.

%% TODO Missing unknown cases