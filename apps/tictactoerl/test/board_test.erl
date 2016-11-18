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

wins_with_a__vertical__row_in_the_left_test() ->
    Board = #board_table{top_left = "X", 
                         mid_left = "X", 
                         bottom_left ="X"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a__vertical__row_in_the_center_test() ->
    Board = #board_table{top_center = "X", 
                         center = "X", 
                         bottom_center ="X"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a__vertical__row_in_the_right_test() ->
    Board = #board_table{top_right = "X", mid_right = "X", bottom_right ="X"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a__horizontal__row_in_the_top_test() ->
    Board = #board_table{top_left = "O", top_center = "O", top_right ="O"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a__horizontal__row_in_the_center_test() ->
    Board = #board_table{mid_left = "O", center = "O", mid_right ="O"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a__horizontal__row_in_the_bottom_test() ->
    Board = #board_table{bottom_left = "O", bottom_center = "O", bottom_right ="O"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a__descendent_diagonal__row_test() ->
    Board = #board_table{top_left = "X", 
                                        center = "X", 
                                                     bottom_right ="X"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

wins_with_a__ascendent_diagonal__row_test() ->
    Board = #board_table{bottom_left = "X", 
                                           center = "X", 
                                                        top_right ="X"},

    GameStatus = board:has_winner(Board),

    game_over = GameStatus.

%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&
%%%       Board Position      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mark_position_when__topleft__is_available_test() ->
    PreviousBoad = #board_table{},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, 1, "X"),

    Free = true,
    CurrentBoard = #board_table{top_left = "X"}.

mark_position_when__topcenter__is_available_test() ->
    PreviousBoad = #board_table{},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, 2, "X"),

    Free = true,
    CurrentBoard = #board_table{top_center = "X"}.

mark_position_when__topright__is_available_test() ->
    PreviousBoad = #board_table{},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, 3, "X"),
    
    Free = true,
    CurrentBoard = #board_table{top_right = "X"}.

mark_position_when__midleft__is_available_test() ->
    PreviousBoad = #board_table{},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, 4, "X"),

    Free = true,
    CurrentBoard = #board_table{mid_left = "X"}.

mark_position_when__center__is_available_test() ->
    PreviousBoad = #board_table{},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, 5, "X"),

    Free = true,
    CurrentBoard = #board_table{center = "X"}.

mark_position_when__midright__is_available_test() ->
    PreviousBoad = #board_table{},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, 6, "X"),

    Free = true,
    CurrentBoard = #board_table{mid_right = "X"}.

mark_position_when__bottomleft__is_available_test() ->
    PreviousBoad = #board_table{},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, 7, "X"),

    Free = true,
    CurrentBoard = #board_table{bottom_left = "X"}.

mark_position_when__bottomcenter__is_available_test() ->
    PreviousBoad = #board_table{},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, 8, "X"),

    Free = true,
    CurrentBoard = #board_table{bottom_center = "X"}.

mark_position_when__bottomright__is_available_test() ->
    PreviousBoad = #board_table{top_left = "X", center = "O", top_right = "X"},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, 9, "O"),

    Free = true,
    CurrentBoard = #board_table{top_left = "X", center = "O", top_right = "X", bottom_right = "O"}.

%%%%%%%%%%%%%%%%%%%%
%%%   SAD PATH   %%%
%%%%%%%%%%%%%%%%%%%%

does_not_mark_position_when_player_chooses_an__already_marked_one__test() ->
    PreviousBoad = #board_table{top_left = "X", center = "O", top_right = "X"},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, 1, "O"),

    Free = false,
    CurrentBoard = PreviousBoad.

does_not_mark_position_when_player_chooses__an_unknown_position__test() ->
    PreviousBoad = #board_table{top_left = "X", center = "O", top_right = "X"},

    {Free, CurrentBoard} = board:mark_position_if_available(PreviousBoad, unknown, "O"),

    Free = false,
    CurrentBoard = PreviousBoad.