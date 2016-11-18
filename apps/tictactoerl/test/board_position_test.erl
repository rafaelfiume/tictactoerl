-module(board_position_test).
-include_lib("eunit/include/eunit.hrl").

maps_user_input_to_correct_board_position_test() ->
    1 = board_position:user_input("1"),
    9 = board_position:user_input("9"),
    1 = board_position:user_input("1\n"),
    2 = board_position:user_input("2\n"),
    3 = board_position:user_input("3\n"),
    4 = board_position:user_input("4\n"),
    5 = board_position:user_input("5\n"),
    6 = board_position:user_input("6\n"),
    7 = board_position:user_input("7\n"),
    8 = board_position:user_input("8\n"),
    9 = board_position:user_input("9\n"),
    unknown = board_position:user_input("k\n"),
    unknown = board_position:user_input("10"),
    unknown = board_position:user_input("10\n").

