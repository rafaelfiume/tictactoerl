-module(board_position_test).
-include_lib("eunit/include/eunit.hrl").

% test generator function
maps_user_input_to_correct_board_position_test_() ->
    [ % test generators
     ?_assertEqual(1, board_position:user_input("1")),
     ?_assertEqual(9, board_position:user_input("9")),
     ?_assertEqual(1, board_position:user_input("1\n")),
     ?_assertEqual(2, board_position:user_input("2\n")),
     ?_assertEqual(3, board_position:user_input("3\n")),
     ?_assertEqual(4, board_position:user_input("4\n")),
     ?_assertEqual(5, board_position:user_input("5\n")),
     ?_assertEqual(6, board_position:user_input("6\n")),
     ?_assertEqual(7, board_position:user_input("7\n")),
     ?_assertEqual(8, board_position:user_input("8\n")),
     ?_assertEqual(9, board_position:user_input("9\n")),
     
     ?_assertEqual(unknown_position, board_position:user_input("k\n")),
     ?_assertEqual(unknown_position, board_position:user_input("10")),
     ?_assertEqual(unknown_position, board_position:user_input("10\n")),
     ?_assertEqual(unknown_position, board_position:user_input("11"))
    ].
    
