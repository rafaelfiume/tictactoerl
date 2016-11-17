-module(board).

-export([to_string/1, has_winner/1, mark_position_if_available/3]).

-include("board_table.hrl").

to_string(#board_table{top_left = Tl   , top_center = Tc   , top_right = Tr,
                             mid_left = Ml   , center = Ce       , mid_right = Mr,
                             bottom_left = Bl, bottom_center = Bc, bottom_right = Br}) ->
    lists:flatten(io_lib:format(
                  " ~s | ~s | ~s \n"
                  "---+---+---\n"
                  " ~s | ~s | ~s \n"
                  "---+---+---\n"
                  " ~s | ~s | ~s \n", [Tl, Tc, Tr,
                                       Ml, Ce, Mr,
                                       Bl, Bc, Br])).

has_winner(Board) -> 
    case has_vertical_winner(Board) orelse has_horizontal_winner(Board) orelse has_diagonal_winner(Board) of
        true  -> game_over;
        false -> game_on
    end.

has_vertical_winner(#board_table{top_left   = A, mid_left  = A, bottom_left   = A}) when A =:= "X" orelse A =:= "O" -> true;
has_vertical_winner(#board_table{top_center = A, center    = A, bottom_center = A}) when A =:= "X" orelse A =:= "O" -> true;
has_vertical_winner(#board_table{top_right  = A, mid_right = A, bottom_right  = A}) when A =:= "X" orelse A =:= "O" -> true;
has_vertical_winner(_) -> false.

has_horizontal_winner(#board_table{top_left    = A, top_center    = A, top_right    = A}) when A =:= "X" orelse A =:= "O" -> true;
has_horizontal_winner(#board_table{mid_left    = A, center        = A, mid_right    = A}) when A =:= "X" orelse A =:= "O" -> true;
has_horizontal_winner(#board_table{bottom_left = A, bottom_center = A, bottom_right = A}) when A =:= "X" orelse A =:= "O" -> true;
has_horizontal_winner(_) -> false.

has_diagonal_winner(#board_table{bottom_left = A, center = A, top_right    = A}) when A =:= "X" orelse A =:= "O" -> true;
has_diagonal_winner(#board_table{top_left    = A, center = A, bottom_right = A}) when A =:= "X" orelse A =:= "O" -> true;
has_diagonal_winner(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&
%%%       Board Position      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mark_position_if_available(B = #board_table{}, 1, Player) -> B#board_table{top_left      = Player};
mark_position_if_available(B = #board_table{}, 2, Player) -> B#board_table{top_center    = Player};
mark_position_if_available(B = #board_table{}, 3, Player) -> B#board_table{top_right     = Player};
mark_position_if_available(B = #board_table{}, 4, Player) -> B#board_table{mid_left      = Player};
mark_position_if_available(B = #board_table{}, 5, Player) -> B#board_table{center        = Player};
mark_position_if_available(B = #board_table{}, 6, Player) -> B#board_table{mid_right     = Player};
mark_position_if_available(B = #board_table{}, 7, Player) -> B#board_table{bottom_left   = Player};
mark_position_if_available(B = #board_table{}, 8, Player) -> B#board_table{bottom_center = Player};
mark_position_if_available(B = #board_table{}, 9, Player) -> B#board_table{bottom_right  = Player};
mark_position_if_available(B = #board_table{}, Position, _Player) -> io:format("Unknown position ~p", [Position]), B#board_table{}. %% TODO Missing unknown cases
