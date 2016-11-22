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

has_winner(B) -> 
    case has_vertical_winner(B) orelse has_horizontal_winner(B) orelse has_diagonal_winner(B) of
        true  -> game_over;
        false -> game_on
    end.

mark_position_if_available(B, 1, Player) -> 
    case cell_is_free_at(B#board_table.top_left) of
        free -> {true, B#board_table{top_left = Player}};
        marked -> {false, B}
    end;
mark_position_if_available(B, 2, Player) -> 
    case cell_is_free_at(B#board_table.top_center) of
        free -> {true, B#board_table{top_center = Player}};
        marked -> {false, B}
    end;
mark_position_if_available(B, 3, Player) -> 
    case cell_is_free_at(B#board_table.top_right) of
        free -> {true, B#board_table{top_right = Player}};
        marked -> {false, B}
    end;
mark_position_if_available(B, 4, Player) -> 
    case cell_is_free_at(B#board_table.mid_left) of
        free -> {true, B#board_table{mid_left = Player}};
        marked -> {false, B}
    end;
mark_position_if_available(B, 5, Player) -> 
    case cell_is_free_at(B#board_table.center) of
        free -> {true, B#board_table{center = Player}};
        marked -> {false, B}
    end;
mark_position_if_available(B, 6, Player) -> 
    case cell_is_free_at(B#board_table.mid_right) of
        free -> {true, B#board_table{mid_right = Player}};
        marked -> {false, B}
    end;
mark_position_if_available(B, 7, Player) -> 
    case cell_is_free_at(B#board_table.bottom_left) of
        free -> {true, B#board_table{bottom_left = Player}};
        marked -> {false, B}
    end;
mark_position_if_available(B, 8, Player) -> 
    case cell_is_free_at(B#board_table.bottom_center) of
        free -> {true, B#board_table{bottom_center = Player}};
        marked -> {false, B}
    end;
mark_position_if_available(B, 9, Player) -> 
    case cell_is_free_at(B#board_table.bottom_right) of
        free -> {true, B#board_table{bottom_right = Player}};
        marked -> {false, B}
    end;
mark_position_if_available(B, unknown_position, _Player) -> 
        {false, B}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     Internals     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

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

cell_is_free_at(Position) ->
    case Position =:= ?NO_SELECTION of
        true -> free;
        false -> marked
    end.

