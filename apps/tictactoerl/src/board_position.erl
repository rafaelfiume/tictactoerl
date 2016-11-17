-module(board_position).

-export([user_input/1]).

user_input(Input) when is_list(Input) -> user_input(iolist_to_binary(Input));
user_input(Input) when is_binary(Input) -> 
    case Input of 
        <<"1", _/binary>> -> 1;
        <<"2", _/binary>> -> 2;
        <<"3", _/binary>> -> 3;
        <<"4", _/binary>> -> 4;
        <<"5", _/binary>> -> 5;
        <<"6", _/binary>> -> 6;
        <<"7", _/binary>> -> 7;
        <<"8", _/binary>> -> 8;
        <<"9", _/binary>> -> 9;
        _ -> unknown
    end.