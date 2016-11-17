-module(tictactoerl_fsm).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, % setup/teardown/upgrade
         handle_event/3, handle_sync_event/4, handle_info/3, % otp fsm events
         %% async events
         board_created/2, player_x_turn/2, player_o_turn/2, player_won/2,
         %% in progress...
         board_to_string/1, board_has_winner/1, mark_position_if_available/3]).

-include("board.hrl").
-record(state, {desc = "",
                board = #board_table{},
                status = "",
                pid}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_FSM CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, board_created, prompt()}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}. 

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(_Event, StateName, State) ->
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%       States      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

board_created(_Msg, S) -> 
    {next_state, player_o_turn, prompt(S#state{desc = "Game Board Creation...\n",
                                               status = "The game will start with Player X\n"
                                                        "Choose position: "})}.

player_x_turn(Position, S = #state{board = PreviousBoard}) ->
    NewBoard = mark_position_if_available(PreviousBoard, Position, "O"),

    NewState = S#state{board = NewBoard},

    case board_has_winner(NewBoard) of
        game_on -> {next_state, player_o_turn, prompt(NewState#state{desc = "\nPlayer X:\n", status = "Choose position: "})};
        game_over -> {next_state, player_won, prompt(NewState#state{desc = "\nPlayer O:\n", status = "PLAYER O WON!"})}
    end.

player_o_turn(Position, S=#state{board = PreviousBoard}) ->
    NewBoard = mark_position_if_available(PreviousBoard, Position, "X"),

    NewState = S#state{board = NewBoard},

    case board_has_winner(NewBoard) of
        game_on -> {next_state, player_x_turn, prompt(NewState#state{desc = "\nPlayer O:\n", status = "Choose position: "})};
        game_over -> {next_state, player_won, prompt(NewState#state{desc = "\nPlayer X:\n", status = "PLAYER X WON!"})}
    end.
    
player_won(_Msg, S) ->
    io:format("returning stop normal"),
    {stop, shutdown, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     Internals     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

prompt() -> %% actually not prompting, just sending an event
    gen_fsm:send_event(self(), game_on),
    #state{}.
prompt(State = #state{pid = undefined}) ->
    output(State),
    State#state{pid=get_input()};
prompt(State = #state{pid = Pid}) when is_pid(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    prompt(State#state{pid = undefined}).

output(State = #state{desc = Desc, board = Board, status = Status}) ->
    io:format("~s", [Desc]),
    io:format(board_to_string(Board)),
    io:format("~s", [Status]),
    State.

get_input()
 ->
    Parent = self(),
    spawn_link(fun() ->
        gen_fsm:send_event(Parent, option(io:get_line("")))
    end).

%%% TODO Unit test this
option(Input) when is_list(Input) -> option(iolist_to_binary(Input));
option(Input) when is_binary(Input) -> 
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

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%       Board       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

board_to_string(#board_table{top_left = Tl   , top_center = Tc   , top_right = Tr,
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

board_has_winner(Board) -> 
    case has_winner(Board) of
        true  -> game_over;
        false -> game_on
    end.

has_winner(Board) ->
    has_vertical_winner(Board) 
            orelse has_horizontal_winner(Board)
            orelse has_diagonal_winner(Board).

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
