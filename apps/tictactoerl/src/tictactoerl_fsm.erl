-module(tictactoerl_fsm).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, % setup/teardown/upgrade
         handle_event/3, handle_sync_event/4, handle_info/3, % otp fsm events
         %% async events
         board_created/2, player_x_turn/2, player_o_turn/2, player_won/2,
         %% in progress...
         board_to_string/1, board_has_winner/1]).

-include("board.hrl").
-record(state, {desc = "",
                board = #board_table{},
                status = ""}).

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

board_created(_Msg, State) -> 
    {next_state, player_x_turn, prompt(State#state{desc = "Game Board Creation...\n",
                                                   status = "The game will start with Player X\n"})}.

player_x_turn(game_on, State) ->
    {next_state, player_o_turn, prompt(State#state{desc = "\nPlayer X:\n",
                                                   status = "Choose position:"})};
player_x_turn(game_over, State) ->
    {next_state, player_won, prompt(State#state{desc = "\nPlayer X:\n",
                                                   status = "PLAYER X WON"})} .

player_o_turn(game_on, State) ->
    {next_state, player_x_turn, prompt(State#state{desc = "Player O:\n",
                                                   status = "Choose position:"})};
player_o_turn(game_over, State) ->
    {next_state, player_won, prompt(State#state{desc = "\nPlayer X:\n",
                                                   status = "PLAYER X WON"})}.
    
player_won(_Msg, State) ->
    {stop, normal, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     Internals     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

prompt() -> %% actually not prompting, just sending an event
    gen_fsm:send_event(self(), game_on),
    #state{}.
prompt(State = #state{desc = Desc, board = Board, status = Status}) ->
    io:format("~s", [Desc]),
    io:format(board_to_string(Board)),
    io:format("~s", [Status]),
    gen_fsm:send_event(self(), board_has_winner(Board)),
    State.

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