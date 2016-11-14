-module(tictactoerl_fsm).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, % setup/teardown/upgrade
         handle_event/3, handle_sync_event/4, handle_info/3, % global events
         %% only async events
         board_created/2, player_x_turn/2, player_o_turn/2,
         %% in progress...
         board_to_string/1]).

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

player_x_turn(_Msg, State) ->
    {next_state, player_o_turn, prompt(State#state{desc = "\nPlayer X:\n",
                                                   status = "Choose position:"})} .
    


player_o_turn(_Msg, State) ->
    %{next_state, player_x_turn, prompt(State#state{desc = "Player O:\n",
    %                                               status = "Choose position:"})}.
    {stop, normal, State}. 

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     Internals     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

prompt() -> %% actually not prompting, just sending an event
    gen_fsm:send_event(self(), olamundo),
    #state{}.
prompt(State = #state{desc = Desc, board = Board, status = Status}) ->
    io:format("~s", [Desc]),
    io:format(board_to_string(Board)),
    io:format("~s", [Status]),
    gen_fsm:send_event(self(), olamundo),
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
