-module(tictactoerl_fsm).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, % setup/teardown/upgrade
         handle_event/3, handle_sync_event/4, handle_info/3, % global events
         %% only async events
         board_created/2, player_x_turn/2]).

-record(state, {desc="",
                board = "",
                status = ""}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_FSM CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    gen_fsm:send_event(self(), olamundo),
    {ok, board_created, #state{}}.

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
    {stop, normal, prompt(State#state{desc = "Game Board Creation...", 
                                      status = "The game will start with Player X\n"
                                               "Choose position:"})}.

player_x_turn(_Msg, State) ->
    {stop, normal, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     Internals     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

prompt(State = #state{desc = Desc, status = Status}) ->
    io:format("~s~n", [Desc]),
    io:format("   |   |   ~n"
              "---+---+---~n"
              "   |   |   ~n"
              "---+---+---~n"
              "   |   |   ~n"),
    io:format("~s", [Status]),
    State.
