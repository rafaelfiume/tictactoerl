-module(tictactoerl_fsm).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, % setup/teardown/upgrade
         handle_event/3, handle_sync_event/4, handle_info/3, % otp fsm events
         %% async events
         board_created/2, player_x_turn/2, player_o_turn/2, player_won/2]).

-include("board_table.hrl"). %% TODO This should be incapsulated in Board module
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
    {ok, board_created, start()}.

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
    {next_state, player_x_turn, prompt(S#state{desc = "Game Board Creation...\n",
                                               status = "The game will start with Player X\n"
                                                        "Choose position: "})}.
    
player_x_turn(Position, S=#state{board = PreviousBoard}) ->
    {FreePosition, NewBoard} = board:mark_position_if_available(PreviousBoard, Position, "X"),

    NewState = S#state{board = NewBoard},
    
    case FreePosition of
        true ->
            case board:has_winner(NewBoard) of
                game_on -> {next_state, player_o_turn, prompt(NewState#state{desc = "\nPlayer O:\n", status = "Choose position: "})};
                game_over -> {next_state, player_won, prompt(NewState#state{desc = "\nPlayer X:\n", status = "PLAYER X WON!"})}
            end;
        false ->
            {next_state, player_x_turn, prompt(NewState#state{desc = "\nPlayer X:\n", status = "Choose position: "})}
    end.

player_o_turn(Position, S = #state{board = PreviousBoard}) ->
    {FreePosition, NewBoard} = board:mark_position_if_available(PreviousBoard, Position, "O"),

    NewState = S#state{board = NewBoard},

    case FreePosition of
        true ->
            case board:has_winner(NewBoard) of
                game_on -> {next_state, player_x_turn, prompt(NewState#state{desc = "\nPlayer X:\n", status = "Choose position: "})};
                game_over -> {next_state, player_won, prompt(NewState#state{desc = "\nPlayer O:\n", status = "PLAYER O WON!"})}
            end;
        false ->
            {next_state, player_o_turn, prompt(NewState#state{desc = "\nPlayer O:\n", status = "Choose position: "})}
    end.
    
player_won(_Msg, S) ->
    {stop, shutdown, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     Internals     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
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
    io:format(board:to_string(Board)),
    io:format("~s", [Status]),
    State.

get_input() ->
    Parent = self(),
    spawn_link(fun() ->
        Position = board_position:user_input(io:get_line("")),
        gen_fsm:send_event(Parent, Position)
    end).
