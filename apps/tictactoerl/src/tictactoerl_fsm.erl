-module(tictactoerl_fsm).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, % setup/teardown/upgrade
         handle_event/3, handle_sync_event/4, handle_info/3, % otp fsm events
         %% async events
         board_created/2, player_x_turn/2, player_o_turn/2, game_ends/2]).

-include("board.hrl").

-type turn()     :: 1..9.

-record(state, {desc             :: string(),
                board = #board{} :: board:board(),
                status           :: string(),
                turn = 1         :: turn(),
                pid              :: pid()}).

-define(MAX_TURNS, 9).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_FSM CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    random_positions:new(),
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
    init:stop(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%       States      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

board_created(_Msg, State) -> 
    {next_state, player_x_turn, prompt(State#state{desc = "Game Board Creation...\n",
                                                   status = "The game will start with Player X\n"
                                                            "Choose position: "})}.

player_x_turn(Position, State) ->
    play_turn("X", Position, State).

player_o_turn(Position, State) ->
    play_turn("O", Position, State).
    
game_ends(_Msg, S) ->
    {stop, shutdown, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     Internals     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    gen_fsm:send_event(self(), game_on),
    #state{}.

play_turn(CurrentPlayer, Position, State = #state{board = PreviousBoard, turn = ?MAX_TURNS}) ->
    {_, CurrentBoard} = board:mark_position_if_available(PreviousBoard, Position, CurrentPlayer),

    CurrentState = State#state{board = CurrentBoard},

    do_if_has_winner_or_else(CurrentBoard, 
                             fun() -> won(CurrentPlayer, CurrentState) end,
                             fun() -> draw(CurrentState) end);
play_turn(CurrentPlayer, Position, State = #state{board = PreviousBoard, turn = Turn}) ->
    {FreePosition, CurrentBoard} = board:mark_position_if_available(PreviousBoard, Position, CurrentPlayer),

    CurrentState = State#state{board = CurrentBoard},

    case FreePosition of
        true ->
            do_if_has_winner_or_else(CurrentBoard, 
                                     fun() -> won(CurrentPlayer, CurrentState) end,
                                     fun() -> change_turn(CurrentPlayer, CurrentState#state{turn = Turn + 1}) end);
        false ->
            replay_turn(CurrentPlayer, CurrentState)
    end.

do_if_has_winner_or_else(Board, HasWinner, GameIsStillOn) ->
    case board:has_winner(Board) of
        game_over -> HasWinner();
        game_on -> GameIsStillOn()
    end.

change_turn(CurrentPlayer, State) ->
    {NextPlayer, NextFsmState} = case CurrentPlayer of
        "X" -> {"O", player_o_turn};
        "O" -> {"X", player_x_turn}
    end,
    next_turn(NextPlayer, NextFsmState, State).

replay_turn(CurrentPlayer, State) ->
     NextFsmState = case CurrentPlayer of
        "X" -> player_x_turn;
        "O" -> player_o_turn
    end,
    next_turn(CurrentPlayer, NextFsmState, State).

next_turn(Player, FsmState, State) ->
    {next_state, FsmState, prompt(State#state{desc = "\nPlayer "++Player++":\n", status = "Choose position: "})}.

won(Player, State) ->
    {next_state, game_ends, prompt(State#state{desc = "\nPlayer "++Player++":\n", status = "PLAYER "++Player++" WON!"})}.

draw(State) ->
    {next_state, game_ends, prompt(State#state{desc = "\nNo More Turns Left :-)\n", status = "GAME ENDS WITH A DRAW!"})}.

prompt(State = #state{pid = undefined}) ->
    output(State),
    State#state{pid = get_input()};
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
    % TODO : 26/11/2016 : Move the logic to get the rigth input_reader to another module?
    InputFunction = case application:get_env(tictactoerl, botmode) of
        {ok, on} -> fun() -> bot_console_input_reader:read_user_input(Parent) end;
        _ -> fun() -> console_input_reader:read_user_input(Parent) end
    end,
    spawn_link(InputFunction).

