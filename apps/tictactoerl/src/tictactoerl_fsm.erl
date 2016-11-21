-module(tictactoerl_fsm).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, % setup/teardown/upgrade
         handle_event/3, handle_sync_event/4, handle_info/3, % otp fsm events
         %% async events
         board_created/2, player_x_turn/2, player_o_turn/2, game_ends/2]).

-include("board_table.hrl"). %% TODO This should be incapsulated in Board module
-record(state, {desc = "",
                board = #board_table{},
                status = "",
                turn = 1,
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

play_turn(CurrentPlayer, Position, State = #state{board = PreviousBoard, turn = 9}) ->
    {_, CurrentBoard} = board:mark_position_if_available(PreviousBoard, Position, CurrentPlayer),
    draw(State#state{board = CurrentBoard});

play_turn(CurrentPlayer, Position, State = #state{board = PreviousBoard, turn = Turn}) ->
    {FreePosition, CurrentBoard} = board:mark_position_if_available(PreviousBoard, Position, CurrentPlayer),

    NextTurn = Turn + 1,
    CurrentState = State#state{board = CurrentBoard, turn = NextTurn},

    {PlayerTurn, MaybePlayerWinner} = case CurrentPlayer of
        "X" -> {player_o_turn, player_x_turn};
        "O" -> {player_x_turn, player_o_turn}
    end,
    case FreePosition of
        true ->
            case board:has_winner(CurrentBoard) of
                game_on -> next_turn(PlayerTurn, CurrentState);
                game_over -> won(CurrentPlayer, CurrentState)
            end;
        false ->
            next_turn(MaybePlayerWinner, CurrentState)
    end.

next_turn(PlayerTurn, State) ->
    Player = case PlayerTurn of
        player_o_turn -> "O";
        player_x_turn -> "X"
    end,
    {next_state, PlayerTurn, prompt(State#state{desc = "\nPlayer "++Player++":\n", status = "Choose position: "})}.

won(Player, State) ->
    {next_state, game_ends, prompt(State#state{desc = "\nPlayer "++Player++":\n", status = "PLAYER "++Player++" WON!"})}.

draw(State) ->
    {next_state, game_ends, prompt(State#state{desc = "\nNo More Turns Left :-)\n", status = "GAME ENDS WITH A DRAW!"})}.

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
