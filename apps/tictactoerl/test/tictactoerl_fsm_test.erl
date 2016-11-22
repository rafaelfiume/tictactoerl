-module(tictactoerl_fsm_test).
-include_lib("eunit/include/eunit.hrl").

-include("../src/board_table.hrl").

% State copied like done here => http://learnyousomeerlang.com/static/erlang/processquest/apps/processquest-1.1.0/test/pq_player_tests.erl
% Is this the best approach?
-record(state, {desc = "", % copied from tictactoerl_fsm.erl
                board = #board_table{},
                status = "",
                turn = 1,
                pid}).

does_not_update_turn_number_when_player_chooses_an_invalid_position_test() ->
    State = #state{},

    gen_fsm:send_event(self(), 1),
    {next_state, _PlayerTurn, NewState} = tictactoerl_fsm:player_x_turn(unknown_position, State),

    ?assertEqual(1, NewState#state.turn).
