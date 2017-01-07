-module(bot_console_input_reader).

-export([read_user_input/1]).

-spec read_user_input(pid()) -> 'ok'.
read_user_input(Parent) ->
    wait_for_your_time_to_play(),

    Position = random_positions:next(),
    event_sender:send_event(Parent, Position).

wait_for_your_time_to_play() ->
    {ok, TurnDuration} = application:get_env(tictactoerl, turn_duration_in_seconds),
    timer:sleep(TurnDuration * 1000).