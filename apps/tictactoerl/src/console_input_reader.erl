-module(console_input_reader).

-export([read_user_input/1]).

-spec read_user_input(pid()) -> 'ok'.
read_user_input(Parent) ->
    Position = board_position:user_input(io:get_line("")),
    event_sender:send_event(Parent, Position).
