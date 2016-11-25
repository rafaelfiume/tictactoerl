-module(console_input_reader).

-export([read_user_input/1]).

read_user_input(Parent) ->
    Position = board_position:user_input(io:get_line("")),
    event_sender:send_event(Parent, Position).
