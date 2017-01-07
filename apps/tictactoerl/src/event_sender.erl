-module(event_sender).

-export([send_event/2]).

-spec send_event(pid(), board:position()) -> 'ok'.
send_event(Parent, Position) ->
    gen_fsm:send_event(Parent, Position).