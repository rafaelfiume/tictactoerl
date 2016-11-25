-module(event_sender).

-export([send_event/2]).

send_event(Parent, Position) ->
    gen_fsm:send_event(Parent, Position).