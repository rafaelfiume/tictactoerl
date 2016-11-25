-module(console_input_reader_test).
-include_lib("eunit/include/eunit.hrl").

sends_an_event_to_fsm_when_receiving_input_from_user_test() ->
    Parent = self(),
    UserInput = "1",
    given_that_user_types(UserInput),
    
    assert_that_it_sends_event_with(Parent, UserInput),
    
    whenn(fun() -> console_input_reader:read_user_input(Parent) end).

given_that_user_types(UserInput) ->
    code:unstick_dir(filename:dirname(code:where_is_file("io.beam"))), %% TODO 24/Is it possible to replace code:unstick by a similar meck functionality?
    meck:new(io, [passthrough, no_link]),
    meck:expect(io, get_line, fun(_Prompt) -> UserInput end).

assert_that_it_sends_event_with(ExpectedParent, ExpectedPosition) ->
    meck:new(event_sender),
    meck:expect(event_sender, send_event, fun(ActualParent, ActualPosition) ->
        ?assertEqual(ExpectedParent, ActualParent),
        ?assertEqual(integer_to_list(ActualPosition), ExpectedPosition)
    end).

whenn(FunctionCall) ->
    FunctionCall(),
    meck:unload(event_sender),
    meck:unload(io).