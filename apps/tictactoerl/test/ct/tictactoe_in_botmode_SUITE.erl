-module(tictactoe_in_botmode_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [
     computer_does_all_the_playing_when_tictactoe_is_in_bot_mode
    ].

init_per_testcase(_, Config) ->
    mock_io(),
    application:set_env(tictactoerl, botmode, on), %% BOTMODE ON!! %%
    application:set_env(tictactoerl, turn_duration_in_seconds, 0),
    {ok, Pid} = tictactoerl_fsm:start_link(),
    [{pid, Pid} | Config].

end_per_testcase(_, Config) ->
    meck:unload(io),
    Pid = ?config(pid, Config),
    unlink(Pid),
    exit(Pid, shutdown),
    wait_for_death(Pid).

mock_io() ->
    code:unstick_dir(filename:dirname(code:where_is_file("io.beam"))),
    meck:new(io, [passthrough, no_link]),
    Parent = self(),
    meck:expect(io, format, fun(Str) ->
        Parent ! {out, Str},
        ok
    end),
    meck:expect(io, format, fun(Str, Args) ->
        Parent ! {out, lists:flatten(io_lib:format(Str, Args))}
    end).

wait_for_death(Pid) ->
    case is_process_alive(Pid) of
        true -> 
            timer:sleep(10),
            wait_for_death(Pid);
        false ->
            ok
    end.

%%%%%%%%%%%%%%%%%%
%%% TEST CASES %%%
%%%%%%%%%%%%%%%%%%

computer_does_all_the_playing_when_tictactoe_is_in_bot_mode(_Config) ->
    %% given app is up and running with botmode on 

    game_ends_with(either("PLAYER X WON!"), orr("PLAYER O WON!"), orr("GAME ENDS WITH A DRAW!")).

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

%% fuzzily match the input string, waiting 1s at most
game_ends_with(Expected1, Expected2, Expected3) ->
    receive
        {out, Prompt} ->
            %ct:pal("Expected:~n~s~nPrompt:~n~s", [Expected, Prompt]),
            ct:pal("Prompt is: ~n~s~n", [Prompt]),
            case string:equal(Expected1, Prompt) orelse string:equal(Expected2, Prompt) orelse string:equal(Expected3, Prompt) of
                true -> ct:pal("trying again........");
                false -> game_ends_with(Expected1, Expected2, Expected3)
            end

    after 1000 ->
        ct:pal("MBOX: ~p", [process_info(self(), messages)]),
        error({too_long, {out, Expected1, Expected2, Expected3}})
    end.

either(Stuff) -> Stuff.

orr(Stuff) -> Stuff.