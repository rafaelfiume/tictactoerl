-module(tictactoerl_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [board_created].

init_per_testcase(_, Config) ->
    mock_io(),
    {ok, Pid} = tictactoerl_fsm:start_link(),
    [{pid, Pid} | Config].

end_per_testcase(_, Config) ->
    meck:unload(io),
    Pid = ?config(pid, Config),
    unlink(Pid),
    exit(Pid, shutdown),
    wait_for_death(Pid).

mock_io() ->
    Parent = self(),
    code:unstick_dir(filename:dirname(code:where_is_file("io.beam"))),
    meck:new(io, [passthrough, no_link]),
    meck:expect(io, format, fun (Str) ->
        Parent ! {out, Str},
        ok
    end),
    meck:expect(io, format, fun (Str, Args) ->
        Parent ! {out, lists:flatten(io_lib:format(Str, Args))}
    end),
    meck:expect(io, get_line, fun (_Prompt) ->
        Parent ! {in, self()},
        receive {Parent, In} -> In end
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

board_created(_Config) ->
    out("Game Board Creation...\n"),
    out("   |   |   ~n"
        "---+---+---~n"
        "   |   |   ~n"
        "---+---+---~n"
        "   |   |   ~n"),
    out("The game will start with Player X\n"
        "Choose position:").

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

in(Input) ->
    receive
        {in, Pid} -> Pid ! {self(), Input}
    after 1000 ->
        ct:pal("MBOX: ~p", [process_info(self(), messages)]),
        error({too_long, {in, Input}})
    end.

%% fuzzily match the input string, waiting 1s at most
out(Expected) ->
    receive
        {out, Prompt} ->
            ct:pal("Expected: ~s~nPrompt  : ~s", [Expected, Prompt]),
            true = string:equal(Expected, Prompt)

    after 1000 ->
        ct:pal("MBOX: ~p", [process_info(self(), messages)]),
        error({too_long, {out, Expected}})
    end.
