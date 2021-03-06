-module(tictactoe_has_winner_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [
     player_wins_with__vertical__line
     ,
     player_wins_with__horizontal__line
     ,
     player_wins_with__diagonal__line
     ,
     game_ends_with_a_draw_when_players_cant_get_three_in_a_row
     ,
     player_has_to_select_a__free__position_in_the_board
     ,
     player_has_to_select_a__valid__position_in_the_board
    ].

init_per_testcase(_, Config) ->
    mock_io(),
    application:set_env(tictactoerl, botmode, off),
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
    end),
    meck:expect(io, get_line, fun(_Prompt) ->
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

board_created() ->
    out("Game Board Creation...\n"),
    out("   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("The game will start with Player X\n"
        "Choose position: ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  TEST CASES  - WINNING \O/  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player_wins_with__vertical__line(_Config) ->
    board_created(),
    in("1"),

    out("\nPlayer O:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("5"),

    out("\nPlayer X:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   | O |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("3"),

    out("\nPlayer O:\n"),
    out(" X |   | X \n"
        "---+---+---\n"
        "   | O |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("2"),

    out("\nPlayer X:\n"),
    out(" X | O | X \n"
        "---+---+---\n"
        "   | O |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("7"),

    out("\nPlayer O:\n"),
    out(" X | O | X \n"
        "---+---+---\n"
        "   | O |   \n"
        "---+---+---\n"
        " X |   |   \n"),
    out("Choose position: "), in("8"),

    out("\nPlayer O:\n"),
    out(" X | O | X \n"
        "---+---+---\n"
        "   | O |   \n"
        "---+---+---\n"
        " X | O |   \n"),
    out("PLAYER O WON!").

player_wins_with__horizontal__line(_Config) ->
    board_created(),
    in("1"),

    out("\nPlayer O:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("5"),

    out("\nPlayer X:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   | O |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("2"),

    out("\nPlayer O:\n"),
    out(" X | X |   \n"
        "---+---+---\n"
        "   | O |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("4"),

    out("\nPlayer X:\n"),
    out(" X | X |   \n"
        "---+---+---\n"
        " O | O |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("3"),

    out("\nPlayer X:\n"),
    out(" X | X | X \n"
        "---+---+---\n"
        " O | O |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("PLAYER X WON!").

player_wins_with__diagonal__line(_Config) ->
    board_created(),
    in("1"),

    out("\nPlayer O:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("2"),

    out("\nPlayer X:\n"),
    out(" X | O |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("5"),

    out("\nPlayer O:\n"),
    out(" X | O |   \n"
        "---+---+---\n"
        "   | X |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("3"),

    out("\nPlayer X:\n"),
    out(" X | O | O \n"
        "---+---+---\n"
        "   | X |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("9"),

    out("\nPlayer X:\n"),
    out(" X | O | O \n"
        "---+---+---\n"
        "   | X |   \n"
        "---+---+---\n"
        "   |   | X \n"),
    out("PLAYER X WON!").

%%%%%%%%%%%%%%%%%%%
%%%   DRAW oO   %%%
%%%%%%%%%%%%%%%%%%%

game_ends_with_a_draw_when_players_cant_get_three_in_a_row(_Config) ->
    board_created(),
    in("1"),

    out("\nPlayer O:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("5"),

    out("\nPlayer X:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   | O |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("4"),

    out("\nPlayer O:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        " X | O |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("7"),

    out("\nPlayer X:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        " X | O |   \n"
        "---+---+---\n"
        " O |   |   \n"),
    out("Choose position: "), in("3"),

    out("\nPlayer O:\n"),
    out(" X |   | X \n"
        "---+---+---\n"
        " X | O |   \n"
        "---+---+---\n"
        " O |   |   \n"),
    out("Choose position: "), in("9"),

    out("\nPlayer X:\n"),
    out(" X |   | X \n"
        "---+---+---\n"
        " X | O |   \n"
        "---+---+---\n"
        " O |   | O \n"),
    out("Choose position: "), in("8"),

    out("\nPlayer O:\n"),
    out(" X |   | X \n"
        "---+---+---\n"
        " X | O |   \n"
        "---+---+---\n"
        " O | X | O \n"),
    out("Choose position: "), in("2"),

    out("\nPlayer X:\n"),
    out(" X | O | X \n"
        "---+---+---\n"
        " X | O |   \n"
        "---+---+---\n"
        " O | X | O \n"),
    out("Choose position: "), in("6"),

    out("\nNo More Turns Left :-)\n"),
    out(" X | O | X \n"
        "---+---+---\n"
        " X | O | X \n"
        "---+---+---\n"
        " O | X | O \n"),
    out("GAME ENDS WITH A DRAW!").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   TEST CASES  - SAD PATH    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player_has_to_select_a__free__position_in_the_board(_Config) ->
    board_created(),
    in("1"),

    out("\nPlayer O:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("1"),

    out("\nPlayer O:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("2"),

    out("\nPlayer X:\n"),
    out(" X | O |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("2"),

    out("\nPlayer X:\n"),
    out(" X | O |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: ").

player_has_to_select_a__valid__position_in_the_board(_Config) ->
    board_created(),
    in("w"),

    out("\nPlayer X:\n"),
    out("   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("1"),

    out("\nPlayer O:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: "), in("99"),

    out("\nPlayer O:\n"),
    out(" X |   |   \n"
        "---+---+---\n"
        "   |   |   \n"
        "---+---+---\n"
        "   |   |   \n"),
    out("Choose position: ").

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
            ct:pal("Expected:~n~s~nPrompt:~n~s", [Expected, Prompt]),
            true = string:equal(Expected, Prompt)

    after 1000 ->
        ct:pal("MBOX: ~p", [process_info(self(), messages)]),
        error({too_long, {out, Expected}})
    end.
