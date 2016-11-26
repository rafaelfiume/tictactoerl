-module(random_positions_test).
-include_lib("eunit/include/eunit.hrl").

receives_a_random_number_from_one_to_nine_test() ->
    Pid = random_positions:new(),

    FirstResult = random_positions:next(Pid),
    SecondResult = random_positions:next(Pid),

    ?assert(1 =< FirstResult),  ?assert(FirstResult =< 9),
    ?assert(1 =< SecondResult), ?assert(SecondResult =< 9),
    ?assert(FirstResult =/= SecondResult).

% TODO : 26/11/2016 : It fills we're testing the internals here...
genetates_9_different_numbers_randomly_one_time_each_from_1_to_9_test() ->
    ListOfNumbers = [2, 3, 5, 9, 6, 1, 8], % missing 4 and 7

    ARandomNumber = random_positions:next(ListOfNumbers),
    AnotherRandomNumber = random_positions:next([ARandomNumber|ListOfNumbers]),

    MissingNumbers = [ARandomNumber, AnotherRandomNumber],
    ?assertEqual(2, length(MissingNumbers)),
    ?assertEqual([4, 7], lists:sort(MissingNumbers)).
