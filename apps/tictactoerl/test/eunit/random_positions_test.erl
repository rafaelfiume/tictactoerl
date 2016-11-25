-module(random_positions_test).
-include_lib("eunit/include/eunit.hrl").

genetates_9_different_numbers_randomly_one_time_each_from_1_to_9_test() ->
    ListOfNumbers = [2, 3, 5, 9, 6, 1, 8], % missing 4 and 7

    FinalListOfNumbers = random_positions:next(
        random_positions:next(ListOfNumbers)
    ),

    ?assertEqual(9, length(FinalListOfNumbers)),
    ?assertEqual([1,2,3,4,5,6,7,8,9], lists:sort(FinalListOfNumbers)).