-module(sherl_db_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


passing_test() ->
    ?assert(true).

failing_test() ->
    ?assert(false).
