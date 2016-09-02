-module(sherl_db_SUITE).

-compile(export_all).
-include("../include/url.hrl").
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{seconds,10}}].

init_per_suite(Config) ->
    sherl_db:start([]),
    Config.

end_per_suite(_Config) ->
    sherl_db:stop(),
    ok.

all() ->
    [unknown_url_is_undefined, roundtrip1, roundtrip2, concurrent_creating].

%% Test cases start here.


unknown_url_is_undefined(_Config) ->
    undefined = sherl_db:get_url("no-such-url-in-db"),
    ok.

roundtrip1(_Config) ->
    Url1 = "url1",
    Ans1 = sherl_db:get_code(Url1),

    Url1 = Ans1#url.url,
    1 = Ans1#url.code,
    Ans1 = sherl_db:get_url(Ans1#url.code),
    %% same url yields same record
    Ans1 = sherl_db:get_code(Url1),
    ok.

roundtrip2(_Config) ->
    Url2 = "url2",
    Ans2 = sherl_db:get_code(Url2),
    Url2 = Ans2#url.url,
    2 = Ans2#url.code,
    Ans2 = sherl_db:get_url(Ans2#url.code),
    ok.

concurrent_creating(_Config) ->
    NumClients = 5,
    Seq = lists:map(fun erlang:integer_to_list/1, lists:seq(1, 10)),
    Parent = self(),
    F = fun() ->
                Codes = lists:map(fun(N) ->
                                          sherl_db:get_code("http://" ++ N)
                                  end,
                                  Seq),
                    Parent ! {self(), Codes}
        end,
    Pids = lists:map(fun(_x) ->
                             spawn(F) end, lists:seq(1, NumClients)),
    Results = [simple_gather(Pid) || Pid <- Pids ],
    Codes = [ X#url.code || X <- hd(Results) ],
    ExpectedCodes = lists:seq(3,12),
    lists:foreach(fun(L) ->
                          ExpectedCodes = [X#url.code || X <- L]
                  end,
                  Results),
    ok.

simple_gather(Pid) ->
    receive
        {Pid, Val} ->
            Val
    end.
