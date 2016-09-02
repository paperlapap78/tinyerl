%% supervisor

-module(tinyerl_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Tinyerl = {tinyerl_main, {tinyerl, start_link, []},
               permanent, 2000, worker, [tinyerl]},
    {ok, {{one_for_one, 20, 60}, [Tinyerl]}}.
