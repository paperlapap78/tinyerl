-module(tinyerl).

-include("../include/url.hrl").
-define(SERVER, ?MODULE).
-behaviour(gen_server).
-export([start_link/0, stop/0, encode/1, decode/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

encode(Url) ->
    gen_server:call(?SERVER, {encode, Url}).

decode(Code) ->
    gen_server:call(?SERVER, {decode, Code}).

init([]) ->
    error_logger:info_report({"initializing sherl"}),
    sherl_db:start([]),
    error_logger:info_report({"sherl_db started"}),
    {ok, #state{}).

handle_call({decode, Code}, _From, State) ->
    case sherl_db:get_url(base62:decode(Code)) of
        undefined ->
            {reply, {not_found, Code}, State};
        UrlRec ->
            {reply, {ok, UrlRec#url.url}, State}
    end;
handle_call({encode, Url}, _From, State) ->
    Rec = sherl_db:get_code(Url),
    Code = base62:encode(Rec#url.code),
    {reply, {ok, Code}, State};
handle_call(stop, _From, State) ->
    sherl_db:stop(),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
