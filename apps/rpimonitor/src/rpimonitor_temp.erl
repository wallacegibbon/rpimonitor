-module(rpimonitor_temp).

-behaviour(gen_server).

-export([handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	 code_change/3]).

-export([start_link/0,stop/0]).

-export([get_temp/0]).

-define(TEMP_FILE, "/sys/class/thermal/thermal_zone0/temp").

handle_call(get_temp, _From, #{f:=F}=State) ->
    {ok,0} = file:position(F, 0),
    {ok,TempStr} = file:read(F, 20),
    {reply,{ok,list_to_integer(string:chomp(TempStr))/1000},State};
handle_call(stop, _From, State) ->
    {stop,normal,stopped,State}.

handle_cast(_Msg, State) ->
    {noreply,State}.

handle_info(_Info, State) ->
    {noreply,State}.

init([]) ->
    {ok,F} = file:open(?TEMP_FILE, [read]),
    {ok,#{f=>F}}.

terminate(_Reason, #{f:=F}) ->
    ok = file:close(F),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.


start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

get_temp() ->
    gen_server:call(?MODULE, get_temp).

