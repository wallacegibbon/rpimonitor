%%%-------------------------------------------------------------------
%% @doc rpimonitor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rpimonitor_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

child(Module) ->
    {Module,{Module,start_link,[]},permanent,10000,worker,[Module]}.

child_list() ->
    Mods = [rpimonitor_ups,rpimonitor_temp,rpimonitor_ui],
    lists:map(fun child/1, Mods).

init([]) ->
    {ok,{{one_for_all,0,1},child_list()}}.

start_link() ->
    supervisor:start_link({local,?SERVER}, ?MODULE, []).

