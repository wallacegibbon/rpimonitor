%%%-------------------------------------------------------------------
%% @doc rpimonitor public API
%% @end
%%%-------------------------------------------------------------------

-module(rpimonitor_app).

-behaviour(application).

-export([start/2,stop/1]).

start(_StartType, _StartArgs) ->
    rpimonitor_sup:start_link().

stop(_State) ->
    ok.

