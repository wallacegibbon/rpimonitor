-module(rpimonitor_ina219).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2,
	 code_change/3]).

-export([start_link/0, stop/0]).

-export([initialize/1, get_bus_voltage/0]).

-include("./rpimonitor_ina219.hrl").


handle_call({initialize, Mode}, _From, #{iic := IIc} = State) ->
    {reply, ok, State};
handle_call({get, bus_voltage}, _From, #{iic := IIc} = State) ->
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

init([]) ->
    {ok, IIc} = i2c:start_link("i2c-1", ?INA219_IIC_ADDRESS),
    {ok, #{iic => IIc}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

initialize(Mode) ->
    gen_server:call(?MODULE, {initialize, Mode}).

get_bus_voltage() ->
    gen_server:call(?MODULE, {get, bus_voltage}).

