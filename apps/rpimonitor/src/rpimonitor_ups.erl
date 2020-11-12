%%% This module is for the RPI UPS HAT (based on INA219, HY2120 and HY2213):
%%%     https://www.waveshare.net/wiki/UPS_HAT
%%%
%%% The shunt resistor of this board is 0.1 ohms.

-module(rpimonitor_ups).

-behaviour(gen_server).

-export([handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	 code_change/3]).

-export([start_link/0,stop/0]).

-export([get_voltage/0,get_current/0,get_power/0]).

-compile([{nowarn_unused_function,[{read_shunt_voltage_mv,1}]}]).

-include("./rpimonitor_ina219.hrl").

%% the I2C address of this board is 0x42 (A1=GND, A0=SDA)
-define(INA219_I2C_ADDRESS, 16#42).

-define(INA219_DEFAULT_CURRENTLSB, 0.1).
-define(INA219_DEFAULT_POWERLSB, 0.002).
-define(INA219_DEFAULT_CALVALUE, 4096).

%% INA219 configuration for this board
-define(INA219_CONFIGURATION,
	<<0:2,?INA219_VOLTAGE_RANGE_32V:1,?INA219_DIV_8_320MV:2,
	  ?INA219_ADCRES_12BIT_32S:4,?INA219_ADCRES_12BIT_32S:4,
	  ?INA219_MODE_SANDBVOLT_CONTINUOUS:3>>).

handle_call({get,voltage}, _From, #{i2c:=I2C}=State) ->
    V = read_bus_voltage_v(I2C),
    Percent = calc_percent_from_bus_voltage(V),
    {reply,{ok,V,Percent},State};
handle_call({get,current}, _From, #{i2c:=I2C}=State) ->
    V = read_current_a(I2C),
    {reply,{ok,V},State};
handle_call({get,power}, _From, #{i2c:=I2C}=State) ->
    V = read_power_w(I2C),
    {reply,{ok,V},State};

handle_call(stop, _From, State) ->
    {stop,normal,stopped,State}.

handle_cast(_Msg, State) ->
    {noreply,State}.

handle_info(_Info, State) ->
    {noreply,State}.

init([]) ->
    {ok,I2C} = i2c:start_link("i2c-1", ?INA219_I2C_ADDRESS),
    write_calibration(I2C),
    write_config(I2C),
    {ok,#{i2c=>I2C}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.


start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

get_voltage() ->
    gen_server:call(?MODULE, {get,voltage}).

get_current() ->
    gen_server:call(?MODULE, {get,current}).

get_power() ->
    gen_server:call(?MODULE, {get,power}).


write_config(I2C) ->
    i2c:write(I2C, <<?INA219_REG_CONFIG:8,?INA219_CONFIGURATION/binary>>).

write_calibration(I2C) ->
    i2c:write(I2C, <<?INA219_REG_CALIBRATION:8,
		     ?INA219_DEFAULT_CALVALUE:16>>).

calc_percent_from_bus_voltage(BusVoltage) ->
    (BusVoltage - 6) / 2.4.

read_bus_voltage_v(I2C) ->
    ok = write_calibration(I2C),
    N = i2c_read(I2C, ?INA219_REG_BUSVOLTAGE),
    (N bsr 3) * 0.004.

read_current_a(I2C) ->
    N = i2c_read(I2C, ?INA219_REG_CURRENT),
    trunc_num(N) * ?INA219_DEFAULT_CURRENTLSB / 1000.

read_power_w(I2C) ->
    ok = write_calibration(I2C),
    N = i2c_read(I2C, ?INA219_REG_POWER),
    trunc_num(N) * ?INA219_DEFAULT_POWERLSB.

read_shunt_voltage_mv(I2C) ->
    ok = write_calibration(I2C),
    N = i2c_read(I2C, ?INA219_REG_SHUNTVOLTAGE),
    trunc_num(N) * 0.01.

i2c_read(I2C, Address) ->
    ok = i2c:write(I2C, <<Address:8>>),
    <<N:16>> = i2c:read(I2C, 2),
    N.

trunc_num(N) when N > 32767 ->
    N - 65535;
trunc_num(N) ->
    N.

