-module(rpimonitor_ui).
-export([handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([start_link/0, stop/0]).
-behaviour(gen_server).

get_current_info() ->
	{ok, _, VoltagePercent} = rpimonitor_ups:get_voltage(),
	{ok, Temp} = rpimonitor_temp:get_temp(),
	Info = io_lib:format(
		"CPU Temperature: ~.2f'C, Battery Power: ~.2f%",
		[Temp, VoltagePercent * 100]
	),
	lists:flatten(Info).

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(refresh, #{infotxt := Text} = State) ->
	wxStaticText:setLabel(Text, get_current_info()),
	erlang:send_after(1000, ?MODULE, refresh),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

init([]) ->
	Wx = wx:new(),
	Frm = wxFrame:new(
		Wx,
		-1,
		"Rpi Monitor",
		[{pos, {1500, 0}}, {size, {360, 20}}]
	),
	Text = wxStaticText:new(Frm, 100, "no data"),
	wxFrame:show(Frm),
	erlang:send_after(1000, ?MODULE, refresh),
	{ok, #{frm => Frm, infotxt => Text}}.

terminate(_Reason, #{frm := Frm}) ->
	ok = wxFrame:destroy(Frm),
	ok.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

