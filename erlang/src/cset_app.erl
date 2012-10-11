-module(cset_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Dispatch = [
		{'_', [
			{[], cset_handler, []}
		]}
	],
	{ok, _} = cowboy:start_listener(cset_websocket_listener, 100,
		cowboy_tcp_transport, [{port, 1337}],
    	cowboy_http_protocol, [{dispatch, Dispatch}]
    ),
    cset_sup:start_link().

stop(_State) ->
    ok.
