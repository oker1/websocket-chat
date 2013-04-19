-module(cset_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


-define(SERVER, ?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = [
    {'_', [
      {[], cset_handler, []}
    ]}
  ],

  Port = case application:get_env(cset, port) of
    undefined   -> 1337;
    {ok, P}     -> P
  end,

  Nodes = case application:get_env(cset, nodes) of
    undefined   -> [];
    {ok, ''}    -> [];
    {ok, N}     -> [N]
  end,

  {ok, _} = cowboy:start_listener(cset_websocket_listener, 100,
    cowboy_tcp_transport, [{port, Port}],
    cowboy_http_protocol, [{dispatch, Dispatch}]
  ),

  lager:start(),
  cset_server:start_link({nodes, Nodes}).

stop(_State) ->
    ok.
