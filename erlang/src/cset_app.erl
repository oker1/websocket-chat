-module(cset_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


-define(SERVER, ?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Port = case application:get_env(cset, port) of
    undefined   -> 1337;
    {ok, P}     -> P
  end,

  Nodes = case application:get_env(cset, nodes) of
    undefined   -> [];
    {ok, ''}    -> [];
    {ok, N}     -> [N]
  end,

  Dispatch = [
    {'_', [
      {"/",          http_handler,      {port, Port}},
      {"/websocket", websocket_handler, {port, Port}}
    ]}
  ],

  {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
    [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}]),

  lager:start(),
  cset_server:start_link({nodes, Nodes}).

stop(_State) ->
    ok.
