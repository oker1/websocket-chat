-module(websocket_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3, websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, _Opts) ->
  cset_server:register(self()),
  Req2 = cowboy_req:compact(Req),
  {ok, Req2, undefined_state, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
  cset_server:incomingMessageFromClient(Msg, self()),
  {ok, Req, State};

websocket_handle(_Any, Req, State) ->
  {ok, Req, State}.

websocket_info({outgoingMessage, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.