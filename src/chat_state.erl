-module(chat_state).

-export([initial/0, add_connecting_user/2, remove_user/2, history/1, set_history/2]).

-include("chat_state.hrl").

initial() ->
  #state{clients=dict:new(), colorCounter=0, history=[]}.

add_connecting_user(State, Pid) ->
  ClientDict = State#state.clients,
  State#state{clients = dict:store(Pid, {connectingUser}, ClientDict)}.

remove_user(State, Pid) ->
  ClientDict = State#state.clients,
  State#state{clients = dict:erase(Pid, ClientDict)}.

history(State) ->
  State#state.history.

set_history(State, NewHistory) ->
  State#state{history = NewHistory}.