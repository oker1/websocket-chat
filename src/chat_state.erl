-module(chat_state).

-export([initial/0, add_connecting_user/2, remove_user/2, history/1, set_history/2,
  assign_color_to_user/3, user_color/2]).

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

assign_color_to_user(State, Pid, Username) ->
  ColorCounter = State#state.colorCounter,
  Color = pick_color(ColorCounter),
  NewColorCounter = ColorCounter + 1,

  NewClientState = {connectedUser, {name, Username}, {color, Color}},
  NewClientDict = dict:store(Pid, NewClientState, State#state.clients),

  State#state{clients = NewClientDict, colorCounter = NewColorCounter}.

user_color(State, Pid) ->
  {ok, {connectedUser, {name, _Name}, {color, Color}}} = dict:find(Pid, State#state.clients),

  Color.

pick_color(ColorCounter) ->
  Colors = ["red", "green", "blue", "magenta", "purple", "plum", "orange"],

  lists:nth(1 + ColorCounter rem length(Colors), Colors).