-module(cset_server).

-behaviour(gen_server).

%% API
-export([register/1, unregister/1, chatMessage/2]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {clients, colorCounter}).

%%%===================================================================
%%% API
%%%===================================================================

register(Pid) ->
    gen_server:cast(?SERVER, {register, Pid}).

unregister(Pid) ->
    gen_server:cast(?SERVER, {unregister, Pid}).

chatMessage(Msg, Pid) ->
    gen_server:cast(?SERVER, {chatMessage, Msg, Pid}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{clients=dict:new(), colorCounter=0}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({register, Pid}, #state{clients = ClientDict} = State) ->
    State2 = State#state{clients = dict:store(Pid, {new}, ClientDict)},
    {noreply, State2};

handle_cast({unregsiter, Pid}, #state{clients = ClientDict} = State) ->
    State2 = State#state{clients = dict:erase(Pid, ClientDict)},
    {noreply, State2};

handle_cast({chatMessage, Msg, Pid}, #state{clients = ClientDict, colorCounter = ColorCounter} = State) ->
    {ok, ClientState} = dict:find(Pid, ClientDict),

    NewClientState = case ClientState of
        {new}       -> {loggedInUser, {name, Msg}, {color, pick_color(ColorCounter)}};
        OtherState  -> OtherState
    end,

    NewColorCounter = case ClientState of
        {new} -> ColorCounter + 1;
        _     -> ColorCounter
    end,

    case ClientState of
        {new} -> handle_colormessage(NewClientState, Pid);
        _     -> handle_chatmessage(NewClientState, Msg, ClientDict)
    end,

    % TODO: store only if changed
    NewClientDict = dict:store(Pid, NewClientState, ClientDict),

    {noreply, #state{clients = NewClientDict, colorCounter = NewColorCounter}}.

pick_color(ColorCounter) ->
    Colors = ["red", "green", "blue", "magenta", "purple", "plum", "orange"],

    lists:nth(1 + ColorCounter rem length(Colors), Colors).

%handle_cast(_Msg, State) ->
%    {noreply, State}.

handle_colormessage(ClientState, Pid) ->
    {loggedInUser, {name, Username}, {color, Color}} = ClientState,
    Json = encode_colormessage(Color),

    Pid ! {chatMessage, Json}.

handle_chatmessage(ClientState, Msg, ClientDict) ->
    {loggedInUser, {name, Username}, {color, Color}} = ClientState,
    Json = encode_chatmessage(Color, Username, Msg),

    lists:foreach(fun ({Pid, {loggedInUser, {name, Name}, {color, Color}}}) ->
              Pid ! {chatMessage, Json}
          end, dict:to_list(ClientDict)).

encode_chatmessage(Color, Username, Message) ->
    {Mega, Secs, _} = os:timestamp(),
    Timestamp = Mega * 1000000 + Secs,
    ChatMessage = {[{type, <<"message">>}, {data, {[{time, Timestamp}, {text, Message}, {author, Username}, {color, list_to_binary(Color)}]}}]},
    {ok, Json} = json:encode(ChatMessage),

    Json.

encode_colormessage(Color) ->
    ColorMessage  = {[{type, <<"color">>}, {data, list_to_binary(Color)}]},
    {ok, Json} = json:encode(ColorMessage),

    Json.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.