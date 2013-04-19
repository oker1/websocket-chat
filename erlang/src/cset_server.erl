-module(cset_server).

-behaviour(gen_server).

%% API
-export([register/1, unregister/1, chatMessage/2, chatMessageFromNode/1]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {clients, colorCounter, history}).

-compile([{parse_transform, lager_transform}]).

%%%===================================================================
%%% API
%%%===================================================================

register(Pid) ->
    gen_server:cast(?SERVER, {register, Pid}).

unregister(Pid) ->
    gen_server:cast(?SERVER, {unregister, Pid}).

chatMessage(Msg, Pid) ->
    gen_server:cast(?SERVER, {chatMessage, Msg, Pid}).

chatMessageFromNode(Msg) ->
    gen_server:cast(?SERVER, {chatMessageFromNode, Msg}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{clients=dict:new(), colorCounter=0, history=[]}}.

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
handle_cast({register, Pid}, #state{clients = ClientDict, history = History} = State) ->
    State2 = State#state{clients = dict:store(Pid, {connectingUser}, ClientDict)},

    {ok, Json} = json:encode(build_historyreply(lists:reverse(History))),
    Pid ! {chatMessage, Json},

    {noreply, State2};

handle_cast({unregsiter, Pid}, #state{clients = ClientDict} = State) ->
    State2 = State#state{clients = dict:erase(Pid, ClientDict)},
    {noreply, State2};

handle_cast({chatMessage, RawMessage, Pid}, #state{clients = ClientDict, colorCounter = ColorCounter, history = History}) ->
    {ok, ClientState} = dict:find(Pid, ClientDict),

    {NewColorCounter, NewHistory, NewClientDict} = case ClientState of
        {connectingUser} -> handle_color(RawMessage, ColorCounter, History, Pid, ClientDict);
        OtherState       -> handle_chat(OtherState, ColorCounter, RawMessage, ClientDict, History)
    end,

    {noreply, #state{clients = NewClientDict, colorCounter = NewColorCounter, history = NewHistory}};

handle_cast({chatMessageFromNode, RawMessage}, #state{clients = ClientDict, colorCounter = ColorCounter, history = History} = State) ->
    broadcast_chatmessage(RawMessage, ClientDict),

    {noreply, State}.

pick_color(ColorCounter) ->
    Colors = ["red", "green", "blue", "magenta", "purple", "plum", "orange"],

    lists:nth(1 + ColorCounter rem length(Colors), Colors).

handle_color(Username, ColorCounter, History, Pid, ClientDict) ->
    NewClientState = {connectedUser, {name, Username}, {color, pick_color(ColorCounter)}},
    NewColorCounter = ColorCounter + 1,
    NewClientDict = dict:store(Pid, NewClientState, ClientDict),

    handle_colormessage(NewClientState, Pid),

    {NewColorCounter, History, NewClientDict}.

handle_chat(ClientState, ColorCounter, ChatMessage, ClientDict, History) ->
    MessageReply = handle_chatmessage(ClientState, ChatMessage, ClientDict),

    {[{type, _}, {data, MessageStructure}]} = MessageReply,

    NewHistory = [ MessageStructure | History ],

    {ColorCounter, NewHistory, ClientDict}.

handle_colormessage(ClientState, Pid) ->
    {connectedUser, {name, _}, {color, Color}} = ClientState,
    ColorReply = build_colorreply(Color),
    {ok, Json} = json:encode(ColorReply),

    Pid ! {chatMessage, Json}.

handle_chatmessage(ClientState, ChatMessage, ClientDict) ->
    {connectedUser, {name, Username}, {color, Color}} = ClientState,
    ChatMessageReply = build_chatreply(Color, Username, ChatMessage),

    broadcast_chatmessage(ChatMessageReply, ClientDict),

    ChatMessageReply.

broadcast_chatmessage(ChatMessageReply, ClientDict) ->
    {ok, Json} = json:encode(ChatMessageReply),
    lists:foreach(fun ({Pid, _}) ->
        Pid ! {chatMessage, Json}
    end, dict:to_list(ClientDict)).

build_chatreply(Color, Username, Message) ->
    {Mega, Secs, _} = os:timestamp(),
    Timestamp = (Mega * 1000000 + Secs) * 1000,

    {[{type, <<"message">>}, {data, {[{time, Timestamp}, {text, Message}, {author, Username}, {color, list_to_binary(Color)}]}}]}.

build_colorreply(Color) ->
    {[{type, <<"color">>}, {data, list_to_binary(Color)}]}.

build_historyreply(History) ->
    {[{type, <<"history">>}, {data, History}]}.

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