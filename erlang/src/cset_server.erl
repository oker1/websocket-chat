-module(cset_server).

-behaviour(gen_server).

%% API
-export([register/1, unregister/1, chatMessage/2, chatMessageFromNode/1]).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {clients, colorCounter, history, nodes}).

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
    lager:info("Got message from other node"),
    gen_server:cast(?SERVER, {chatMessageFromNode, Msg}).

start_link(InitParams) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitParams, []).

init({nodes, Nodes}) ->
    lists:foreach(fun (Node) ->
        lager:info("Connecting to node: ~p", [Node]),
        net_kernel:connect_node(Node)
    end, Nodes),

    {ok, #state{clients=dict:new(), colorCounter=0, history=[], nodes=Nodes}}.

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

handle_cast({chatMessage, RawMessage, Pid}, #state{clients = ClientDict, colorCounter = ColorCounter, history = History, nodes = Nodes}) ->
    {ok, ClientState} = dict:find(Pid, ClientDict),

    {NewColorCounter, NewHistory, NewClientDict} = case ClientState of
        {connectingUser} -> handle_color(RawMessage, ColorCounter, History, Pid, ClientDict);
        OtherState       -> handle_chat(OtherState, ColorCounter, RawMessage, ClientDict, History, Nodes)
    end,

    {noreply, #state{clients = NewClientDict, colorCounter = NewColorCounter, history = NewHistory, nodes = Nodes}};

handle_cast({chatMessageFromNode, RawMessage}, #state{clients = ClientDict, colorCounter = ColorCounter, history = History, nodes = Nodes} = State) ->
    lager:info("Got message from other node"),

    HistoryMessage = extract_history(RawMessage),
    NewHistory = append_history(HistoryMessage, History),

    broadcast_chatmessage(RawMessage, ClientDict),

    {noreply, #state{clients = ClientDict, colorCounter = ColorCounter, history = NewHistory, nodes = Nodes}}.

pick_color(ColorCounter) ->
    Colors = ["red", "green", "blue", "magenta", "purple", "plum", "orange"],

    lists:nth(1 + ColorCounter rem length(Colors), Colors).

handle_color(Username, ColorCounter, History, Pid, ClientDict) ->
    NewClientState = {connectedUser, {name, Username}, {color, pick_color(ColorCounter)}},
    NewColorCounter = ColorCounter + 1,
    NewClientDict = dict:store(Pid, NewClientState, ClientDict),

    handle_colormessage(NewClientState, Pid),

    {NewColorCounter, History, NewClientDict}.

handle_chat(ClientState, ColorCounter, ChatMessage, ClientDict, History, Nodes) ->
    MessageReply = handle_chatmessage(ClientState, ChatMessage, ClientDict, Nodes),

    MessageStructure = extract_history(MessageReply),

    NewHistory = append_history(MessageStructure, History),

    {ColorCounter, NewHistory, ClientDict}.

append_history(MessageStructure, History) ->
    [ MessageStructure | History ].

extract_history(MessageReply) ->
    {[{type, _}, {data, MessageStructure}]} = MessageReply,

    MessageStructure.

handle_colormessage(ClientState, Pid) ->
    {connectedUser, {name, _}, {color, Color}} = ClientState,
    ColorReply = build_colorreply(Color),
    {ok, Json} = json:encode(ColorReply),

    Pid ! {chatMessage, Json}.

handle_chatmessage(ClientState, ChatMessage, ClientDict, Nodes) ->
    {connectedUser, {name, Username}, {color, Color}} = ClientState,
    ChatMessageReply = build_chatreply(Color, Username, ChatMessage),

    lists:foreach(fun (Node) ->
        lager:info("Sending message to node: ~p", [Node]),
        rpc:call(Node, cset_server, chatMessageFromNode, [ChatMessageReply])
    end, Nodes),

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