-module(cset_server).

-behaviour(gen_server).

%% API
-export([register/1, unregister/1, incomingMessageFromClient/2, incomingMessageFromNode/1,
    syncHistoryRequestFromNode/1, colorRequestFromNode/1]).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-compile([{parse_transform, lager_transform}]).

%%%===================================================================
%%% API
%%%===================================================================

register(Pid) ->
    gen_server:cast(?SERVER, {register, Pid}).

unregister(Pid) ->
    gen_server:cast(?SERVER, {unregister, Pid}).

incomingMessageFromClient(Msg, Pid) ->
    gen_server:cast(?SERVER, {incomingMessageFromClient, Msg, Pid}).

incomingMessageFromNode(Msg) ->
    gen_server:cast(?SERVER, {incomingMessageFromNode, Msg}).

syncHistoryRequestFromNode(Node) ->
    gen_server:call(?SERVER, {syncHistoryRequestFromNode, Node}).

colorRequestFromNode(Node) ->
    gen_server:call(?SERVER, {colorRequestFromNode, Node}).

start_link(InitParams) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitParams, []).

init({connect, undefined}) ->
    lager:info("Initializing master state."),
    {ok, chat_state:initial(true)};

init({connect, Node}) ->
    lager:info("Initializing slave state."),
    lager:info("Connecting to master node: ~p", [Node]),
    erlang:monitor_node(Node, true),

    lager:info("Requesting history from master: ~p", [Node]),
    History = rpc:call(Node, cset_server, syncHistoryRequestFromNode, [node()]),
    lager:info("Received history: ~p", [History]),

    {ok, chat_state:set_history(chat_state:initial(Node), History)}.

handle_call({syncHistoryRequestFromNode, Node}, _From, State) ->
    History = chat_state:history(State),
    lager:info("Sending history to node: ~p ~p", [Node, History]),
    {reply, History, State};

handle_call({colorRequestFromNode, Node}, _From, State) ->
    {Color, NewState} = chat_state:reserve_color(State),
    lager:info("Sending color to node: ~p ~p", [Node, Color]),
    {reply, Color, NewState};

handle_call(Request, From, State) ->
    lager:info("Received unkown call request: ~p from: ~p", [Request, From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({register, Pid}, State) ->
    lager:info("User connected."),
    State2 = chat_state:add_connecting_user(State, Pid),
    send_history_to_client(chat_state:history(State), Pid),

    {noreply, State2};

handle_cast({unregister, Pid}, State) ->
    lager:info("User disconnected.", []),
    State2 = chat_state:remove_user(State, Pid),

    {noreply, State2};

handle_cast({incomingMessageFromClient, RawMessage, Pid}, State) ->
    NewState = case chat_state:user_connected(State, Pid) of
        false -> handle_color(RawMessage, Pid, State);
        _    -> handle_chat(RawMessage, Pid, State)
    end,

    {noreply, NewState};

handle_cast({incomingMessageFromNode, RawMessage}, State) ->
    HistoryMessage = chat_protocol:extract_history(RawMessage),

    lager:info("Got message from other node: ~p", [HistoryMessage]),

    broadcast_chatmessage(RawMessage, State),

    {noreply, chat_state:append_history(State, HistoryMessage)};

handle_cast(Message, _State) ->
    lager:info("Received unkown cast message: ~p", [Message]).

handle_color(Username, Pid, State) ->
    State2 = chat_state:assign_color_to_user(State, Pid, Username),
    Color = chat_state:user_color(State2, Pid),

    EncodedMessage = chat_protocol:encode(chat_protocol:color(Color)),

    sendOutgoingMessage(Pid, EncodedMessage),

    lager:info("Received username: ~p. Sent color: ~p", [Username, Color]),

    State2.

handle_chat(ChatMessage, Pid, State) ->
    lager:info("Received Message: ~p", [ChatMessage]),

    {user, Username, Color} = chat_state:connected_user(State, Pid),
    MessageReply = chat_protocol:message(Color, Username, ChatMessage),

    lists:foreach(fun (Node) ->
        lager:info("Sending message to node: ~p", [Node]),
        rpc:call(Node, cset_server, incomingMessageFromNode, [MessageReply])
    end, nodes()),

    broadcast_chatmessage(MessageReply, State),

    MessageStructure = chat_protocol:extract_history(MessageReply),

    chat_state:append_history(State, MessageStructure).

broadcast_chatmessage(ChatMessageReply, State) ->
    EncodedMessage = chat_protocol:encode(ChatMessageReply),
    lists:foreach(fun ({Pid, _}) ->
        sendOutgoingMessage(Pid, EncodedMessage)
    end, dict:to_list(chat_state:client_list(State))).

sendOutgoingMessage(Pid, Message) ->
    Pid ! {outgoingMessage, Message}.

send_history_to_client(History, Pid) ->
    lager:info("Sending history to user.", []),
    EncodedMessage = chat_protocol:encode(chat_protocol:history(lists:reverse(History))),
    sendOutgoingMessage(Pid, EncodedMessage).

handle_info({reconnectNode, Node}, State) ->
    lager:info("Reconnecting node: ~p", [Node]),
    erlang:monitor_node(Node, true),
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    lager:info("Node down: ~p", [Node]),
    timer:send_after(1000, self(), {reconnectNode, Node}),
    {noreply, State};

handle_info(Info, State) ->
    lager:info("Unknown message: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.