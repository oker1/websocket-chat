-module(cset_server).

-behaviour(gen_server).

%% API
-export([register/1, unregister/1, incomingMessageFromClient/2, incomingMessageFromNode/1, syncHistoryRequestFromNode/1]).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("chat_state.hrl").

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

start_link(InitParams) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitParams, []).

init({connect, undefined}) ->
    lager:info("Initializing state"),
    {ok, chat_state:initial()};

init({connect, Node}) ->
    lager:info("Connecting to node: ~p", [Node]),
    erlang:monitor_node(Node, true),

    lager:info("Requesting history from node: ~p", [Node]),
    History = rpc:call(Node, cset_server, syncHistoryRequestFromNode, [node()]),
    lager:info("Received history: ~p", [History]),

    {ok, chat_state:set_history(chat_state:initial(), History)}.

handle_call({syncHistoryRequestFromNode, Node}, _From, State) ->
    History = chat_state:history(State),
    lager:info("Sending history to node: ~p ~p", [Node, History]),
    {reply, History, State};

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

handle_cast({incomingMessageFromClient, RawMessage, Pid}, #state{clients = ClientDict, colorCounter = ColorCounter, history = History} = State) ->
    {ok, ClientState} = dict:find(Pid, ClientDict),

    NewState = case ClientState of
        {connectingUser} -> handle_color(RawMessage, Pid, State);
        OtherState       -> handle_chat(OtherState, ColorCounter, RawMessage, ClientDict, History)
    end,

    {noreply, NewState};

handle_cast({incomingMessageFromNode, RawMessage}, #state{clients = ClientDict, colorCounter = ColorCounter, history = History}) ->
    HistoryMessage = chat_protocol:extract_history(RawMessage),
    NewHistory = append_history(HistoryMessage, History),

    lager:info("Got message from other node: ~p", [HistoryMessage]),

    broadcast_chatmessage(RawMessage, ClientDict),

    {noreply, #state{clients = ClientDict, colorCounter = ColorCounter, history = NewHistory}};

handle_cast(Message, _State) ->
    lager:info("Received unkown cast message: ~p", [Message]).

handle_color(Username, Pid, State) ->
    State2 = chat_state:assign_color_to_user(State, Pid, Username),
    Color = chat_state:user_color(State2, Pid),

    EncodedMessage = chat_protocol:encode(chat_protocol:color(Color)),

    sendOutgoingMessage(Pid, EncodedMessage),

    lager:info("Received username: ~p. Sent color: ~p", [Username, Color]),

    State2.

handle_chat(ClientState, ColorCounter, ChatMessage, ClientDict, History) ->
    lager:info("Received Message: ~p", [ChatMessage]),
    MessageReply = handle_chatmessage(ClientState, ChatMessage, ClientDict),

    MessageStructure = chat_protocol:extract_history(MessageReply),

    NewHistory = append_history(MessageStructure, History),

    #state{clients = ClientDict, colorCounter = ColorCounter, history = NewHistory}.

append_history(MessageStructure, History) ->
    [ MessageStructure | History ].

handle_chatmessage(ClientState, ChatMessage, ClientDict) ->
    {connectedUser, {name, Username}, {color, Color}} = ClientState,
    ChatMessageReply = chat_protocol:message(Color, Username, ChatMessage),

    lists:foreach(fun (Node) ->
        lager:info("Sending message to node: ~p", [Node]),
        rpc:call(Node, cset_server, incomingMessageFromNode, [ChatMessageReply])
    end, nodes()),

    broadcast_chatmessage(ChatMessageReply, ClientDict),

    ChatMessageReply.

broadcast_chatmessage(ChatMessageReply, ClientDict) ->
    EncodedMessage = chat_protocol:encode(ChatMessageReply),
    lists:foreach(fun ({Pid, _}) ->
        sendOutgoingMessage(Pid, EncodedMessage)
    end, dict:to_list(ClientDict)).

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