-module(cset_server).

-behaviour(gen_server).

%% API
-export([register/1, unregister/1, chatMessage/2, chatMessageFromNode/1, syncHistoryRequest/1,
    syncHistoryResponse/1]).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
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

syncHistoryRequest(Node) ->
    gen_server:cast(?SERVER, {syncHistoryRequest, Node}).

syncHistoryResponse(History) ->
    gen_server:cast(?SERVER, {syncHistoryResponse, History}).

start_link(InitParams) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitParams, []).

init({connect, undefined}) ->
    lager:info("Initializing state", []),
    {ok, initial_state()};

init({connect, Node}) ->
    lager:info("Connecting to node: ~p", [Node]),
    erlang:monitor_node(Node, true),

    lager:info("Requesting history from node: ~p", [Node]),
    rpc:call(Node, cset_server, syncHistoryRequest, [node()]),

    {ok, initial_state()}.

initial_state() ->
    #state{clients=dict:new(), colorCounter=0, history=[]}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({register, Pid}, #state{clients = ClientDict, history = History} = State) ->
    lager:info("User connected, sending history.", []),

    State2 = State#state{clients = dict:store(Pid, {connectingUser}, ClientDict)},

    EncodedMessage = chat_protocol:encode(chat_protocol:history(lists:reverse(History))),
    Pid ! {chatMessage, EncodedMessage},

    {noreply, State2};

handle_cast({unregsiter, Pid}, #state{clients = ClientDict} = State) ->
    lager:info("User disconnected.", []),
    State2 = State#state{clients = dict:erase(Pid, ClientDict)},
    {noreply, State2};

handle_cast({chatMessage, RawMessage, Pid}, #state{clients = ClientDict, colorCounter = ColorCounter, history = History}) ->
    {ok, ClientState} = dict:find(Pid, ClientDict),

    {NewColorCounter, NewHistory, NewClientDict} = case ClientState of
        {connectingUser} -> handle_color(RawMessage, ColorCounter, History, Pid, ClientDict);
        OtherState       -> handle_chat(OtherState, ColorCounter, RawMessage, ClientDict, History)
    end,

    {noreply, #state{clients = NewClientDict, colorCounter = NewColorCounter, history = NewHistory}};

handle_cast({chatMessageFromNode, RawMessage}, #state{clients = ClientDict, colorCounter = ColorCounter, history = History}) ->
    HistoryMessage = chat_protocol:extract_history(RawMessage),
    NewHistory = append_history(HistoryMessage, History),

    lager:info("Got message from other node: ~p", [HistoryMessage]),

    broadcast_chatmessage(RawMessage, ClientDict),

    {noreply, #state{clients = ClientDict, colorCounter = ColorCounter, history = NewHistory}};

handle_cast({syncHistoryRequest, Node}, #state{clients = ClientDict, colorCounter = ColorCounter, history = History} = State) ->
    lager:info("Sending history to node: ~p ~p", [Node, History]),
    rpc:call(Node, cset_server, syncHistoryResponse, [History]),
    {noreply, State};

handle_cast({syncHistoryResponse, NewHistory}, #state{clients = ClientDict, colorCounter = ColorCounter, history = PreviousHistory}) ->
    lager:info("Received history: ~p", [NewHistory]),
    {noreply, #state{clients = ClientDict, colorCounter = ColorCounter, history = NewHistory}};

handle_cast(Message, _State) ->
    lager:info("Received unkown cast message: ~p", [Message]).

pick_color(ColorCounter) ->
    Colors = ["red", "green", "blue", "magenta", "purple", "plum", "orange"],

    lists:nth(1 + ColorCounter rem length(Colors), Colors).

handle_color(Username, ColorCounter, History, Pid, ClientDict) ->
    Color = pick_color(ColorCounter),
    NewClientState = {connectedUser, {name, Username}, {color, Color}},
    NewColorCounter = ColorCounter + 1,
    NewClientDict = dict:store(Pid, NewClientState, ClientDict),

    handle_colormessage(NewClientState, Pid),

    lager:info("Received username: ~p. Sent color: ~p", [Username, Color]),

    {NewColorCounter, History, NewClientDict}.

handle_chat(ClientState, ColorCounter, ChatMessage, ClientDict, History) ->
    lager:info("Received Message: ~p", [ChatMessage]),
    MessageReply = handle_chatmessage(ClientState, ChatMessage, ClientDict),

    MessageStructure = chat_protocol:extract_history(MessageReply),

    NewHistory = append_history(MessageStructure, History),

    {ColorCounter, NewHistory, ClientDict}.

append_history(MessageStructure, History) ->
    [ MessageStructure | History ].


handle_colormessage(ClientState, Pid) ->
    {connectedUser, {name, _}, {color, Color}} = ClientState,
    ColorReply = chat_protocol:color(Color),
    EncodedMessage = chat_protocol:encode(ColorReply),

    Pid ! {chatMessage, EncodedMessage}.

handle_chatmessage(ClientState, ChatMessage, ClientDict) ->
    {connectedUser, {name, Username}, {color, Color}} = ClientState,
    ChatMessageReply = chat_protocol:message(Color, Username, ChatMessage),

    lists:foreach(fun (Node) ->
        lager:info("Sending message to node: ~p", [Node]),
        rpc:call(Node, cset_server, chatMessageFromNode, [ChatMessageReply])
    end, nodes()),

    broadcast_chatmessage(ChatMessageReply, ClientDict),

    ChatMessageReply.

broadcast_chatmessage(ChatMessageReply, ClientDict) ->
    EncodedMessage = chat_protocol:encode(ChatMessageReply),
    lists:foreach(fun ({Pid, _}) ->
        Pid ! {chatMessage, EncodedMessage}
    end, dict:to_list(ClientDict)).

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