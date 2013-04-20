-module(chat_protocol).

-export([message/3, color/1, history/1, encode/1, extract_history/1]).

message(Color, Username, Message) ->
    {Mega, Secs, _} = os:timestamp(),
    Timestamp = (Mega * 1000000 + Secs) * 1000,

    {[{type, <<"message">>}, {data, {[{time, Timestamp}, {text, Message}, {author, Username}, {color, list_to_binary(Color)}]}}]}.

color(Color) ->
    {[{type, <<"color">>}, {data, list_to_binary(Color)}]}.

history(History) ->
    {[{type, <<"history">>}, {data, History}]}.

encode(Message) ->
    {ok, Json} = json:encode(Message),
    Json.

extract_history(MessageReply) ->
    {[{type, _}, {data, MessageStructure}]} = MessageReply,

    MessageStructure.