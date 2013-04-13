Websocket chat in erlang, ported from http://martinsikora.com/nodejs-and-websocket-simple-chat-tutorial

./rebar compile
erl -pa ebin -pa deps/*/ebin -boot start_sasl -s cset

open html/frontend.html in browser
