# Websocket chat in erlang

Backend ported from http://martinsikora.com/nodejs-and-websocket-simple-chat-tutorial

```
wget https://github.com/rebar/rebar/wiki/rebar
chmod +x rebar
.rebar get-deps && ./rebar compile
erl -pa ebin -pa deps/*/ebin -boot start_sasl -s cset -sname cset1 -cset port 1337
erl -pa ebin -pa deps/*/ebin -boot start_sasl -s cset -sname cset2 -cset port 1338 -cset connect "'cset1@<your-hostname>'"
```

open http://localhost:1337
open http://localhost:1338


TODOS:
use supervisors
cleanup server module
sync history when node comes back
global color cycling