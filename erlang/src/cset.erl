-module(cset).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% ===================================================================
%% Application callbacks
%% ===================================================================
start_link() ->
    ensure_started(crypto),
    ensure_started(cowboy),
    core_sup:start_link().

start() ->
    ensure_started(crypto),
    ensure_started(cowboy),
    application:start(cset).

stop() ->
    Res = application:stop(cset),
    application:stop(cowboy),
    application:stop(crypto),
    Res.
