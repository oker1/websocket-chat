
-module(cset_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(InitParams) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, InitParams).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(InitParams) ->
    {ok, { {one_for_one, 5, 10}, InitParams} }.

