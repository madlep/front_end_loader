
-module(fel_sup).

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

start_link(LogFileName) ->
  supervisor:start_link(?MODULE, [LogFileName]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LogFileName]) ->
  % TODO - need to handle restarts at point in time where file reader died
  {ok, { {one_for_one, 5, 10}, [
    {fel_file_reader, {fel_file_reader, start_link, [LogFileName]}, temporary, brutal_kill, [fel_file_reader]}
  ]} }.

