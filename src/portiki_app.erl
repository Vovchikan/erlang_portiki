%%%-------------------------------------------------------------------
%% @doc portiki public API
%% @end
%%%-------------------------------------------------------------------

-module(portiki_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    portiki_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
