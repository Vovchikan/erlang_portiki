%%%-------------------------------------------------------------------
%% @doc portiki top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(portiki_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 3,
                 period => 2000},
    ChildSpecs = [
                  #{ id       => portiki_debugger,
                     start    => {portiki_debugger, start_link, []},
                     restart  => permanent,
                     type     => worker,
                     modules  => [portiki_debugger] },
                  #{ id       => portiki_watcher,
                     start    => {portiki_watcher, start_link, []},
                     restart  => permanent,
                     type     => worker,
                     modules  => [portiki_watcher] }
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
