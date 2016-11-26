-module(tictactoerl_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 1, 5},
          [{console,
            {tictactoerl_fsm, start_link, []},
             temporary, 5000, worker, [tictactoerl_fsm]}]}}.
