-module(random_positions).
-behaviour(gen_server).

% required by gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% api
-export([new/0, next/0, next/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
  {ok, []}.

handle_cast(_Msg, N) ->
  {noreply, N}.

handle_info(_Msg, N) ->
  {noreply, N}.

code_change(_OldVsn, N, _Other) ->
  {ok, N}.

terminate(_Reason, _N) ->
  ok.

handle_call(next_number, _From, ListOfNumbers) ->
  RandomNumber = next(ListOfNumbers),
  {reply, RandomNumber, [RandomNumber|ListOfNumbers]}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%        API        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new() -> 'ignore' | {'error', _} | {'ok', pid()}.
new() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec next() -> board:position().
next() ->
  gen_server:call(?MODULE, next_number).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     INTERNALS     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

% This will loop forever if ListOfNumbers has 9 numbers or more.
% Trusting the client this is not going to happen
next(ListOfNumbers) when is_list(ListOfNumbers) ->
    Number = rand:uniform(9),
    case Number =/= 0 andalso not lists:member(Number, ListOfNumbers) of
        true -> Number;
        false -> next(ListOfNumbers)
    end.
