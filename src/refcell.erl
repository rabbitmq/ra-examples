%% @doc implements a simple reference cell that processes can watch to be
%% notified of changes to it's value.
-module(refcell).
-behaviour(ra_machine).

-export([
         init/1,
         apply/4,

         put/2,
         get/1,
         watch/1,

         start/1,
         state_enter/2
         ]).

-record(?MODULE, {value :: term(),
                  watchers = #{} :: #{pid() => ok}}).

-opaque state() :: #?MODULE{}.

-type command() :: {put, term()} | get | {watch | unwatch, pid()}.

-export_type([
              state/0,
              command/0
              ]).

init(_) ->
    #?MODULE{}.

apply(_Meta, {put, Value}, Effects,
      #?MODULE{value = Value} = State) ->
    %% no change
    {State, Effects, ok};
apply(#{index := Idx}, {put, Value}, Effects0,
      #?MODULE{watchers = Watchers, value = OldValue} = State0) ->
    %% notify all watchers of the change of value
    Effects1 = maps:fold(
                fun (P, _, Acc) ->
                        [{send_msg, P, {refcell_changed, OldValue, Value}}
                         | Acc]
                end, Effects0, Watchers),
    State = State0#?MODULE{value = Value},
    %% emit a release cursor effect every 1000 commands or so
    %% (give or take the number of non state machine commands that ra
    %% processes
    Effects = case Idx rem 1000 of
                  0 -> [{release_cursor, Idx, State} | Effects1];
                  _ -> Effects1
              end,
    {State, Effects, ok};
apply(_Meta, get, Effects, State = #?MODULE{value = Value}) ->
    {State, Effects, Value};
apply(_Meta, {watch, Pid}, Effects, State = #?MODULE{watchers = Watchers}) ->
    {State#?MODULE{watchers = Watchers#{Pid => ok}},
     [{monitor, process, Pid} | Effects], ok};
apply(_Meta, {down, Pid, noconnection}, Effects, State) ->
    %% noconnection doesn't mean the watcher process is actually down
    %% to find out we monitor the node and re-issue monitors when the node
    %% comes back (see nodeup)
    {State, [{monitor, node, node(Pid)} | Effects], ok};
apply(_Meta, {down, Pid, _Reason}, Effects,
      #?MODULE{watchers = Watchers} = State) ->
    %% clean up watchers when monitor triggers
    {State#?MODULE{watchers = maps:remove(Pid, Watchers)}, Effects, ok};
apply(_Meta, {nodeup, Node}, Effects0,
      #?MODULE{watchers = Watchers} = State) ->
    %% notify all watchers
    Effects = maps:fold(
                fun (P, _, Acc) when node(P) =:= Node ->
                        [{monitor, proess, P} | Acc]
                end, Effects0, Watchers),
    {State, Effects, ok};
apply(_Meta, {nodedown, _}, Effects, State) ->
    %% we need to handle the nodedown as well to avoid crashing
    {State, Effects, ok}.


%% when a server enters leader state we need to re-issue all monitors
state_enter(leader, #?MODULE{watchers = Watchers}) ->
    maps:fold(
                fun (P, _, Acc) ->
                        [{monitor, process, P} | Acc]
                end, [], Watchers);
state_enter(_, _) ->
    [].

%% API

put(Server, Value) ->
    case ra:process_command(Server, {put, Value}) of
        {ok, _, ok} -> ok;
        Err -> Err
    end.

get(Server) ->
    case ra:process_command(Server, get) of
        {ok, Value, _} ->
            {ok, Value};
        Err -> Err
    end.

watch(Server) ->
    case ra:process_command(Server, {watch, self()}) of
        {ok, ok, _} -> ok;
        Err -> Err
    end.

start(Name) ->
    start(Name, [node()]).

start(Name, Nodes) ->
    Servers = [{Name, N} || N <- Nodes],
    ra:start_cluster(Name, {module, ?MODULE, #{}}, Servers).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
