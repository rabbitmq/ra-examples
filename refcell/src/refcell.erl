%% @doc implements a simple reference cell that processes can watch to be
%% notified of changes to it's value.
-module(refcell).
-behaviour(ra_machine).

-export([
	 init/1,
	 init/0,
	 apply/3,

	 put/2,
	 get/1,
	 watch/1,

	 start/1,
	 start/2,

	 members/0,
	 members/1,

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

init() ->
    application:ensure_all_started(ra),
    init(init).

apply(_Meta, {put, Value},
      #?MODULE{value = Value} = State) ->
    %% no change
    {State, ok, []};
apply(#{index := Idx}, {put, Value},
      #?MODULE{watchers = Watchers, value = OldValue} = State0) ->
    %% notify all watchers of the change of value
    Effects0 = maps:fold(
		 fun(P, _, Acc) ->
			 [{send_msg, P, {refcell_changed, OldValue, Value}}
			  | Acc]
		 end, [], Watchers),
    State = State0#?MODULE{value = Value},
    %% emit a release cursor effect every 1000 commands or so
    %% (give or take the number of non state machine commands that ra
    %% processes
    Effects = case Idx rem 1000 of
		  0 -> [{release_cursor, Idx, State} | Effects0];
		  _ -> Effects0
	      end,
    {State, ok, Effects};
apply(_Meta, get, State = #?MODULE{value = Value}) ->
    {State, Value, []};
apply(_Meta, {watch, Pid}, State = #?MODULE{watchers = Watchers}) ->
    {State#?MODULE{watchers = Watchers#{Pid => ok}},
     ok, [{monitor, process, Pid}]};
apply(_Meta, {down, Pid, noconnection}, State) ->
    %% noconnection doesn't mean the watcher process is actually down
    %% to find out we monitor the node and re-issue monitors when the node
    %% comes back (see nodeup)
    {State, ok, [{monitor, node, node(Pid)}]};
apply(_Meta, {down, Pid, _Reason},
      #?MODULE{watchers = Watchers} = State) ->
    %% clean up watchers when monitor triggers
    {State#?MODULE{watchers = maps:remove(Pid, Watchers)}, ok, []};
apply(_Meta, {nodeup, Node},
      #?MODULE{watchers = Watchers} = State) ->
    %% notify all watchers
    Effects = maps:fold(
		fun(P, _, Acc) when node(P) =:= Node ->
			[{monitor, proess, P} | Acc]
		end, [], Watchers),
    {State, ok, Effects};
apply(_Meta, {nodedown, _}, State) ->
    %% we need to handle the nodedown as well to avoid crashing
    {State, ok, []}.


%% when a server enters leader state we need to re-issue all monitors
state_enter(leader, #?MODULE{watchers = Watchers}) ->
    maps:fold(
      fun(P, _, Acc) ->
	      [{monitor, process, P} | Acc]
      end, [], Watchers);
state_enter(_, _) ->
    [].

%% API

put(Server, Value) ->
    case ra:process_command(Server, {put, Value}) of
	{ok, _, _} -> ok;
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
	{ok, _, _} -> ok;
	Err -> Err
    end.

start(Name) ->
    start(Name, [node()]).

start(Name, Nodes) ->
    Servers = [{refcell, N} || N <- Nodes],
    ra:start_cluster(Name, {module, ?MODULE, #{}}, Servers).

members() ->
    members(node()).

members(Node) ->
    case ra:members({refcell, Node}) of
	{ok, Result, Leader} -> io:format("Cluster Members:~nLeader:~p~nFollowers:~p~n" ++
					      "Nodes:~p~n", [Leader, lists:delete(Leader, Result), Result]);
	Err -> io:format("Cluster Status error: ~p", [Err])
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
