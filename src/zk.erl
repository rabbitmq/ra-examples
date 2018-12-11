% %% @doc Ra machine implementing a subset of the ZooKeeper API
% %% [https://zookeeper.apache.org/doc/current/zookeeperOver.html#Simple+API]
-module(zk).
% -behaviour(ra_machine).

% -export([
%          init/1,
%          apply/4
%          ]).

% -record(?MODULE, {}).

% -opaque state() :: #?MODULE{}.

% -export_type([
%               state/0
%               ]).

% -type command() ::
%     {create, location(), Flags :: [flag()]}.

% init(_) ->
%     #?MODULE{}.

% apply(_Meta, _Cmd, Effects, State) ->
%     {State, Effects, ok}.

% -ifdef(TEST).
% -include_lib("eunit/include/eunit.hrl").
% -endif.
