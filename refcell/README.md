# A Reference Cell Example

This is a reference cell example based on [Ra](https://github.com/rabbitmq/ra).

## Compiling

``` bash
rebar3 compile
```


## Test the example in localhost:


To test the example in localhost with three nodes you can:

- run the node1:
```
rebar3 shell --sname node1

```

- run the node2:
```
rebar3 shell --sname node1

```
- run the node2:
```
rebar3 shell --sname node1

```

Init the nodes with `refcell:init()`:

```erlang
(node1@GaS)1> refcell:init().
2019-05-24T17:21:11.981891+02:00 notice: WAL: recovering ["/home/gabriele/git/Pivotal/ra/refcell/node1@GaS/00000030.wal"]
2019-05-24T17:21:11.982120+02:00 notice: wal: opening new file "00000031.wal"
{refcell,undefined,#{}}
(node1@GaS)2> 2019-05-24T17:21:11.987432+02:00 notice: ra_log_segment_writer: error sending ra_log_event to: MYTE8K5Z7DX5I4A4. Error: No Pid
2019-05-24T17:21:11.987524+02:00 notice: ra_log_segment_writer: cleaning closed table for 'MYTE8K5Z7DX5I4A4' range: 0-1
2019-05-24T17:21:11.987598+02:00 notice: segment_writer: deleting wal file: 00000030.wal

```

Then start the cluster:

```erlang
ErlangNodes = [node1@GaS, node2@GaS, node3@GaS].
refcell:start("My Test CLuster", ErlangNodes).
```

Check the cluster members:

```erlang
refcell:members().
Cluster Members:
Leader:{refcell,node1@GaS}
Followers:[{refcell,node2@GaS},{refcell,node3@GaS}]
Nodes:[{refcell,node1@GaS},{refcell,node2@GaS},{refcell,node3@GaS}]
```

put a value:
```erlang
refcell:put({refcell,node2@GaS}, "MyValue").
```

get a value:
```erlang
refcell:get({refcell,node1@GaS}).
```

You can test the failover by stopping a node, change the value, restart the node and execute the get again


