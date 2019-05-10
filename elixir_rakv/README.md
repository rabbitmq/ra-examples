# RaKv

Elixir implementation of Ra Key-Value store described in [Ra's State Machine Tutorial](https://github.com/rabbitmq/ra/blob/master/docs/internals/STATE_MACHINE_TUTORIAL.md)

## Installation / Building

mix do deps.get, deps.compile, compile

## Running

# Start three cluster nodes
shell1> iex --sname a@localhost -S mix
shell2> iex --sname b@localhost -S mix
shell3> iex --sname c@localhost -S mix

# Start another node to issue commands
shell4> iex --sname com@localhost -S mix

# Start the cluster
iex4 > RaKv.start([])
14:38:11.471 [info]  ra: started cluster ra\_kv with 3 servers
0 servers failed to start: []
Leader: {:rakv, :a@localhost}
{:ok, [rakv: :c@localhost, rakv: :b@localhost, rakv: :a@localhost], []}

# Issue a put command to a@localhost
iex4 > RaKv.put({:rakv, :a@localhost}, :key, "value 1")
{:ok, :inserted, leaderNode }

# Issue a get command to b@localhost
iex4 > RaKv.get({:rakv, :b@localhost}, :key)
{:ok, "value 1", leaderNode }

# Issue a put command to c@localhost
iex4 > RaKv.put({:rakv, :c@localhost}, :key, "value 2")
{:ok, :inserted, leaderNode }

# Issue a get command to b@localhost
iex4 > RaKv.get({:rakv, :b@localhost}, :key)
{:ok, "value 2", leaderNode }

