defmodule RaKv do
  @moduledoc """
  Documentation for RaKv.
  """

  def start(_args) do
    # the initial cluster members
    members = Enum.map([:a@localhost, :b@localhost, :c@localhost], fn node -> { :rakv, node } end)
    # an arbitrary cluster name
    clusterName = <<"ra_kv">>
    # the config passed to `init/1`, must be a `map`
    config = %{}
    # the machine configuration
    machine = {:module, RaKv.Machine, config}
    # ensure ra is started
    Application.ensure_all_started(:ra)
    # start a cluster instance running the `ra_kv` machine
    :ra.start_cluster(clusterName, machine, members)
  end

  ## Client API

  def put(serverid, key, value) do
    :ra.process_command(serverid, {:put, key, value})
  end

  def get(serverid, key) do
    :ra.process_command(serverid, {:get, key})
  end

end
