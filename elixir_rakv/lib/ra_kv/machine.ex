defmodule RaKv.Machine do
  @behaviour :ra_machine

  @impl :ra_machine
  def init(_args) do
    %{}
  end
  
  @impl :ra_machine
  def apply(_meta, {:put, key, value}, effects, state) do
    {Map.put(state, key, value), effects, :inserted}
  end

  @impl :ra_machine
  def apply(_meta, {:get, key}, effects, state) do
    reply = Map.get(state, key, nil)
    {state, effects, reply}
  end

end

