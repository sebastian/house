defmodule House.Mode do
  use GenServer

  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def start_link(), do:
    GenServer.start_link(__MODULE__, [], [name: __MODULE__])

  def auto(), do:
    GenServer.cast(__MODULE__, {:set, :auto})

  def manual(), do:
    GenServer.cast(__MODULE__, {:set, :manual})

  def get(), do:
    GenServer.call(__MODULE__, :get)

  def auto?(), do:
    get() == :auto


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  def init(_) do
    Logger.info("Init mode controller")
    {:ok, :auto}
  end

  def handle_cast({:set, mode}, state) do
    {:noreply, mode}
  end

  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end
end
