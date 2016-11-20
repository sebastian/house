defmodule House.Updater do
  @moduledoc false

  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def update() do
    with load_updates(),
         make_release(),
         restart() do
      Logger.warn("Restart scheduled")
    else
      _ -> Logger.warn("Update failed")
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_updates() do
    Logger.info("Loading version of software")
    case System.cmd("git", ["pull"], cd: cwd()) do
      {result, 0} ->
        Logger.info("Success: #{result}")
        true
      {result, code} ->
        Logger.warn("`git pull` failed (status code: #{code}): #{result}")
        false
    end
  end

  defp make_release() do
    Logger.info("Building a release")
    case System.cmd("make", ["release"], cd: cwd()) do
      {result, 0} ->
        Logger.info("Success: #{result}")
        true
      {result, code} ->
        Logger.warn("`make release` failed (status code: #{code}): #{result}")
        false
    end
  end

  defp restart() do
    Logger.info("Building a release")
    case System.cmd("make", ["restart"], cd: cwd()) do
      {result, 0} ->
        Logger.info("Success: #{result}")
        true
      {result, code} ->
        Logger.warn("`restart` failed (status code: #{code}): #{result}")
        false
    end
  end

  defp cwd(), do:
    System.cwd!()
    |> Path.join("../..")
end
