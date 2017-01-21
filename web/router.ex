defmodule House.Router do
  use House.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", House do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get "/trigger_away", PageController, :off
    get "/is_off", PageController, :is_off
  end
end
