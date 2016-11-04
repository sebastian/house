defmodule House.PageController do
  use House.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
