defmodule Doteki.Mixfile do
  use Mix.Project

  def project do
    [
      app: :doteki,
      version: "0.1.0",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps
    ]
  end

  def application do
    [
       applications: [],
       env: []
    ]
  end

  defp deps do
    [
      {:bucs, git: "https://github.com/botsunit/bucs.git", tag: "0.0.2"}    
    ]
  end
end