defmodule Doteki.Mixfile do
  use Mix.Project

  def project do
    [app: :doteki,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: []]
  end

  defp deps do
    [ 
      {:bucs, ~r/.*/, git: "https://github.com/botsunit/bucs.git", tag: "0.0.1"},
    ]
  end
end
