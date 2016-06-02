defmodule Doteki.Mixfile do
  use Mix.Project

  def project do
    [
      app: :doteki,
      version: "0.0.2",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps
    ]
  end

  def application do
    [
       applications: [:kernel, :stdlib],
       env: []
    ]
  end

  defp deps do
    [
      {:bucs, git: "https://github.com/botsunit/bucs.git", branch: "master"}    
    ]
  end
end