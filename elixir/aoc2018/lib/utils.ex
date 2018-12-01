defmodule Utils do
  def show([a, b, c, d, e | _]), do: '[#{a}, #{b}, #{c}, #{d}, #{e}.... ]'
  def show(list), do: "#{list}"
end
