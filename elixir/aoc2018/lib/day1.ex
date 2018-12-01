defmodule Day1 do

  @doc """
  Calculate frequency at each shift

  ### Examples
  iex> Day1.shift(["+1", "+1", "+1"])
  3
  iex> Day1.shift(["+1", "+1", "-2"])
  0
  iex> Day1.shift(["-1", "-2", "-3"])
  -6
  """
  def shift(shifts) do
    shifts
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
  end

  defp store(current, {shift, history}) do
    {shift + current, MapSet.put(history, shift)}
  end

  @doc"""
  Loop through the frequency shifts until the frequency repeats
  
  ### Examples
  iex> Day1.loop(["+1", "-1"])
  0
  iex> Day1.loop(["+3", "+3", "+4", "-2", "-4"])
  10
  iex> Day1.loop(["-6", "+3", "+8", "+5", "-6"])
  5
  iex> Day1.loop(["+7", "+7", "-2", "-7", "-4"])
  14
  """
  def loop(shifts) do 
    [{result, _} | _] = shifts 
    |> Stream.cycle
    |> Stream.map(&String.to_integer/1)
    |> Stream.scan({0, MapSet.new()}, &store/2)
    |> Stream.drop_while(fn ({current, history}) -> not MapSet.member?(history, current) end)
    |> Stream.take(1)
    |> Enum.to_list

    result
  end

end
