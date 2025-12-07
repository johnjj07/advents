defmodule Advent25.Day1 do

  def solve(input_file, part \\ 2) do
    stream = File.stream!(input_file)
    reducer = case part do
      1 -> &calculate_pt_1/2
      2 -> &calculate_pt_2/2
      _ -> raise "Unexpected part"
    end
    {_, count} = File.stream!(input_file)
    |> Enum.map(&process_line/1)
    |> Enum.reduce({50, 0}, reducer) 
    count
  end

  def process_line(line) do
    String.trim(line)
    |> turn
  end

  def calculate_pt_2(number, {location, count} = acc) do
    full_turns = abs(div(number, 100))
    clicks = rem(number, 100)
    new_location = Integer.mod(location + clicks, 100)
    if new_location == 0 || ((location + clicks) != new_location && location != 0) do
      {new_location, count + 1 + full_turns}
    else
      {new_location, count + full_turns}
    end
  end

  def calculate_pt_1(number, {location, count} = acc) do
    new_location = Integer.mod(location + number, 100)
    if new_location == 0 do
      {new_location, count + 1}
    else
     {new_location, count}
    end
  end

  defp turn("L" <> number) do
    String.to_integer(number) * -1
  end

  defp turn("R" <> number) do
    String.to_integer(number)
  end

  defp turn(line) do
    raise "Unexpected Input"
  end

end
