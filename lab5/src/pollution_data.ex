defmodule PollutionData do

  defp importLinesFromCSV do
    l = File.read!("pollution.csv")
        |> String.split("\r\n")
    cond do
      :erlang.length(l) > 5900 -> l
      true -> "Lines from CSV file weren't imported properly!"
    end
  end

  defp parseLines(data) do
    for x <- data do parseOneLine(x) end
  end

  defp parseOneLine(line) do
    [day,hour,lat,long,val] = String.split(line,",")

    dateDay = day |> String.split("-")
              |> Enum.reverse
              |> Enum.map(fn (el) -> elem(Integer.parse(el),0) end)
              |> :erlang.list_to_tuple

    dateTime = hour |> String.split(":")
               |> Enum.map(fn (el) -> elem(Integer.parse(el),0) end)
               |> :erlang.list_to_tuple
               |> Tuple.append(0)

    location = [lat,long] |> Enum.map(fn (el) -> elem(Float.parse(el),0) end)
               |> :erlang.list_to_tuple
    %{
      :datetime => {dateDay,dateTime},
      :location => location,
      :pollutionLevel => elem(Float.parse(val),0)
    }
  end

  # 119 unikatowych stacji
  defp identifyStations(parsedLines) do
    parsedLines |> Enum.map(fn map -> map.location end)
    |> Enum.uniq_by(fn x -> x end)
  end

  defp loadStation({lat,long}) do
    stationName = "station_#{long}_#{lat}"
    :pollution_gen_server.addStation(stationName,{lat,long})
  end

  def loadStats(parsedLines) do
    identifyStations(parsedLines) |> Enum.map(fn loc -> loadStation(loc) end)
  end

  defp loadStations(parsedLines) do
    :pollution_gen_server.startServer(:pollution.createMonitor)
    :timer.tc(PollutionData,:loadStats,[parsedLines])
          |> elem(0)
          |> Kernel./(1_000_000)
          |> IO.puts
  end

  defp loadValue(line) do
    %{datetime: date, location: loc, pollutionLevel: val} = line
    :pollution_gen_server.addValue(loc,date,"PM10",val)
  end

  def loadVal(parsedLines) do
    parsedLines |> Enum.map(fn line -> loadValue(line) end)
  end

  defp loadValues(parsedLines) do
    :timer.tc(PollutionData,:loadVal,[parsedLines])
          |> elem(0)
          |> Kernel./(1_000_000)
          |> IO.puts
  end

  defp analyzeData do
    r1 = :timer.tc(:pollution_gen_server,:getStationMean,[{20.06,49.986},"PM10"])
    r2 = :timer.tc(:pollution_gen_server,:getDailyMean,[{2017,5,3},"PM10"])
    IO.puts "Time1 = #{elem(r1,0)} and value1 = #{elem(r1,1)}, time2 = #{elem(r2,0)} and value2 = #{elem(r2,1)}}"
  end

  def startServerWithData do
    d = importLinesFromCSV
    pl = parseLines(d)
    loadStations(pl)
    loadValues(pl)
    analyzeData
    :ok
  end

end
