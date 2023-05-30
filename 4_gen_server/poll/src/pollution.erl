-module(pollution).

-export([
  create_monitor/0,
  add_station/3,
  add_value/5,
  remove_value/4,
  get_station/2,
  get_one_value/4,
  get_station_mean/3,
  get_daily_mean/3,
  get_correlation/3
]).

-include("records.hrl").

create_monitor() -> #monitor{}.

is_in_monitor(Name, Point, Monitor) -> lists:any(fun(S) -> S#station.name == Name orelse S#station.point == Point end, Monitor#monitor.stations).


add_station(Name, Point, Monitor) ->
  case is_in_monitor(Name, Point, Monitor) of
    true ->
      Monitor;
    false ->
      Monitor#monitor{
        stations = [#station{name = Name, point = Point} | Monitor#monitor.stations]
      }
  end.

get_station(StationCoords = {_, _}, Stations) ->
  case lists:any(fun(S) -> StationCoords == S#station.point end, Stations) of
    false -> {error, "No station in monitor."};
    true -> lists:nth(1, [S || S <- Stations, S#station.point == StationCoords])
  end;

get_station(StationName, Stations) ->
  case lists:any(fun(S) -> StationName == S#station.name end, Stations) of
    false -> {error, "No station in monitor."};
    true -> lists:nth(1, [S || S <- Stations, S#station.name == StationName])
  end.

add_value(StationIdentifier, Datetime, Type, Value, Monitor) ->
  case get_station(StationIdentifier, Monitor#monitor.stations) of
    {error, message} -> {error, message};
    Station ->  case lists:any(fun(M) -> M#measurement.station == Station andalso M#measurement.datetime == Datetime andalso M#measurement.type == Type end, Monitor#monitor.measurements) of
            true ->
              Monitor;
            false ->
              Monitor#monitor{measurements = [#measurement{station = Station, datetime = Datetime, type = Type, value = Value} | Monitor#monitor.measurements]}
          end
  end.

remove_value(StationIdentifier, Datetime, Type, Monitor) ->
  case get_station(StationIdentifier, Monitor#monitor.stations) of
    {error, message} -> Monitor;
    Station -> case lists:any(fun(R) -> R#measurement.station == Station andalso
                                        R#measurement.datetime == Datetime andalso
                                        R#measurement.type == Type end,
                                        Monitor#monitor.measurements) of
        true ->
            NewMeasurements = lists:filter(
                fun(R) ->
                    not (R#measurement.station == Station andalso
                    R#measurement.datetime == Datetime andalso
                    R#measurement.type == Type) end,
                    Monitor#monitor.measurements),
            Monitor#monitor{measurements = NewMeasurements};
        false ->
            {error, "Value not in station."}
        end
  end.

get_one_value(StationIdentifier, Datetime, Type, Monitor) ->
    Station = get_station(StationIdentifier, Monitor#monitor.stations),
    Filtered = [R#measurement.value || R <- Monitor#monitor.measurements, R#measurement.station == Station andalso
        R#measurement.datetime == Datetime andalso
        R#measurement.type == Type],
    case Filtered == [] of
        false -> lists:nth(1, Filtered);
        true -> {error, "No such measurement."}
    end.

get_station_mean(StationIdentifier, Type, Monitor) ->
    Station = get_station(StationIdentifier, Monitor#monitor.stations),
    Measurements = [R#measurement.value || R <- Monitor#monitor.measurements,
        R#measurement.station == Station andalso
        R#measurement.type == Type],
    case erlang:length(Measurements) of
        0 -> {error, "Too few elements to compute mean."};
        _ -> lists:sum(Measurements) / erlang:length(Measurements)
    end.

get_daily_mean(Type, Date, Monitor) ->
    Measurements = [M#measurement.value || M <- Monitor#monitor.measurements, 
        element(1, M#measurement.datetime) == Date andalso
        M#measurement.type == Type],
  
    case erlang:length(Measurements) of
        0 -> {error, "Too few elements to compute mean."};
        _ -> lists:sum(Measurements) / erlang:length(Measurements)
    end.

get_correlation(Type_1, Type_2, Monitor) -> 
    Diffs = [M1#measurement.value - M2#measurement.value ||
        M1 <- Monitor#monitor.measurements,
        M2 <- Monitor#monitor.measurements,
        M1#measurement.datetime == M2#measurement.datetime,
        M1#measurement.type == Type_1,
        M2#measurement.type == Type_2
    ],
    case erlang:length(Diffs) of
        0 -> {error, "Too few element to compute correlation"};
        _ -> lists:sum(Diffs) / erlang:length(Diffs)
    end.