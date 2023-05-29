-record(monitor, {
  stations = [],
  records = [],
  measurements = []
}).

-record(station, {
  name,
  point
}).

-record(measurement, {
  station,
  datetime,
  type,
  value
}).