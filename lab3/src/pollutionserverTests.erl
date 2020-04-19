-module(pollutionserverTests).
-author("Lukasz Jezapkowicz").


-include_lib("eunit/include/eunit.hrl").

% here are tests that test if structure is building properly
structure_manip_test() ->
  [
    { "Starts server",
      pollutionserver:start(),
      ?assertMatch(1,1)
    },
    { "Checks if empty monitor is correctly build",
      fun () ->
        ?assertMatch([],pollutionserver:getMonitor())
      end ()
    },
    { "Checks if station is correctly added",
      fun () ->
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        pollutionserver:addStation(StationName,LatAndLon),

        ?assertMatch(true,lists:member({StationName,LatAndLon,sets:new()},pollutionserver:getMonitor()))
      end ()
    },
    { "Checks if station cannot be duplicated",
      fun () ->
        pollutionserver:reset(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        pollutionserver:addStation(StationName,LatAndLon),
        pollutionserver:addStation(StationName,LatAndLon),

        ?assertMatch(true,lists:member({StationName,LatAndLon,sets:new()},pollutionserver:getMonitor())),
        ?assertMatch(1,length(pollutionserver:getMonitor()))
      end ()
    },
    { "Checks if some measurement is correctly added to the Station",
      fun () ->
        pollutionserver:reset(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        pollutionserver:addStation(StationName,LatAndLon),
        Time = calendar:local_time(),
        pollutionserver:addValue(StationName,Time,"Temp",10),
        [{_,_,S}] = [{A,B,C} || {A,B,C} <- pollutionserver:getMonitor(), A == StationName],

        ?assertMatch(true,sets:is_element({"Temp",10,Time},S))
      end ()
    },
    { "Checks if some measurement cannot be duplicated in the Station",
      fun () ->
        pollutionserver:reset(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        pollutionserver:addStation(StationName,LatAndLon),
        Time = calendar:local_time(),
        pollutionserver:addValue(StationName,Time,"Temp",10),
        pollutionserver:addValue(StationName,Time,"Temp",10),
        [{_,_,S}] = [{A,B,C} || {A,B,C} <- pollutionserver:getMonitor(), A == StationName],

        ?assertMatch(true,sets:is_element({"Temp",10,Time},S)),
        ?assertMatch(1,sets:size(S))
      end ()
    },
    { "Checks if some measurement cannot be added to non-existing Station",
      fun () ->
        pollutionserver:reset(),
        StationName = "Rybnik",
        Time = calendar:local_time(),
        pollutionserver:addValue(StationName,Time,"Temp",10),

        ?assertMatch(0,length(pollutionserver:getMonitor()))
      end ()
    },
    { "Checks if some measurement can be deleted from the Station",
      fun () ->
        pollutionserver:reset(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        pollutionserver:addStation(StationName,LatAndLon),
        Time = calendar:local_time(),
        pollutionserver:addValue(StationName,Time,"Temp",10),
        pollutionserver:removeValue(StationName,Time,"Temp"),
        [{_,_,S}] = [{A,B,C} || {A,B,C} <- pollutionserver:getMonitor(), A == StationName],

        ?assertMatch(false,sets:is_element({"Temp",10,Time},S))
      end ()
    },
    { "Checks if some measurement cannot be deleted from the non-existing station",
      fun () ->
        pollutionserver:reset(),
        StationName = "Rybnik",
        Time = calendar:local_time(),
        pollutionserver:removeValue(StationName,Time,"Temp"),

        ?assertMatch(0,length(pollutionserver:getMonitor()))
      end ()
    },
    { "Checks if you can get existing measurement",
      fun () ->
        pollutionserver:reset(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        pollutionserver:addStation(StationName,LatAndLon),
        Time = calendar:local_time(),
        pollutionserver:addValue(StationName,Time,"Temp",10),

        ?assertMatch(10,pollution:getOneValue(StationName,Time,"Temp",pollutionserver:getMonitor()))
      end ()
    },
    { "Checks if you cannot get non-existing measurement from existing station",
      fun () ->
        pollutionserver:reset(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        pollutionserver:addStation(StationName,LatAndLon),
        Time = calendar:local_time(),
        pollutionserver:addValue(StationName,Time,"Temp",10),

        ?assertMatch("There was no such measurement!",pollution:getOneValue(StationName,Time,"PM10",
          pollutionserver:getMonitor()))
      end ()
    },
    { "Checks if you cannot get measurement from non-existing station",
      fun () ->
        pollutionserver:reset(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        pollutionserver:addStation(StationName,LatAndLon),
        Time = calendar:local_time(),
        pollutionserver:addValue(StationName,Time,"Temp",10),

        ?assertMatch("There is no such station!",pollution:getOneValue("Krakow",Time,"Temp",
          pollutionserver:getMonitor()))
      end ()
    },
    { "Ends server",
      fun () ->
        pollutionserver:stop(),
        ?assertMatch(1,1)
      end ()
    }
  ].

% this helping method generates Monitor with some stations and measurements in it
generateMonitor() ->
  Time = calendar:local_time(),
  pollutionserver:reset(),
  pollutionserver:addStation("Rybnik",{50.1022,18.5463}),
  pollutionserver:addStation("Krakow",{50.0647,19.9450}),
  pollutionserver:addValue("Rybnik",Time,"Temp",10),
  pollutionserver:addValue("Rybnik",Time,"Temp",-25),
  pollutionserver:addValue("Rybnik",Time,"PM10",100),
  pollutionserver:addValue("Rybnik",Time,"PM10",500),
  pollutionserver:addValue("Krakow",Time,"PM10",125),
  pollutionserver:addValue("Krakow",Time,"PM10",1000),
  pollutionserver:addValue("Krakow",Time,"Temp",2),
  pollutionserver:addValue("Krakow",Time,"Temp",-10),
  Time.


% here are tests that test if methods manipulating data are working correctly
data_manip_test() ->
  [
    { "Starts server",
      pollutionserver:start(),
      ?assertMatch(1,1)
    },
    { "Checks if the mean value is correct for the station",
      fun () ->
        _ = generateMonitor(),
        {_,LatAndLon1,_} = lists:nth(1,pollutionserver:getMonitor()),
        {_,LatAndLon2,_} = lists:nth(2,pollutionserver:getMonitor()),
        ?assertMatch(-4.0,pollutionserver:getStationMean(LatAndLon1,"Temp")),
        ?assertMatch(562.5,pollutionserver:getStationMean(LatAndLon1,"PM10")),
        ?assertMatch(-7.5,pollutionserver:getStationMean(LatAndLon2,"Temp")),
        ?assertMatch(300.0,pollutionserver:getStationMean(LatAndLon2,"PM10"))
      end ()
    },
    { "Checks if the mean value of non-existing station cannot be calculated",
      fun () ->
        pollutionserver:reset(),
        ?assertMatch("There is no such station!",pollutionserver:getStationMean({50,30},"Temp"))
      end ()
    },
    { "Checks if the mean value of non-existing measurement type cannot be calculated",
      fun () ->
        _ = generateMonitor(),
        {_,LatAndLon1,_} = lists:nth(1,pollutionserver:getMonitor()),
        {_,LatAndLon2,_} = lists:nth(2,pollutionserver:getMonitor()),
        ?assertMatch("There are no measurement of this type!",pollutionserver:getStationMean(LatAndLon1,"PM2,5")),
        ?assertMatch("There are no measurement of this type!",pollutionserver:getStationMean(LatAndLon2,"PM2,5"))
      end ()
    },
    { "Checks if the daily mean value of measurement type is calculated correctly",
      fun () ->
        DateFull = generateMonitor(),
        {Date, _} = DateFull,
        ?assertMatch(-5.75,pollutionserver:getDailyMean(Date,"Temp")),
        ?assertMatch(431.25,pollutionserver:getDailyMean(Date,"PM10"))
      end ()
    },
    { "Checks if the given parameter amplitude is calculated correctly",
      fun () ->
        Date = generateMonitor(),
        {_,LatAndLon1,_} = lists:nth(1,pollutionserver:getMonitor()),
        {_,LatAndLon2,_} = lists:nth(2,pollutionserver:getMonitor()),
        ?assertMatch({{"Amplitude",12},{"Max Value",2,Date},{"Min Value",-10,Date}},
          pollutionserver:getParameterAmplitude(LatAndLon1,"Temp")),
        ?assertMatch({{"Amplitude",875},{"Max Value",1000,Date},{"Min Value",125,Date}},
          pollutionserver:getParameterAmplitude(LatAndLon1,"PM10")),
        ?assertMatch({{"Amplitude",35},{"Max Value",10,Date},{"Min Value",-25,Date}},
          pollutionserver:getParameterAmplitude(LatAndLon2,"Temp")),
        ?assertMatch({{"Amplitude",400},{"Max Value",500,Date},{"Min Value",100,Date}},
          pollutionserver:getParameterAmplitude(LatAndLon2,"PM10"))
      end ()
    },
    { "Checks if the given parameter amplitude cannot be calculated on non-existing station",
      fun () ->
        _ = generateMonitor(),
        ?assertMatch("There is no such station!",pollutionserver:getParameterAmplitude("Rybik","PM10")),
        ?assertMatch("There is no such station!",pollutionserver:getParameterAmplitude("Kakow","PM10"))
      end ()
    },
    { "Checks if the given parameter amplitude cannot be calculated for non-existing parameter",
      fun () ->
        _ = generateMonitor(),
        {_,LatAndLon1,_} = lists:nth(1,pollutionserver:getMonitor()),
        {_,LatAndLon2,_} = lists:nth(2,pollutionserver:getMonitor()),
        ?assertMatch("There are no measurement of this type!",pollutionserver:getParameterAmplitude(LatAndLon1,"PM105")),
        ?assertMatch("There are no measurement of this type!",pollutionserver:getParameterAmplitude(LatAndLon2,"PM105"))
      end ()
    },
    { "Checks if the given station info is not printed for non existing station",
      fun () ->
        _ = generateMonitor(),
        ?assertMatch("There is no such station!",pollutionserver:getInfoStation("Rybik")),
        ?assertMatch("There is no such station!",pollutionserver:getInfoStation("Krakw"))
      end ()
    },
    { "Checks if the given station info is not printed for station with no measurements",
      fun () ->
        pollutionserver:reset(),
        pollutionserver:addStation("Rybnik",{50,30}),
        ?assertMatch("There are no measurements!",pollution:getInfoStation("Rybnik",pollutionserver:getMonitor()))
      end ()
    },
    { "Checks if the given station daily info is not printed for non existing station",
      fun () ->
        Date = generateMonitor(),
        ?assertMatch("There is no such station!",pollutionserver:getDailyInfoStation("Rybik",Date)),
        ?assertMatch("There is no such station!",pollutionserver:getDailyInfoStation("Krakw",Date))
      end ()
    },
    { "Checks if the given station daily info is not printed for station with no measurements",
      fun () ->
        pollutionserver:reset(),
        pollutionserver:addStation("Rybnik",{50,30}),
        ?assertMatch("There are no measurements!",pollutionserver:getDailyInfoStation("Rybnik",{2020,9,3})),
        ?assertMatch("There are no measurements!",pollutionserver:getDailyInfoStation("Rybnik",{2020,9,3}))
      end ()
    },
    { "Ends server",
      fun () ->
        pollutionserver:stop(),
        ?assertMatch(1,1)
      end ()
    }
  ].