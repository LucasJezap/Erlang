-module(pollution_tests).
-author("Lukasz Jezapkowicz").

-include_lib("eunit/include/eunit.hrl").

% here are tests that test if structure is building properly
structure_manip_test() ->
  [
    { "Checks if empty monitor is correctly build",
      fun () ->
        Monitor = pollution:createMonitor(),
        ?assertMatch([],Monitor)
      end ()
    },
    { "Checks if station is correctly added",
      fun () ->
        Monitor = pollution:createMonitor(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        NewMonitor = pollution:addStation(StationName,LatAndLon,Monitor),

        ?assertMatch(true,lists:member({StationName,LatAndLon,sets:new()},NewMonitor))
      end ()
    },
    { "Checks if station cannot be duplicated",
      fun () ->
        Monitor = pollution:createMonitor(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        M1 = pollution:addStation(StationName,LatAndLon,Monitor),
        M2 = pollution:addStation(StationName,LatAndLon,M1),

        ?assertMatch(true,lists:member({StationName,LatAndLon,sets:new()},M2)),
        ?assertMatch(1,length(M2))
      end ()
    },
    { "Checks if some measurement is correctly added to the Station",
      fun () ->
        Monitor = pollution:createMonitor(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        M1 = pollution:addStation(StationName,LatAndLon,Monitor),
        Time = calendar:local_time(),
        M2 = pollution:addValue(StationName,Time,"Temp",10,M1),
        [{_,_,S}] = [{A,B,C} || {A,B,C} <- M2, A == StationName],

        ?assertMatch(true,sets:is_element({"Temp",10,Time},S))
      end ()
    },
    { "Checks if some measurement cannot be duplicated in the Station",
      fun () ->
        Monitor = pollution:createMonitor(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        M1 = pollution:addStation(StationName,LatAndLon,Monitor),
        Time = calendar:local_time(),
        M2 = pollution:addValue(StationName,Time,"Temp",10,M1),
        M3 = pollution:addValue(StationName,Time,"Temp",10,M2),
        [{_,_,S}] = [{A,B,C} || {A,B,C} <- M3, A == StationName],

        ?assertMatch(true,sets:is_element({"Temp",10,Time},S)),
        ?assertMatch(1,sets:size(S))
      end ()
    },
    { "Checks if some measurement cannot be added to non-existing Station",
      fun () ->
        Monitor = pollution:createMonitor(),
        StationName = "Rybnik",
        Time = calendar:local_time(),
        M1 = pollution:addValue(StationName,Time,"Temp",10,Monitor),

        ?assertMatch(0,length(M1))
      end ()
      },
    { "Checks if some measurement can be deleted from the Station",
      fun () ->
        Monitor = pollution:createMonitor(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        M1 = pollution:addStation(StationName,LatAndLon,Monitor),
        Time = calendar:local_time(),
        M2 = pollution:addValue(StationName,Time,"Temp",10,M1),
        M3 = pollution:removeValue(StationName,Time,"Temp",M2),
        [{_,_,S}] = [{A,B,C} || {A,B,C} <- M3, A == StationName],

        ?assertMatch(false,sets:is_element({"Temp",10,Time},S))
      end ()
    },
    { "Checks if some measurement cannot be deleted from the non-existing station",
      fun () ->
        Monitor = pollution:createMonitor(),
        StationName = "Rybnik",
        Time = calendar:local_time(),
        M1 = pollution:removeValue(StationName,Time,"Temp",Monitor),

        ?assertMatch(0,length(M1))
      end ()
    },
    { "Checks if you can get existing measurement",
      fun () ->
        Monitor = pollution:createMonitor(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        M1 = pollution:addStation(StationName,LatAndLon,Monitor),
        Time = calendar:local_time(),
        M2 = pollution:addValue(StationName,Time,"Temp",10,M1),

        ?assertMatch(10,pollution:getOneValue(StationName,Time,"Temp",M2))
      end ()
    },
    { "Checks if you cannot get non-existing measurement from existing station",
      fun () ->
        Monitor = pollution:createMonitor(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        M1 = pollution:addStation(StationName,LatAndLon,Monitor),
        Time = calendar:local_time(),
        M2 = pollution:addValue(StationName,Time,"Temp",10,M1),

        ?assertMatch("There was no such measurement!",pollution:getOneValue(StationName,Time,"PM10",M2))
      end ()
    },
    { "Checks if you cannot get measurement from non-existing station",
      fun () ->
        Monitor = pollution:createMonitor(),
        StationName = "Rybnik",
        LatAndLon = {50.1022,18.5463},
        M1 = pollution:addStation(StationName,LatAndLon,Monitor),
        Time = calendar:local_time(),
        M2 = pollution:addValue(StationName,Time,"Temp",10,M1),

        ?assertMatch("There is no such station!",pollution:getOneValue("Krakow",Time,"Temp",M2))
      end ()
    }
  ].

% this helping method generates Monitor with some stations and measurements in it
generateMonitor() ->
  Time = calendar:local_time(),
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Rybnik",{50.1022,18.5463},P),
  P2 = pollution:addStation("Krakow",{50.0647,19.9450},P1),
  P3 = pollution:addValue("Rybnik",Time,"Temp",10,P2),
  P4 = pollution:addValue("Rybnik",Time,"Temp",-25,P3),
  P5 = pollution:addValue("Rybnik",Time,"PM10",100,P4),
  P6 = pollution:addValue("Rybnik",Time,"PM10",500,P5),
  P7 = pollution:addValue("Krakow",Time,"PM10",125,P6),
  P8 = pollution:addValue("Krakow",Time,"PM10",1000,P7),
  P9 = pollution:addValue("Krakow",Time,"Temp",2,P8),
  P10 = pollution:addValue("Krakow",Time,"Temp",-10,P9),
  {P10, Time}.

% here are tests that test if methods manipulating data are working correctly
data_manip_test() ->
  [
    { "Checks if the mean value is correct for the station",
      fun () ->
        {Monitor,_} = generateMonitor(),
        {_,LatAndLon1,_} = lists:nth(1,Monitor),
        {_,LatAndLon2,_} = lists:nth(2,Monitor),
        ?assertMatch(-4.0,pollution:getStationMean(LatAndLon1,"Temp",Monitor)),
        ?assertMatch(562.5,pollution:getStationMean(LatAndLon1,"PM10",Monitor)),
        ?assertMatch(-7.5,pollution:getStationMean(LatAndLon2,"Temp",Monitor)),
        ?assertMatch(300.0,pollution:getStationMean(LatAndLon2,"PM10",Monitor))
      end ()
    },
    { "Checks if the mean value of non-existing station cannot be calculated",
      fun () ->
        Monitor = pollution:createMonitor(),
        ?assertMatch("There is no such station!",pollution:getStationMean({50,30},"Temp",Monitor))
      end ()
    },
    { "Checks if the mean value of non-existing measurement type cannot be calculated",
      fun () ->
        {Monitor, _} = generateMonitor(),
        {_,LatAndLon1,_} = lists:nth(1,Monitor),
        {_,LatAndLon2,_} = lists:nth(2,Monitor),
        ?assertMatch("There are no measurement of this type!",pollution:getStationMean(LatAndLon1,"PM2,5",Monitor)),
        ?assertMatch("There are no measurement of this type!",pollution:getStationMean(LatAndLon2,"PM2,5",Monitor))
      end ()
    },
    { "Checks if the daily mean value of measurement type is calculated correctly",
      fun () ->
        {Monitor,DateFull} = generateMonitor(),
        {Date, _} = DateFull,
        ?assertMatch(-5.75,pollution:getDailyMean(Date,"Temp",Monitor)),
        ?assertMatch(431.25,pollution:getDailyMean(Date,"PM10",Monitor))
      end ()
    },
    { "Checks if the daily mean value of non-existing measurement type or wrong Date is tried to be calculated,
        it throws an error",
      fun () ->
        {Monitor,DateFull} = generateMonitor(),
        {Date, _} = DateFull,
        ?assertError(badarith,pollution:getDailyMean(Date,"PM2,5",Monitor)),
        ?assertError(badarith,pollution:getDailyMean({2021,12,12},"PM10",Monitor))
      end ()
    },
    { "Checks if the given parameter amplitude is calculated correctly",
      fun () ->
        {Monitor,Date} = generateMonitor(),
        {_,LatAndLon1,_} = lists:nth(1,Monitor),
        {_,LatAndLon2,_} = lists:nth(2,Monitor),
        ?assertMatch({{"Amplitude",12},{"Max Value",2,Date},{"Min Value",-10,Date}},
          pollution:getParameterAmplitude(LatAndLon1,"Temp",Monitor)),
        ?assertMatch({{"Amplitude",875},{"Max Value",1000,Date},{"Min Value",125,Date}},
          pollution:getParameterAmplitude(LatAndLon1,"PM10",Monitor)),
        ?assertMatch({{"Amplitude",35},{"Max Value",10,Date},{"Min Value",-25,Date}},
          pollution:getParameterAmplitude(LatAndLon2,"Temp",Monitor)),
        ?assertMatch({{"Amplitude",400},{"Max Value",500,Date},{"Min Value",100,Date}},
          pollution:getParameterAmplitude(LatAndLon2,"PM10",Monitor))
      end ()
    },
    { "Checks if the given parameter amplitude cannot be calculated on non-existing station",
      fun () ->
        {Monitor,_} = generateMonitor(),
        ?assertMatch("There is no such station!",pollution:getParameterAmplitude("Rybik","PM10",Monitor)),
        ?assertMatch("There is no such station!",pollution:getParameterAmplitude("Kakow","PM10",Monitor))
      end ()
    },
    { "Checks if the given parameter amplitude cannot be calculated for non-existing parameter",
      fun () ->
        {Monitor,_} = generateMonitor(),
        {_,LatAndLon1,_} = lists:nth(1,Monitor),
        {_,LatAndLon2,_} = lists:nth(2,Monitor),
        ?assertMatch("There are no measurement of this type!",pollution:getParameterAmplitude(LatAndLon1,"PM105",Monitor)),
        ?assertMatch("There are no measurement of this type!",pollution:getParameterAmplitude(LatAndLon2,"PM105",Monitor))
      end ()
    },
    { "Checks if the given station info is not printed for non existing station",
      fun () ->
        {Monitor,_} = generateMonitor(),
        ?assertMatch("There is no such station!",pollution:getInfoStation("Rybik",Monitor)),
        ?assertMatch("There is no such station!",pollution:getInfoStation("Krakw",Monitor))
      end ()
    },
    { "Checks if the given station info is not printed for station with no measurements",
      fun () ->
        Monitor = pollution:createMonitor(),
        M1 = pollution:addStation("Rybnik",{50,30},Monitor),
        ?assertMatch("There are no measurements!",pollution:getInfoStation("Rybnik",M1))
      end ()
    },
    { "Checks if the given station daily info is not printed for non existing station",
      fun () ->
        {Monitor,Date} = generateMonitor(),
        ?assertMatch("There is no such station!",pollution:getDailyInfoStation("Rybik",Date,Monitor)),
        ?assertMatch("There is no such station!",pollution:getDailyInfoStation("Krakw",Date,Monitor))
      end ()
    },
    { "Checks if the given station daily info is not printed for station with no measurements",
      fun () ->
        Monitor = pollution:createMonitor(),
        M1 = pollution:addStation("Rybnik",{50,30},Monitor),
        ?assertMatch("There are no measurements!",pollution:getDailyInfoStation("Rybnik",{2020,9,3},M1)),
        ?assertMatch("There are no measurements!",pollution:getDailyInfoStation("Rybnik",{2020,9,3},M1))
      end ()
    }
  ].