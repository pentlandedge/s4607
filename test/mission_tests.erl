%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2016 Pentland Edge Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. 
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software 
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT 
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the 
%% License for the specific language governing permissions and limitations 
%% under the License.
%%

-module(mission_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the decoding of the mission segment. 
mission_test_() ->
    [mission_plan_checks(), flight_plan_checks(), platform_type_checks(),
     platform_config_checks(), reference_time_checks(), 
     encode_decode_checks1(), decode_platform_type_checks()].

mission_plan_checks() ->
    {ok, MS1} = mission:decode(sample_mission_seg1()),
    {ok, MS2} = mission:decode(sample_mission_seg2()),
    [?_assertEqual("Global Domin", mission:get_mission_plan(MS1)),
     ?_assertEqual("Short", mission:get_mission_plan(MS2))].

flight_plan_checks() ->
    {ok, MS1} = mission:decode(sample_mission_seg1()),
    {ok, MS2} = mission:decode(sample_mission_seg2()),
    [?_assertEqual("Fly By", mission:get_flight_plan(MS1)),
     ?_assertEqual("Full length1", mission:get_flight_plan(MS2))].

platform_type_checks() ->
    B1 = sample_mission_seg1(),
    % Hack the platform type to get new test samples.
    B2 = binary:replace(B1, <<36>>, <<0>>, [{scope, {24,1}}]),
    B3 = binary:replace(B1, <<36>>, <<255>>, [{scope, {24,1}}]),
    B4 = binary:replace(B1, <<36>>, <<127>>, [{scope, {24,1}}]),
    B5 = binary:replace(B1, <<36>>, <<9>>, [{scope, {24,1}}]),
    B6 = binary:replace(B1, <<36>>, <<39>>, [{scope, {24,1}}]),
    {ok, MS1} = mission:decode(B1),
    {ok, MS2} = mission:decode(B2),
    {ok, MS3} = mission:decode(B3),
    {ok, MS4} = mission:decode(B4),
    {ok, MS5} = mission:decode(B5),
    {ok, MS6} = mission:decode(B6),
    [?_assertEqual(reaper, mission:get_platform_type(MS1)),
     ?_assertEqual(unidentified, mission:get_platform_type(MS2)),
     ?_assertEqual(future_use, mission:get_platform_type(MS4)),
     ?_assertEqual(predator, mission:get_platform_type(MS5)),
     ?_assertEqual(twin_otter, mission:get_platform_type(MS6)),
     ?_assertEqual(other, mission:get_platform_type(MS3))].

platform_config_checks() ->
    {ok, MS1} = mission:decode(sample_mission_seg1()),
    {ok, MS2} = mission:decode(sample_mission_seg2()),
    [?_assertEqual("Skynet v12", mission:get_platform_config(MS1)),
     ?_assertEqual("Skynet", mission:get_platform_config(MS2))].
    
reference_time_checks() ->
    {ok, MS1} = mission:decode(sample_mission_seg1()),
    {ok, MS2} = mission:decode(sample_mission_seg2()),
    [?_assertEqual(2015, mission:get_year(MS1)),
     ?_assertEqual(12, mission:get_month(MS1)),
     ?_assertEqual(31, mission:get_day(MS1)),
     ?_assertEqual(1997, mission:get_year(MS2)),
     ?_assertEqual(8, mission:get_month(MS2)),
     ?_assertEqual(29, mission:get_day(MS2))].
   
encode_decode_checks1() ->
    MS = mission:new("Drifter 1", "A1234", other, "Build 1", 2016, 2, 5),
    EMS = mission:encode(MS),
    {ok, DEMS} = mission:decode(EMS),
    [?_assertEqual("Drifter 1", mission:get_mission_plan(DEMS)),
     ?_assertEqual("A1234", mission:get_flight_plan(DEMS)),
     ?_assertEqual(other, mission:get_platform_type(DEMS)),
     ?_assertEqual("Build 1", mission:get_platform_config(DEMS)),
     ?_assertEqual(2016, mission:get_year(DEMS)),
     ?_assertEqual(2, mission:get_month(DEMS)),
     ?_assertEqual(5, mission:get_day(DEMS))].

decode_platform_type_checks() ->
    [?_assertEqual(unidentified, mission:decode_platform_type(0)),
     ?_assertEqual(paul_revere, mission:decode_platform_type(23)),
     ?_assertEqual(mariner_uav, mission:decode_platform_type(24)),
     ?_assertEqual(bac_111, mission:decode_platform_type(25)),
     ?_assertEqual(coyote, mission:decode_platform_type(26)),
     ?_assertEqual(king_air, mission:decode_platform_type(27)),
     ?_assertEqual(limit, mission:decode_platform_type(28)),
     ?_assertEqual(nrl_np_3b, mission:decode_platform_type(29)),
     ?_assertEqual(sostar_x, mission:decode_platform_type(30)),
     ?_assertEqual(watchkeeper, mission:decode_platform_type(31)),
     ?_assertEqual(alliance_ground_surveillance, mission:decode_platform_type(32)),
     ?_assertEqual(stryker, mission:decode_platform_type(33)),
     ?_assertEqual(ags_hale_uav, mission:decode_platform_type(34)),
     ?_assertEqual(sidm, mission:decode_platform_type(35)),
     ?_assertEqual(reaper, mission:decode_platform_type(36)),
     ?_assertEqual(warrior_a, mission:decode_platform_type(37)),
     ?_assertEqual(warrior, mission:decode_platform_type(38))].

sample_mission_seg1() ->
    <<"Global Domin","Fly By      ",36,"Skynet v12",16#07, 16#DF, 12, 31>>.

sample_mission_seg2() ->
    <<"Short       ","Full length1",36,"Skynet    ",16#07, 16#CD, 08, 29>>.
