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
     encode_decode_checks1(), decode_platform_type_checks(),
     encode_platform_type_checks()].

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
    [?_assertEqual(mq_9_reaper, mission:get_platform_type(MS1)),
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
     ?_assertEqual(29, mission:get_day(MS2)),
     ?_assertEqual({1997, 8, 29}, mission:get_time(MS2))].
   
encode_decode_checks1() ->
    MS = mission:new("Drifter 1", "A1234", other, "Build 1", 2016, 2, 5),
    mission:display(MS),
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
     ?_assertEqual(acs, mission:decode_platform_type(1)),
     ?_assertEqual(arl_m, mission:decode_platform_type(2)),
     ?_assertEqual(sentinel, mission:decode_platform_type(3)),
     ?_assertEqual(rotary_wing_radar, mission:decode_platform_type(4)),
     ?_assertEqual(global_hawk_navy, mission:decode_platform_type(5)),
     ?_assertEqual(horizon, mission:decode_platform_type(6)),
     ?_assertEqual(e_8, mission:decode_platform_type(7)),
     ?_assertEqual(p_3c, mission:decode_platform_type(8)),
     ?_assertEqual(predator, mission:decode_platform_type(9)),
     ?_assertEqual(radarsat2, mission:decode_platform_type(10)),
     ?_assertEqual(u_2, mission:decode_platform_type(11)),
     ?_assertEqual(e_10, mission:decode_platform_type(12)),
     ?_assertEqual(ugs_single, mission:decode_platform_type(13)),
     ?_assertEqual(ugs_cluster, mission:decode_platform_type(14)),
     ?_assertEqual(ground_based, mission:decode_platform_type(15)),
     ?_assertEqual(uav_army, mission:decode_platform_type(16)),
     ?_assertEqual(uav_marines, mission:decode_platform_type(17)),
     ?_assertEqual(uav_navy, mission:decode_platform_type(18)),
     ?_assertEqual(uav_air_force, mission:decode_platform_type(19)),
     ?_assertEqual(global_hawk_air_force, mission:decode_platform_type(20)),
     ?_assertEqual(global_hawk_australia, mission:decode_platform_type(21)),
     ?_assertEqual(global_hawk_germany, mission:decode_platform_type(22)),
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
     ?_assertEqual(mq_9_reaper, mission:decode_platform_type(36)),
     ?_assertEqual(warrior_a, mission:decode_platform_type(37)),
     ?_assertEqual(warrior, mission:decode_platform_type(38))].

encode_platform_type_checks() ->
    [?_assertEqual(<<0>>, mission:encode_platform_type(unidentified)),
     ?_assertEqual(<<1>>, mission:encode_platform_type(acs)),
     ?_assertEqual(<<2>>, mission:encode_platform_type(arl_m)),
     ?_assertEqual(<<3>>, mission:encode_platform_type(sentinel)),
     ?_assertEqual(<<4>>, mission:encode_platform_type(rotary_wing_radar)),
     ?_assertEqual(<<5>>, mission:encode_platform_type(global_hawk_navy)),
     ?_assertEqual(<<6>>, mission:encode_platform_type(horizon)),
     ?_assertEqual(<<7>>, mission:encode_platform_type(e_8)),
     ?_assertEqual(<<8>>, mission:encode_platform_type(p_3c)),
     ?_assertEqual(<<9>>, mission:encode_platform_type(predator)),
     ?_assertEqual(<<10>>, mission:encode_platform_type(radarsat2)),
     ?_assertEqual(<<11>>, mission:encode_platform_type(u_2)),
     ?_assertEqual(<<12>>, mission:encode_platform_type(e_10)),
     ?_assertEqual(<<13>>, mission:encode_platform_type(ugs_single)),
     ?_assertEqual(<<14>>, mission:encode_platform_type(ugs_cluster)),
     ?_assertEqual(<<15>>, mission:encode_platform_type(ground_based)),
     ?_assertEqual(<<16>>, mission:encode_platform_type(uav_army)),
     ?_assertEqual(<<17>>, mission:encode_platform_type(uav_marines)),
     ?_assertEqual(<<18>>, mission:encode_platform_type(uav_navy)),
     ?_assertEqual(<<19>>, mission:encode_platform_type(uav_air_force)),
     ?_assertEqual(<<20>>, mission:encode_platform_type(global_hawk_air_force)),
     ?_assertEqual(<<21>>, mission:encode_platform_type(global_hawk_australia)),
     ?_assertEqual(<<22>>, mission:encode_platform_type(global_hawk_germany)),
     ?_assertEqual(<<23>>, mission:encode_platform_type(paul_revere)),
     ?_assertEqual(<<24>>, mission:encode_platform_type(mariner_uav)),
     ?_assertEqual(<<25>>, mission:encode_platform_type(bac_111)),
     ?_assertEqual(<<26>>, mission:encode_platform_type(coyote)),
     ?_assertEqual(<<27>>, mission:encode_platform_type(king_air)),
     ?_assertEqual(<<28>>, mission:encode_platform_type(limit)),
     ?_assertEqual(<<29>>, mission:encode_platform_type(nrl_np_3b)),
     ?_assertEqual(<<30>>, mission:encode_platform_type(sostar_x)),
     ?_assertEqual(<<31>>, mission:encode_platform_type(watchkeeper)),
     ?_assertEqual(<<32>>, mission:encode_platform_type(alliance_ground_surveillance)),
     ?_assertEqual(<<33>>, mission:encode_platform_type(stryker)),
     ?_assertEqual(<<34>>, mission:encode_platform_type(ags_hale_uav)),
     ?_assertEqual(<<35>>, mission:encode_platform_type(sidm)),
     ?_assertEqual(<<36>>, mission:encode_platform_type(reaper)),
     ?_assertEqual(<<37>>, mission:encode_platform_type(warrior_a)),
     ?_assertEqual(<<38>>, mission:encode_platform_type(warrior)),
     ?_assertEqual(<<39>>, mission:encode_platform_type(twin_otter))].

sample_mission_seg1() ->
    <<"Global Domin","Fly By      ",36,"Skynet v12",16#07, 16#DF, 12, 31>>.

sample_mission_seg2() ->
    <<"Short       ","Full length1",36,"Skynet    ",16#07, 16#CD, 08, 29>>.
