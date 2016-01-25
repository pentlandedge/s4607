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
    [mission_plan_checks(), flight_plan_checks(), platform_type_checks()].

mission_plan_checks() ->
    MS1 = mission:decode(sample_mission_seg1()),
    MS2 = mission:decode(sample_mission_seg2()),
    [?_assertEqual("Global Domin", mission:get_mission_plan(MS1)),
     ?_assertEqual("Short", mission:get_mission_plan(MS2))].

flight_plan_checks() ->
    MS1 = mission:decode(sample_mission_seg1()),
    MS2 = mission:decode(sample_mission_seg2()),
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
    MS1 = mission:decode(B1),
    MS2 = mission:decode(B2),
    MS3 = mission:decode(B3),
    MS4 = mission:decode(B4),
    MS5 = mission:decode(B5),
    MS6 = mission:decode(B6),
    [?_assertEqual(reaper, mission:get_platform_type(MS1)),
     ?_assertEqual(unidentified, mission:get_platform_type(MS2)),
     ?_assertEqual(future_use, mission:get_platform_type(MS4)),
     ?_assertEqual(predator, mission:get_platform_type(MS5)),
     ?_assertEqual(twin_otter, mission:get_platform_type(MS6)),
     ?_assertEqual(other, mission:get_platform_type(MS3))].

sample_mission_seg1() ->
    <<"Global Domin","Fly By      ",36,"Skynet v12",16#07, 16#DF, 12, 31>>.

sample_mission_seg2() ->
    <<"Short       ","Full length1",36,"Skynet v12",16#07, 16#DF, 12, 31>>.
