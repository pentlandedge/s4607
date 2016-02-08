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

-module(job_def_tests).

-export([sample_job_def/0]).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the decoding of the mission segment. 
job_def_test_() ->
    [job1_checks()].

job1_checks() ->
    {ok, JD1} = job_def:decode(job_def1()),
    [?_assertEqual(16909060, job_def:get_job_id(JD1)),
     ?_assertEqual(global_hawk_sensor, job_def:get_sensor_id_type(JD1)),
     ?_assertEqual("Model1", job_def:get_sensor_id_model(JD1)),
     ?_assertEqual(no_filtering, job_def:get_target_filt_flag(JD1)),
     ?_assertEqual(23, job_def:get_priority(JD1)),
     ?_assertEqual(flat_earth, job_def:get_geoid_model(JD1))].

job_def1() ->
    <<1,2,3,4, 5, "Model1", 0, 23, 
      64,0,0,0, "õUUU", 64,0,0,0, "õUUU", 64,0,0,0, "õUUU", 64,0,0,0, "õUUU",
      1, 1,0, 255,255, 1,0, 16#27,16#10, 45, 0,128, 
      255,255, 127,74, 0,100, 5, 90, 3, 1, 3>>.

sample_job_def() ->
    P = [{job_id, 100}, {sensor_id_type, rotary_wing_radar},
         {sensor_id_model, "Heli 1"}, {target_filt_flag, []}, {priority, 30},
         {bounding_a_lat, 33.3}, {bounding_a_lon, 3.45},
         {bounding_b_lat, 23.4}, {bounding_b_lon, 350},
         {bounding_c_lat, -45.0}, {bounding_c_lon, 2.45},
         {bounding_d_lat, -60.0}, {bounding_d_lon, 140},
         {radar_mode, {monopulse_calibration, asars_aip}}, {nom_rev_int, 65000},
         {ns_pos_unc_along_track, no_statement}],

    job_def:new(P).

