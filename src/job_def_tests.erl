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

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the decoding of the mission segment. 
job_def_test_() ->
    [job1_checks()].

job1_checks() ->
    JD1 = job_def:decode(job_def1()),
    [?_assertEqual(16909060, job_def:get_job_id(JD1)),
     ?_assertEqual(global_hawk_sensor, job_def:get_sensor_id_type(JD1)),
     ?_assertEqual(flat_earth, job_def:get_geoid_model(JD1))].

job_def1() ->
    <<1,2,3,4, 5, "Model1", 0, 23, 
      64,0,0,0, "õUUU", 64,0,0,0, "õUUU", 64,0,0,0, "õUUU", 64,0,0,0, "õUUU",
      1, 1,0, 255,255, 1,0, 16#27,16#10, 45, 0,128, 
      255,255, 127,74, 0,100, 5, 90, 3, 1, 3>>.
