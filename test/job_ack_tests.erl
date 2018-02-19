%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2018 Pentland Edge Ltd.
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

-module(job_ack_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the Job Acknowledge segment functions. 
job_ack_test_() ->
    Bin = sample_job_ack(),
    {ok, JA} = job_ack:decode(Bin),
    ExpectedMode = {attack_control_with_tracking, joint_stars},
    [?_assertEqual(16#12345678, job_ack:get_job_id(JA)),
     ?_assertEqual("Job Ack ID", job_ack:get_requestor_id(JA)),
     ?_assertEqual("JAckTaskID", job_ack:get_requestor_task_id(JA)),
     ?_assertEqual(vader, job_ack:get_sensor_id_type(JA)),
     ?_assertEqual("Darth ", job_ack:get_sensor_id_model(JA)),
     ?_assertEqual(99, job_ack:get_radar_priority(JA)),
     ?_assert(almost_equal(45.0, job_ack:get_bounding_a_lat(JA), 0.00001)),
     ?_assert(almost_equal(345.0, job_ack:get_bounding_a_lon(JA), 0.00001)),
     ?_assert(almost_equal(45.0, job_ack:get_bounding_b_lat(JA), 0.00001)),
     ?_assert(almost_equal(345.0, job_ack:get_bounding_b_lon(JA), 0.00001)),
     ?_assert(almost_equal(45.0, job_ack:get_bounding_c_lat(JA), 0.00001)),
     ?_assert(almost_equal(345.0, job_ack:get_bounding_c_lon(JA), 0.00001)),
     ?_assert(almost_equal(45.0, job_ack:get_bounding_d_lat(JA), 0.00001)),
     ?_assert(almost_equal(345.0, job_ack:get_bounding_d_lon(JA), 0.00001)),
     ?_assertEqual(ExpectedMode, job_ack:get_radar_mode(JA)),
     ?_assertEqual(16#123, job_ack:get_duration(JA)),
     ?_assertEqual(16#1234, job_ack:get_revisit_interval(JA)),
     ?_assertEqual(approved_with_modification, job_ack:get_request_status(JA)),
     ?_assertEqual(2018, job_ack:get_start_year(JA)),
     ?_assertEqual(5, job_ack:get_start_month(JA)),
     ?_assertEqual(23, job_ack:get_start_day(JA)),
     ?_assertEqual(10, job_ack:get_start_hour(JA)),
     ?_assertEqual(11, job_ack:get_start_min(JA)),
     ?_assertEqual(50, job_ack:get_start_sec(JA)),
     ?_assertEqual("XN", job_ack:get_requestor_nationality(JA))
    ].

%% Return a binary job acknowledge segment to use as test data.
sample_job_ack() ->
    <<16#12345678:32,
      "Job Ack ID",
      "JAckTaskID",
      27,
      "Darth ",
      99,
      64,0,0,0, 245,85,85,85, 64,0,0,0, 245,85,85,85, 
      64,0,0,0, 245,85,85,85, 64,0,0,0, 245,85,85,85, 
      21, 
      16#123:16, 
      16#1234:16,
      2,
      2018:16,5,23,10,11,50,
      "XN">>.

%% Utility function to compare whether floating point values are within a
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.

