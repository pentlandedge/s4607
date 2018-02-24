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
    [decode_checks(), new_default_checks(), encode_decode_checks(),
     request_status_checks()].

decode_checks() ->
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

new_default_checks() ->
    JA = job_ack:new([]),
    [?_assertEqual(1, job_ack:get_job_id(JA)),
     ?_assertEqual("          ", job_ack:get_requestor_id(JA)),
     ?_assertEqual("          ", job_ack:get_requestor_task_id(JA)),
     ?_assertEqual(no_statement, job_ack:get_sensor_id_type(JA)),
     ?_assertEqual(no_statement, job_ack:get_sensor_id_model(JA)),
     ?_assertEqual(99, job_ack:get_radar_priority(JA)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_a_lat(JA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_a_lon(JA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_b_lat(JA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_b_lon(JA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_c_lat(JA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_c_lon(JA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_d_lat(JA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_d_lon(JA), 0.00001)),
     ?_assertEqual({unspecified_mode, generic}, job_ack:get_radar_mode(JA)),
     ?_assertEqual(continuous, job_ack:get_duration(JA)),
     ?_assertEqual(default_interval, job_ack:get_revisit_interval(JA)),
     ?_assertEqual(approved, job_ack:get_request_status(JA)),
     ?_assertEqual(2000, job_ack:get_start_year(JA)),
     ?_assertEqual(1, job_ack:get_start_month(JA)),
     ?_assertEqual(1, job_ack:get_start_day(JA)),
     ?_assertEqual(0, job_ack:get_start_hour(JA)),
     ?_assertEqual(0, job_ack:get_start_min(JA)),
     ?_assertEqual(0, job_ack:get_start_sec(JA)),
     ?_assertEqual("XN", job_ack:get_requestor_nationality(JA))
    ].

%% Create a new segment, encode then decode it and check that the resulting 
%% record matches the expected values.
encode_decode_checks() ->
    JA = job_ack:new([]),
    EJA = job_ack:encode(JA),
    FEJA = iolist_to_binary(EJA),
    {ok, DEJA} = job_ack:decode(FEJA),

    [?_assertEqual(1, job_ack:get_job_id(DEJA)),
     ?_assertEqual("          ", job_ack:get_requestor_id(DEJA)),
     ?_assertEqual("          ", job_ack:get_requestor_task_id(DEJA)),
     ?_assertEqual(no_statement, job_ack:get_sensor_id_type(DEJA)),
     ?_assertEqual(no_statement, job_ack:get_sensor_id_model(DEJA)),
     ?_assertEqual(99, job_ack:get_radar_priority(DEJA)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_a_lat(DEJA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_a_lon(DEJA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_b_lat(DEJA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_b_lon(DEJA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_c_lat(DEJA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_c_lon(DEJA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_d_lat(DEJA), 0.00001)),
     ?_assert(almost_equal(0.0, job_ack:get_bounding_d_lon(DEJA), 0.00001)),
     ?_assertEqual({unspecified_mode, generic}, job_ack:get_radar_mode(DEJA)),
     ?_assertEqual(continuous, job_ack:get_duration(DEJA)),
     ?_assertEqual(default_interval, job_ack:get_revisit_interval(DEJA)),
     ?_assertEqual(approved, job_ack:get_request_status(DEJA)),
     ?_assertEqual(2000, job_ack:get_start_year(DEJA)),
     ?_assertEqual(1, job_ack:get_start_month(DEJA)),
     ?_assertEqual(1, job_ack:get_start_day(DEJA)),
     ?_assertEqual(0, job_ack:get_start_hour(DEJA)),
     ?_assertEqual(0, job_ack:get_start_min(DEJA)),
     ?_assertEqual(0, job_ack:get_start_sec(DEJA)),
     ?_assertEqual("XN", job_ack:get_requestor_nationality(DEJA))
    ].

request_status_checks() ->
    [?_assertEqual(request, job_ack:decode_request_status(0)),
     ?_assertEqual(approved, job_ack:decode_request_status(1)),
     ?_assertEqual(approved_with_modification, job_ack:decode_request_status(2)),
     ?_assertEqual(denied_line_of_sight, job_ack:decode_request_status(3)),
     ?_assertEqual(denied_timeline, job_ack:decode_request_status(4)),
     ?_assertEqual(denied_orbit, job_ack:decode_request_status(5)),
     ?_assertEqual(denied_priority, job_ack:decode_request_status(6)),
     ?_assertEqual(denied_area_of_interest, job_ack:decode_request_status(7)),
     ?_assertEqual(denied_illegal_request, job_ack:decode_request_status(8)),
     ?_assertEqual(denied_function_inoperative, job_ack:decode_request_status(9)),
     ?_assertEqual(denied_other, job_ack:decode_request_status(10)),
     ?_assertEqual(0, job_ack:encode_request_status(request)),
     ?_assertEqual(1, job_ack:encode_request_status(approved)),
     ?_assertEqual(2, job_ack:encode_request_status(approved_with_modification)),
     ?_assertEqual(3, job_ack:encode_request_status(denied_line_of_sight)),
     ?_assertEqual(4, job_ack:encode_request_status(denied_timeline)),
     ?_assertEqual(5, job_ack:encode_request_status(denied_orbit)), 
     ?_assertEqual(6, job_ack:encode_request_status(denied_priority)),
     ?_assertEqual(7, job_ack:encode_request_status(denied_area_of_interest)),
     ?_assertEqual(8, job_ack:encode_request_status(denied_illegal_request)),
     ?_assertEqual(9, job_ack:encode_request_status(denied_function_inoperative)),
     ?_assertEqual(10, job_ack:encode_request_status(denied_other))
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

