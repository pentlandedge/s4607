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

-module(job_req_tests).

-include_lib("eunit/include/eunit.hrl").

-export([sample_params/0, sample_job_request/0, sample_job_request_params/0]).

%% Define a test generator for the Job Request segment functions. 
job_req_test_() ->
    [valid_checks(), valid_checks2(), new_checks(), encode_checks()].

valid_checks() ->
    Bin = sample_job_request(),
    {ok, JR} = job_req:decode(Bin),
    job_req:display(JR),
    ReqID = job_req:get_requestor_id(JR),
    TaskID = job_req:get_requestor_task_id(JR),
    Pri = job_req:get_requestor_priority(JR),
    Mode = job_req:get_radar_mode(JR),
    RangeRes = job_req:get_radar_range_res(JR),
    XRangeRes = job_req:get_radar_cross_range_res(JR),
    Yr = job_req:get_earliest_start_year(JR),
    Mth = job_req:get_earliest_start_month(JR),
    Day = job_req:get_earliest_start_day(JR),
    Hr = job_req:get_earliest_start_hour(JR),
    Min = job_req:get_earliest_start_min(JR),
    Sec = job_req:get_earliest_start_sec(JR),
    DelaySec = job_req:get_allowed_delay(JR),
    Duration = job_req:get_duration(JR),
    RevInt = job_req:get_revisit_interval(JR),
    SensorType = job_req:get_sensor_id_type(JR),
    SensorModel = job_req:get_sensor_id_model(JR),
    ReqType = job_req:get_request_type(JR),
    [?_assertEqual("Job Req ID", ReqID),
     ?_assertEqual("JReqTaskID", TaskID),
     ?_assertEqual(default_priority, Pri),
     ?_assert(almost_equal(45.0, job_req:get_bounding_a_lat(JR), 0.00001)),
     ?_assert(almost_equal(345.0, job_req:get_bounding_a_lon(JR), 0.00001)),
     ?_assert(almost_equal(45.0, job_req:get_bounding_b_lat(JR), 0.00001)),
     ?_assert(almost_equal(345.0, job_req:get_bounding_b_lon(JR), 0.00001)),
     ?_assert(almost_equal(45.0, job_req:get_bounding_c_lat(JR), 0.00001)),
     ?_assert(almost_equal(345.0, job_req:get_bounding_c_lon(JR), 0.00001)),
     ?_assert(almost_equal(45.0, job_req:get_bounding_d_lat(JR), 0.00001)),
     ?_assert(almost_equal(345.0, job_req:get_bounding_d_lon(JR), 0.00001)),
     ?_assertEqual({maritime_mti_low_res, joint_stars}, Mode),
     ?_assertEqual(300, RangeRes),
     ?_assertEqual(dont_care, XRangeRes),
     ?_assertEqual(2018, Yr),
     ?_assertEqual(5, Mth),
     ?_assertEqual(23, Day),
     ?_assertEqual(10, Hr),
     ?_assertEqual(11, Min),
     ?_assertEqual(50, Sec),
     ?_assertEqual(65000, DelaySec),
     ?_assertEqual(continuous, Duration),
     ?_assertEqual(default_interval, RevInt),
     ?_assertEqual(no_statement, SensorType),
     ?_assertEqual(no_statement, SensorModel),
     ?_assertEqual(initial_request, ReqType)
    ].

valid_checks2() ->
    Bin = sample_job_request2(),
    {ok, JR} = job_req:decode(Bin),
    Duration = job_req:get_duration(JR),
    RevInt = job_req:get_revisit_interval(JR),
    SensorType = job_req:get_sensor_id_type(JR),
    SensorModel = job_req:get_sensor_id_model(JR),
    ReqType = job_req:get_request_type(JR),
    [?_assertEqual(54321, Duration),
     ?_assertEqual(21314, RevInt),
     ?_assertEqual(global_hawk_sensor, SensorType),
     ?_assertEqual("HawkV1", SensorModel),
     ?_assertEqual(cancel_job, ReqType)
    ].

%% Test the construction of new job request segments.
new_checks() ->
    ParamList = sample_params(),
    JR = job_req:new(ParamList),
    [?_assertEqual("Hawkstream", job_req:get_requestor_id(JR)),
     ?_assertEqual("Test Run 1", job_req:get_requestor_task_id(JR)),
     ?_assertEqual(99, job_req:get_requestor_priority(JR)),
     ?_assert(almost_equal(56.36061, job_req:get_bounding_a_lat(JR), 0.00001)),
     ?_assert(almost_equal(-2.82458, job_req:get_bounding_a_lon(JR), 0.00001)),
     ?_assert(almost_equal(56.36070, job_req:get_bounding_b_lat(JR), 0.00001)),
     ?_assert(almost_equal(-2.81774, job_req:get_bounding_b_lon(JR), 0.00001)),
     ?_assert(almost_equal(56.35773, job_req:get_bounding_c_lat(JR), 0.00001)),
     ?_assert(almost_equal(-2.81795, job_req:get_bounding_c_lon(JR), 0.00001)),
     ?_assert(almost_equal(56.35769, job_req:get_bounding_d_lat(JR), 0.00001)),
     ?_assert(almost_equal(-2.82527, job_req:get_bounding_d_lon(JR), 0.00001)),
     ?_assertEqual({attack_planning, joint_stars}, job_req:get_radar_mode(JR)),
     ?_assertEqual(15, job_req:get_radar_range_res(JR)),
     ?_assertEqual(42, job_req:get_radar_cross_range_res(JR)),
     ?_assertEqual(2018, job_req:get_earliest_start_year(JR)),
     ?_assertEqual(2, job_req:get_earliest_start_month(JR)),
     ?_assertEqual(10, job_req:get_earliest_start_day(JR)),
     ?_assertEqual(21, job_req:get_earliest_start_hour(JR)),
     ?_assertEqual(22, job_req:get_earliest_start_min(JR)),
     ?_assertEqual(13, job_req:get_earliest_start_sec(JR)),
     ?_assertEqual(3600, job_req:get_allowed_delay(JR)),
     ?_assertEqual(30, job_req:get_duration(JR)),
     ?_assertEqual(default_interval, job_req:get_revisit_interval(JR)),
     ?_assertEqual(global_hawk_sensor, job_req:get_sensor_id_type(JR)),
     ?_assertEqual("HawkV2", job_req:get_sensor_id_model(JR)),
     ?_assertEqual(initial_request, job_req:get_request_type(JR))
    ].

%% Test the ability to encode job request segments. Create a new segment with
%% parameters that when encoded should match sample_job_request().
encode_checks() ->
    ParamList = sample_job_request_params(),
    JR = job_req:new(ParamList),
    EJR = job_req:encode(JR),
    Bin = iolist_to_binary(EJR),
    [?_assertEqual(Bin, sample_job_request())].

%% Return a binary job request segment to use as test data.
sample_job_request() ->
    <<"Job Req ID","JReqTaskID",0,
      64,0,0,0, 245,85,85,85, 64,0,0,0, 245,85,85,85, 64,0,0,0,
      245,85,85,85, 64,0,0,0, 245,85,85,85, 24, 16#12C:16, 0:16,
      2018:16,5,23,10,11,50,65000:16,0:16,0:16,255,"None  ",0>>.

%% Alternate test data: non defaults in the later parameters.
sample_job_request2() ->
    <<"Job Req ID","JReqTaskID",0,
      64,0,0,0, 245,85,85,85, 64,0,0,0, 245,85,85,85, 64,0,0,0,
      245,85,85,85, 64,0,0,0, 245,85,85,85, 24, 16#12C:16, 0:16,
      2018:16,5,23,10,11,50,65000:16,54321:16,21314:16,5,"HawkV1",1>>.

%% Define a sample parameter list to use with new/1.
sample_params() ->
    [{requestor_id, "Hawkstream"}, 
     {requestor_task_id, "Test Run 1"},
     {requestor_priority, 99},
     {bounding_a_lat, 56.36061},
     {bounding_a_lon, -2.82458},
     {bounding_b_lat, 56.36070},
     {bounding_b_lon, -2.81774},
     {bounding_c_lat, 56.35773},
     {bounding_c_lon, -2.81795},
     {bounding_d_lat, 56.35769},
     {bounding_d_lon, -2.82527},
     {radar_mode, {attack_planning, joint_stars}},
     {radar_range_res, 15},
     {radar_cross_range_res, 42},
     {earliest_start_year, 2018},
     {earliest_start_month, 2},
     {earliest_start_day, 10},
     {earliest_start_hour, 21},
     {earliest_start_min, 22},
     {earliest_start_sec, 13},
     {allowed_delay, 3600},
     {duration, 30},
     {revisit_interval, default_interval},
     {sensor_id_type, global_hawk_sensor},
     {sensor_id_model, "HawkV2"},
     {request_type, initial_request}
    ].

%% Define a sample parameter list to use with new/1. This should encode to 
%% match the binary sample_job_request().
sample_job_request_params() ->
    [{requestor_id, "Job Req ID"}, 
     {requestor_task_id, "JReqTaskID"},
     {requestor_priority, default_priority},
     {bounding_a_lat, 45.0},
     {bounding_a_lon, 345.0},
     {bounding_b_lat, 45.0},
     {bounding_b_lon, 345.0},
     {bounding_c_lat, 45.0},
     {bounding_c_lon, 345.0},
     {bounding_d_lat, 45.0},
     {bounding_d_lon, 345.0},
     {radar_mode, {maritime_mti_low_res, joint_stars}},
     {radar_range_res, 300},
     {radar_cross_range_res, dont_care},
     {earliest_start_year, 2018},
     {earliest_start_month, 5},
     {earliest_start_day, 23},
     {earliest_start_hour, 10},
     {earliest_start_min, 11},
     {earliest_start_sec, 50},
     {allowed_delay, 65000},
     {duration, continuous},
     {revisit_interval, default_interval},
     {sensor_id_type, no_statement},
     {sensor_id_model, no_statement},
     {request_type, initial_request}
    ].

%% Utility function to compare whether floating point values are within a
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
