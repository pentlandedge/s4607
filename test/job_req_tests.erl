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

%% Define a test generator for the Job Request segment functions. 
job_req_test_() ->
    [valid_checks(), valid_checks2(), new_checks()].

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
    ReqId = job_req:get_requestor_id(JR),
    TaskId = job_req:get_requestor_task_id(JR),
    [?_assertEqual("Hawkstream", ReqId),
     ?_assertEqual("Test Run 1", TaskId)
    ].

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
     {bounding_b_lat,  56.3607},
     {bounding_b_lon, -2.81774},
     {bounding_c_lat,  56.35773},
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
     {revisit_interval, 0},
     {sensor_id_type, global_hawk_sensor},
     {sensor_id_model, "HawkV2"},
     {request_type, initial_request}
    ].

%% Utility function to compare whether floating point values are within a
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
