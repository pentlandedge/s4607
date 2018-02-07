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
    [valid_checks(), valid_checks2()].

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

%% Utility function to compare whether floating point values are within a
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
