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
%% @doc Functions for manipulating job definition segments defined in the 
%%      Stanag 4607 standard.

-module(job_req).

-export([decode/1, display/1]).

%% Accessor functions.
-export([
    get_requestor_id/1,
    get_requestor_task_id/1,
    get_requestor_priority/1,
    get_bounding_a_lat/1,
    get_bounding_a_lon/1,
    get_bounding_b_lat/1,
    get_bounding_b_lon/1,
    get_bounding_c_lat/1,
    get_bounding_c_lon/1,
    get_bounding_d_lat/1,
    get_bounding_d_lon/1,
    get_radar_mode/1,
    get_radar_range_res/1,
    get_radar_cross_range_res/1,
    get_earliest_start_year/1,
    get_earliest_start_month/1,
    get_earliest_start_day/1,
    get_earliest_start_hour/1,
    get_earliest_start_min/1,
    get_earliest_start_sec/1,
    get_allowed_delay/1,
    get_duration/1,
    get_revisit_interval/1,
    get_sensor_id_type/1,
    get_sensor_id_model/1,
    get_request_type/1]).

-record(job_req, {
    requestor_id,
    requestor_task_id,
    requestor_priority,
    bounding_a_lat,
    bounding_a_lon,
    bounding_b_lat,
    bounding_b_lon,
    bounding_c_lat,
    bounding_c_lon,
    bounding_d_lat,
    bounding_d_lon,
    radar_mode,
    radar_range_res,
    radar_cross_range_res,
    earliest_start_year,
    earliest_start_month,
    earliest_start_day,
    earliest_start_hour,
    earliest_start_min,
    earliest_start_sec,
    allowed_delay,
    duration,
    revisit_interval,
    sensor_id_type,
    sensor_id_model,
    request_type}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

-opaque job_req() :: #job_req{}.

-export_type([job_req/0]).

-spec decode(Bin::binary()) -> {ok, job_req()}.
decode(<<ReqID:10/binary,TaskID:10/binary,Pri,R4:4/binary,R5:4/binary,
    R6:4/binary,R7:4/binary,R8:4/binary,R9:4/binary,R10:4/binary,
    R11:4/binary,Mode,RangeRes:16,XRangeRes:16,Yr:16,Mth,Day,Hr,Min,Sec,
    Delay:16,Dur:16,RevInt:16,SensorType,Model:6/binary,ReqType,
    _Rest/binary>>) ->
    JRS = #job_req{
        requestor_id = binary_to_list(ReqID),
        requestor_task_id = binary_to_list(TaskID),
        requestor_priority = decode_priority(Pri),
        bounding_a_lat = stanag_types:sa32_to_float(R4),
        bounding_a_lon = stanag_types:ba32_to_float(R5),
        bounding_b_lat = stanag_types:sa32_to_float(R6),
        bounding_b_lon = stanag_types:ba32_to_float(R7),
        bounding_c_lat = stanag_types:sa32_to_float(R8),
        bounding_c_lon = stanag_types:ba32_to_float(R9),
        bounding_d_lat = stanag_types:sa32_to_float(R10),
        bounding_d_lon = stanag_types:ba32_to_float(R11),
        radar_mode = job_def:decode_radar_mode(Mode),
        radar_range_res = decode_res(RangeRes),
        radar_cross_range_res = decode_res(XRangeRes),
        earliest_start_year = Yr,
        earliest_start_month = Mth,
        earliest_start_day = Day,
        earliest_start_hour = Hr,
        earliest_start_min = Min,
        earliest_start_sec = Sec,
        allowed_delay = Delay,
        duration = decode_duration(Dur),
        revisit_interval = decode_revisit_interval(RevInt),
        sensor_id_type = job_def:decode_sensor_id_type(SensorType),
        sensor_id_model = decode_sensor_id_model(Model),
        request_type = decode_request_type(ReqType)},
    {ok, JRS}.

decode_priority(0) -> default_priority;
decode_priority(X) when X >= 0, X =< 99 -> X.

decode_res(0) -> dont_care;
decode_res(X) -> X.

decode_duration(0) -> continuous;
decode_duration(X) -> X.

decode_revisit_interval(0) -> default_interval;
decode_revisit_interval(X) -> X.

decode_sensor_id_model(<<"None  ">>) -> 
    no_statement;
decode_sensor_id_model(<<Model:48/binary>>) -> 
    binary_to_list(Model).

decode_request_type(0) -> initial_request;
decode_request_type(1) -> cancel_job.

display(#job_req{}) ->
    ok.

%% Accessor functions.
get_requestor_id(#job_req{requestor_id = X}) -> X.
get_requestor_task_id(#job_req{requestor_task_id = X}) -> X.
get_requestor_priority(#job_req{requestor_priority = X}) -> X.
get_bounding_a_lat(#job_req{bounding_a_lat = X}) -> X.
get_bounding_a_lon(#job_req{bounding_a_lon = X}) -> X.
get_bounding_b_lat(#job_req{bounding_b_lat = X}) -> X.
get_bounding_b_lon(#job_req{bounding_b_lon = X}) -> X.
get_bounding_c_lat(#job_req{bounding_c_lat = X}) -> X.
get_bounding_c_lon(#job_req{bounding_c_lon = X}) -> X.
get_bounding_d_lat(#job_req{bounding_d_lat = X}) -> X.
get_bounding_d_lon(#job_req{bounding_d_lon = X}) -> X.
get_radar_mode(#job_req{radar_mode = X}) -> X.
get_radar_range_res(#job_req{radar_range_res = X}) -> X.
get_radar_cross_range_res(#job_req{radar_cross_range_res = X}) -> X.
get_earliest_start_year(#job_req{earliest_start_year = X}) -> X.
get_earliest_start_month(#job_req{earliest_start_month = X}) -> X.
get_earliest_start_day(#job_req{earliest_start_day = X}) -> X.
get_earliest_start_hour(#job_req{earliest_start_hour = X}) -> X.
get_earliest_start_min(#job_req{earliest_start_min = X}) -> X.
get_earliest_start_sec(#job_req{earliest_start_sec = X}) -> X.
get_allowed_delay(#job_req{allowed_delay = X}) -> X.
get_duration(#job_req{duration = X}) -> X.
get_revisit_interval(#job_req{revisit_interval = X}) -> X.
get_sensor_id_type(#job_req{sensor_id_type = X}) -> X.
get_sensor_id_model(#job_req{sensor_id_model = X}) -> X.
get_request_type(#job_req{request_type = X}) -> X.

