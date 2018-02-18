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
%% @doc Functions for manipulating job acknowledge segments defined in the 
%%      Stanag 4607 standard.

-module(job_ack).

-export([decode/1, payload_size/1]).

%% Export the field accessor functions.
-export([
    get_job_id/1,
    get_requestor_id/1,
    get_requestor_task_id/1,
    get_sensor_id_type/1,
    get_sensor_id_model/1,
    get_radar_priority/1,
    get_bounding_a_lat/1,
    get_bounding_a_lon/1,
    get_bounding_b_lat/1,
    get_bounding_b_lon/1,
    get_bounding_c_lat/1,
    get_bounding_c_lon/1,
    get_bounding_d_lat/1,
    get_bounding_d_lon/1,
    get_radar_mode/1,
    get_duration/1,
    get_revisit_interval/1,
    get_request_status/1,
    get_start_year/1,
    get_start_month/1,
    get_start_day/1,
    get_start_hour/1,
    get_start_min/1,
    get_start_sec/1,
    get_requestor_nationality/1]).

-record(job_ack, {
    job_id,
    requestor_id,
    requestor_task_id,
    sensor_id_type,
    sensor_id_model,
    radar_priority,
    bounding_a_lat,
    bounding_a_lon,
    bounding_b_lat,
    bounding_b_lon,
    bounding_c_lat,
    bounding_c_lon,
    bounding_d_lat,
    bounding_d_lon,
    radar_mode,
    duration,
    revisit_interval,
    request_status,
    start_year,
    start_month,
    start_day,
    start_hour,
    start_min,
    start_sec,
    requestor_nationality}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

-opaque job_ack() :: #job_ack{}.

-type job_ack_bin() :: <<_:632>>.

-export_type([job_ack/0, job_ack_bin/0]).

-type radar_priority() :: 1..99.

-type request_status() :: request | approved | approved_with_modification | 
    denied_line_of_sight | denied_timeline | denied_orbit | denied_priority | 
    denied_area_of_interest | denied_illegal_request | 
    denied_function_inoperative | denied_other.

-export_type([radar_priority/0, request_status/0]).

%% @doc Decode a binary encoded job acknowledge segment.
-spec decode(Bin::job_ack_bin()) -> {ok, job_ack()}.
decode(<<JobID:32,ReqID:10/binary,TaskID:10/binary,SensorType,Model:6/binary,
    RadPri,A7:4/binary,A8:4/binary,A9:4/binary,A10:4/binary,A11:4/binary,
    A12:4/binary,A13:4/binary,A14:4/binary,Mode,Dur:16,RevInt:16,Status,Yr:16,
    Mth,Day,Hr,Min,Sec,Nat:2/binary>>) ->

    JA = #job_ack{
        job_id = JobID,
        requestor_id = binary_to_list(ReqID),
        requestor_task_id = binary_to_list(TaskID),
        sensor_id_type = job_def:decode_sensor_id_type(SensorType),
        sensor_id_model = job_req:decode_sensor_id_model(Model),
        radar_priority = decode_radar_priority(RadPri),
        bounding_a_lat = stanag_types:sa32_to_float(A7),
        bounding_a_lon = stanag_types:ba32_to_float(A8),
        bounding_b_lat = stanag_types:sa32_to_float(A9),
        bounding_b_lon = stanag_types:ba32_to_float(A10),
        bounding_c_lat = stanag_types:sa32_to_float(A11),
        bounding_c_lon = stanag_types:ba32_to_float(A12),
        bounding_d_lat = stanag_types:sa32_to_float(A13),
        bounding_d_lon = stanag_types:ba32_to_float(A14),
        radar_mode = job_def:decode_radar_mode(Mode),
        duration = job_req:decode_duration(Dur),
        revisit_interval = job_req:decode_revisit_interval(RevInt),
        request_status = decode_request_status(Status),
        start_year = Yr,
        start_month = Mth,
        start_day = Day,
        start_hour = Hr,
        start_min = Min,
        start_sec = Sec,
        requestor_nationality = binary_to_list(Nat)},

    {ok, JA}.


%% @doc Return the expected size of the job acknowledge segment payload in 
%% bytes. Since the payload size is fixed for job acknowledge, the argument 
%% is ignored but retained in order to have a consistent API with the other 
%% segments.
-spec payload_size(any()) -> non_neg_integer().
payload_size(_) -> 79.

%% @doc Decode the radar priority
-spec decode_radar_priority(1..99) -> radar_priority().
decode_radar_priority(X) when X >= 1, X =< 99 -> X.

%% @doc Decode the request status parameter.
-spec decode_request_status(0..10) -> request_status().
decode_request_status(0)  -> request;
decode_request_status(1)  -> approved;
decode_request_status(2)  -> approved_with_modification;
decode_request_status(3)  -> denied_line_of_sight;
decode_request_status(4)  -> denied_timeline;
decode_request_status(5)  -> denied_orbit;
decode_request_status(6)  -> denied_priority;
decode_request_status(7)  -> denied_area_of_interest;
decode_request_status(8)  -> denied_illegal_request;
decode_request_status(9)  -> denied_function_inoperative;
decode_request_status(10) -> denied_other.

%% Field accessor functions.

get_job_id(#job_ack{job_id = X}) -> X.
get_requestor_id(#job_ack{requestor_id = X}) -> X.
get_requestor_task_id(#job_ack{requestor_task_id = X}) -> X.
get_sensor_id_type(#job_ack{sensor_id_type = X}) -> X.
get_sensor_id_model(#job_ack{sensor_id_model = X}) -> X.
get_radar_priority(#job_ack{radar_priority = X}) -> X.
get_bounding_a_lat(#job_ack{bounding_a_lat = X}) -> X.
get_bounding_a_lon(#job_ack{bounding_a_lon = X}) -> X.
get_bounding_b_lat(#job_ack{bounding_b_lat = X}) -> X.
get_bounding_b_lon(#job_ack{bounding_b_lon = X}) -> X.
get_bounding_c_lat(#job_ack{bounding_c_lat = X}) -> X.
get_bounding_c_lon(#job_ack{bounding_c_lon = X}) -> X.
get_bounding_d_lat(#job_ack{bounding_d_lat = X}) -> X.
get_bounding_d_lon(#job_ack{bounding_d_lon = X}) -> X.
get_radar_mode(#job_ack{radar_mode = X}) -> X.
get_duration(#job_ack{duration = X}) -> X.
get_revisit_interval(#job_ack{revisit_interval = X}) -> X.
get_request_status(#job_ack{request_status = X}) -> X.
get_start_year(#job_ack{start_year = X}) -> X.
get_start_month(#job_ack{start_month = X}) -> X.
get_start_day(#job_ack{start_day = X}) -> X.
get_start_hour(#job_ack{start_hour = X}) -> X.
get_start_min(#job_ack{start_min = X}) -> X.
get_start_sec(#job_ack{start_sec = X}) -> X.
get_requestor_nationality(#job_ack{requestor_nationality = X}) -> X.

