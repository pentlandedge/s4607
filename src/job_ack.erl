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

-export([decode/1, encode/1, new/1, payload_size/1, display/1]).

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

%% Export of functions for unit testing.
-ifdef(TEST).
-export([decode_request_status/1, encode_request_status/1]).
-endif.

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

-define(SPACES_10, "          ").

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

%% @doc Produce a binary encoded job acknowledge segment. Left as an iolist().
-spec encode(JobAck::job_ack()) -> iolist().
encode(JA) ->

    % List of parameters in the how to fetch/encode.
    ParamList = 
        [{fun get_job_id/1, fun stanag_types:integer_to_i32/1},
         {fun get_requestor_id/1, fun job_req:encode_10_char/1},
         {fun get_requestor_task_id/1, fun job_req:encode_10_char/1},
         {fun get_sensor_id_type/1, fun job_def:encode_sensor_id_type/1},
         {fun get_sensor_id_model/1, fun job_req:encode_sensor_id_model/1},
         {fun get_radar_priority/1, fun job_req:encode_priority/1},
         {fun get_bounding_a_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_a_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_bounding_b_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_b_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_bounding_c_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_c_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_bounding_d_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_d_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_radar_mode/1, fun job_def:encode_radar_mode/1},
         {fun get_duration/1, fun job_req:encode_duration/1},
         {fun get_revisit_interval/1, fun job_req:encode_revisit_interval/1},
         {fun get_request_status/1, fun encode_request_status/1},
         {fun get_start_datetime/1, fun job_req:encode_start_datetime/1},
         {fun get_requestor_nationality/1, fun list_to_binary/1}],

    sutils:encode_param_list(JA, ParamList).

%% @doc Create a new job acknowledge segment from a supplied list of 
%% {parameter, Value} tuples.
-spec new(ParamList::list()) -> job_ack().
new(ParamList) ->
    % Shorthand.
    F = fun sutils:extract_param_or_default/3,

    #job_ack{
        job_id = F(job_id, ParamList, 1),
        requestor_id = F(requestor_id, ParamList, ?SPACES_10),
        requestor_task_id = F(requestor_task_id, ParamList, ?SPACES_10),
        sensor_id_type = F(sensor_id_type, ParamList, no_statement),
        sensor_id_model = F(sensor_id_model, ParamList, no_statement),
        radar_priority = F(radar_priority, ParamList, 99),
        bounding_a_lat = F(bounding_a_lat, ParamList, 0.0),
        bounding_a_lon = F(bounding_a_lon, ParamList, 0.0),
        bounding_b_lat = F(bounding_b_lat, ParamList, 0.0),
        bounding_b_lon = F(bounding_b_lon, ParamList, 0.0),
        bounding_c_lat = F(bounding_c_lat, ParamList, 0.0),
        bounding_c_lon = F(bounding_c_lon, ParamList, 0.0),
        bounding_d_lat = F(bounding_d_lat, ParamList, 0.0),
        bounding_d_lon = F(bounding_d_lon, ParamList, 0.0),
        radar_mode = F(radar_mode, ParamList, {unspecified_mode, generic}),
        duration = F(duration, ParamList, continuous),
        revisit_interval = F(revisit_interval, ParamList, default_interval),
        request_status = F(request_status, ParamList, approved),
        start_year = F(start_year, ParamList, 2000),
        start_month = F(start_month, ParamList, 1),
        start_day = F(start_day, ParamList, 1),
        start_hour = F(start_hour, ParamList, 0),
        start_min = F(start_min, ParamList, 0),
        start_sec = F(start_sec, ParamList, 0),
        requestor_nationality = F(requestor_nationality, ParamList, "XN")
    }.

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

%% @doc Encode the request status parameter.
-spec encode_request_status(request_status()) -> 0..10.
encode_request_status(request)                      -> 0;
encode_request_status(approved)                     -> 1;
encode_request_status(approved_with_modification)   -> 2;
encode_request_status(denied_line_of_sight)         -> 3;
encode_request_status(denied_timeline)              -> 4;
encode_request_status(denied_orbit)                 -> 5;
encode_request_status(denied_priority)              -> 6;
encode_request_status(denied_area_of_interest)      -> 7;
encode_request_status(denied_illegal_request)       -> 8;
encode_request_status(denied_function_inoperative)  -> 9;
encode_request_status(denied_other)                 -> 10. 

%% @doc Display the contents of a job acknowledge segment.
display(#job_ack{} = JA) ->
    io:format("****************************************~n"),
    io:format("** @job_ack~n"),
    io:format("Job ID: ~p~n", [get_job_id(JA)]),
    io:format("Requestor ID: ~p~n", [get_requestor_id(JA)]),
    io:format("Requestor task ID: ~p~n", [get_requestor_task_id(JA)]),
    io:format("Sensor ID type: ~p~n", [get_sensor_id_type(JA)]),
    io:format("Sensor ID model: ~p~n", [get_sensor_id_model(JA)]),
    io:format("Radar priority: ~p~n", [get_radar_priority(JA)]),
    io:format("Bounding A Lat: ~p~n", [get_bounding_a_lat(JA)]),
    io:format("Bounding A Lon: ~p~n", [get_bounding_a_lon(JA)]),
    io:format("Bounding B Lat: ~p~n", [get_bounding_b_lat(JA)]),
    io:format("Bounding B Lon: ~p~n", [get_bounding_b_lon(JA)]),
    io:format("Bounding C Lat: ~p~n", [get_bounding_c_lat(JA)]),
    io:format("Bounding C Lon: ~p~n", [get_bounding_c_lon(JA)]),
    io:format("Bounding D Lat: ~p~n", [get_bounding_d_lat(JA)]),
    io:format("Bounding D Lon: ~p~n", [get_bounding_d_lon(JA)]),
    io:format("Radar mode: ~p~n", [get_radar_mode(JA)]),
    io:format("Duration: ~p~n", [get_duration(JA)]),
    io:format("Revisit interval: ~p~n", [get_revisit_interval(JA)]),
    io:format("Request status: ~p~n", [get_request_status(JA)]),
    io:format("Start year: ~p~n", [get_start_year(JA)]),
    io:format("Start month: ~p~n", [get_start_month(JA)]),
    io:format("Start day: ~p~n", [get_start_day(JA)]),
    io:format("Start hour: ~p~n", [get_start_hour(JA)]),
    io:format("Start min: ~p~n", [get_start_min(JA)]),
    io:format("Start sec: ~p~n", [get_start_sec(JA)]),
    io:format("Requestor nat. ID: ~p~n", [get_requestor_nationality(JA)]).

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

%% @doc Convenience function to fetch all the start time fields as a single 
%% datetime() object.
-spec get_start_datetime(job_ack()) -> calendar:datetime().
get_start_datetime(#job_ack{start_year = Y, 
                            start_month = Mth,
                            start_day = D,
                            start_hour = H,
                            start_min = Min,
                            start_sec = S}) ->
    {{Y,Mth,D},{H,Min,S}}.

get_requestor_nationality(#job_ack{requestor_nationality = X}) -> X.

