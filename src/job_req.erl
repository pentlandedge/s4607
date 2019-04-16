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
%% @doc Functions for manipulating job request segments defined in the 
%%      Stanag 4607 standard.

-module(job_req).

-export([decode/1, encode/1, new/1, payload_size/1, display/1]).

-export([
    decode_sensor_id_model/1, 
    decode_revisit_interval/1, 
    decode_duration/1]).

-export([
    encode_10_char/1,
    encode_sensor_id_model/1,
    encode_priority/1,
    encode_duration/1,
    encode_revisit_interval/1,
    encode_start_datetime/1]).

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
    get_start_datetime/1,
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

-define(SPACES_10, "          ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

-opaque job_req() :: #job_req{}.

% Job request is 79 bytes long.
-type job_req_bin() :: <<_:632>>.

% Define the parameter fields in the job request segment.
-type field() ::  
    requestor_id |
    requestor_task_id |
    requestor_priority |
    bounding_a_lat |
    bounding_a_lon |
    bounding_b_lat |
    bounding_b_lon |
    bounding_c_lat |
    bounding_c_lon |
    bounding_d_lat |
    bounding_d_lon |
    radar_mode |
    radar_range_res |
    radar_cross_range_res |
    earliest_start_year |
    earliest_start_month |
    earliest_start_day |
    earliest_start_hour |
    earliest_start_min |
    earliest_start_sec |
    allowed_delay |
    duration |
    revisit_interval |
    sensor_id_type |
    sensor_id_model |
    request_type.

-export_type([job_req/0, job_req_bin/0, field/0]).

-type priority() :: default_priority | 1..99.
-type resolution() :: dont_care | 1..65535.

-export_type([priority/0, resolution/0]).

-type duration() :: continuous | 1..65535.
-type revisit_interval() :: default_interval | 1..65535.
-type request_type() :: initial_request | cancel_job.

-export_type([duration/0, revisit_interval/0, request_type/0]).

-type sensor_id_model() :: no_statement | list().

-export_type([sensor_id_model/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function declarations.

-spec decode(Bin::binary()) -> {ok, job_req()} | {error, Reason::atom()}.
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
    {ok, JRS};
decode(_) ->
    {error, job_request_mismatch}.

%% @doc Produce a binary encoded job request segment. Left as an iolist().
-spec encode(JobReq::job_req()) -> iolist().
encode(JR) ->

    % List of parameters in the how to fetch/encode.
    ParamList = 
        [{fun get_requestor_id/1, fun encode_10_char/1}, 
         {fun get_requestor_task_id/1, fun encode_10_char/1},
         {fun get_requestor_priority/1, fun encode_priority/1},
         {fun get_bounding_a_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_a_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_bounding_b_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_b_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_bounding_c_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_c_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_bounding_d_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_d_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_radar_mode/1, fun job_def:encode_radar_mode/1},
         {fun get_radar_range_res/1, fun encode_res/1},
         {fun get_radar_cross_range_res/1, fun encode_res/1},
         {fun get_start_datetime/1, fun encode_start_datetime/1},
         {fun get_allowed_delay/1, fun encode_allowed_delay/1},
         {fun get_duration/1, fun encode_duration/1},
         {fun get_revisit_interval/1, fun encode_revisit_interval/1},
         {fun get_sensor_id_type/1, fun job_def:encode_sensor_id_type/1},
         {fun get_sensor_id_model/1, fun encode_sensor_id_model/1},
         {fun get_request_type/1, fun encode_request_type/1}],

    sutils:encode_param_list(JR, ParamList).

%% @doc Create a new job request segment from a supplied list of 
%% {parameter, Value} tuples.
-spec new(ParamList::list()) -> job_req().
new(ParamList) ->
    % Shorthand.
    F = fun sutils:extract_param_or_default/3,

    #job_req{
        requestor_id = F(requestor_id, ParamList, ?SPACES_10),
        requestor_task_id = F(requestor_task_id, ParamList, ?SPACES_10),
        requestor_priority = F(requestor_priority  , ParamList, default_priority),
        bounding_a_lat = F(bounding_a_lat, ParamList, 0.0),
        bounding_a_lon = F(bounding_a_lon, ParamList, 0.0),
        bounding_b_lat = F(bounding_b_lat, ParamList, 0.0),
        bounding_b_lon = F(bounding_b_lon, ParamList, 0.0),
        bounding_c_lat = F(bounding_c_lat, ParamList, 0.0),
        bounding_c_lon = F(bounding_c_lon, ParamList, 0.0),
        bounding_d_lat = F(bounding_d_lat, ParamList, 0.0),
        bounding_d_lon = F(bounding_d_lon, ParamList, 0.0),
        radar_mode = F(radar_mode, ParamList, {unspecified_mode, generic}),
        radar_range_res = F(radar_range_res, ParamList, dont_care),
        radar_cross_range_res = F(radar_cross_range_res, ParamList, dont_care),
        earliest_start_year = F(earliest_start_year, ParamList, 2000),
        earliest_start_month = F(earliest_start_month, ParamList, 1),
        earliest_start_day = F(earliest_start_day, ParamList, 1),
        earliest_start_hour = F(earliest_start_hour, ParamList, 0),
        earliest_start_min = F(earliest_start_min, ParamList, 0),
        earliest_start_sec = F(earliest_start_sec, ParamList, 0),
        allowed_delay = F(allowed_delay, ParamList, 65535),
        duration = F(duration, ParamList, continuous),
        revisit_interval = F(revisit_interval, ParamList, default_interval),
        sensor_id_type = F(sensor_id_type, ParamList, no_statement),
        sensor_id_model = F(sensor_id_model, ParamList, no_statement),
        request_type = F(request_typ, ParamList, initial_request)
    }.

%% @doc Return the expected size of the job request segment payload in 
%% bytes. Since the payload size is fixed for job request, the argument is
%% ignored but retained in order to have a consistent API with the other 
%% segments.
-spec payload_size(any()) -> non_neg_integer().
payload_size(_) -> 79.

%% Pad strings to 10 characters and convert to binary.
-spec encode_10_char(string()) -> binary().
encode_10_char(Str) when is_list(Str) ->
    Pad = sutils:add_trailing_spaces(Str, 10),
    list_to_binary(Pad).

%% @doc Decode the priority parameter.
-spec decode_priority(0..99) -> priority().
decode_priority(0) -> default_priority;
decode_priority(X) when X > 0, X =< 99 -> X.

%% @doc Encode the priority parameters.
-spec encode_priority(priority()) -> binary().
encode_priority(default_priority) -> <<0>>;
encode_priority(X) when X > 0, X =< 99 -> <<X>>.

%% @doc Decode the resolution parameter.
-spec decode_res(0..65535) -> resolution().
decode_res(0) -> dont_care;
decode_res(X) when X > 0, X =< 65535 -> X.

%% @doc Encode the resolution parameter.
-spec encode_res(resolution()) -> binary().
encode_res(dont_care) -> <<0:16>>;
encode_res(X) when X > 0, X =< 65535 -> <<X:16>>.

%% @doc Encode the start date/time fields.
-spec encode_start_datetime(calendar:datetime()) -> binary().
encode_start_datetime({{Y,Mth,D},{H,Min,S}}) ->
    <<Y:16,Mth,D,H,Min,S>>.

%% @doc Encode the allowed delay
-spec encode_allowed_delay(0..65535) -> binary().
encode_allowed_delay(X) -> <<X:16>>.

%% @doc Decode the duration parameter.
-spec decode_duration(non_neg_integer()) -> duration().
decode_duration(0) -> continuous;
decode_duration(X) -> X.

%% @doc Encode the duration parameter.
-spec encode_duration(duration()) -> binary().
encode_duration(continuous) -> 
    <<0:16>>;
encode_duration(X) when X > 0, X =< 65535 -> 
    <<X:16>>.

%% @doc Decode the revisit interval parameter.
-spec decode_revisit_interval(non_neg_integer()) -> revisit_interval().
decode_revisit_interval(0) -> default_interval;
decode_revisit_interval(X) -> X.

%% @doc Encode the revisit interval parameter.
-spec encode_revisit_interval(revisit_interval()) -> binary().
encode_revisit_interval(default_interval) -> 
    <<0:16>>;
encode_revisit_interval(X) when X > 0, X =< 65535 -> 
    <<X:16>>.

%% @doc Decode the sensor ID model.
-spec decode_sensor_id_model(binary()) -> sensor_id_model().
decode_sensor_id_model(<<"None  ">>) -> 
    no_statement;
decode_sensor_id_model(<<Model:6/binary>>) -> 
    binary_to_list(Model).

%% @doc Encode the sensor ID model as a binary. 
-spec encode_sensor_id_model(sensor_id_model()) -> binary().
encode_sensor_id_model(no_statement) -> <<"None  ">>;
encode_sensor_id_model(Str) when is_list(Str) -> 
    Pad = sutils:add_trailing_spaces(Str, 6),
    list_to_binary(Pad).

%% @doc Decode the request type parameter.
-spec decode_request_type(0..1) -> request_type().
decode_request_type(0) -> initial_request;
decode_request_type(1) -> cancel_job.

%% @doc Encode the request type parameter.
-spec encode_request_type(request_type()) -> 0..1.
encode_request_type(initial_request) -> 0;
encode_request_type(cancel_job) -> 1.

%% @doc Display the contents of a job request segment.
display(#job_req{} = JR) ->
    io:format("****************************************~n"),
    io:format("** @job_req~n"),
    io:format("Requestor ID: ~p~n", [get_requestor_id(JR)]),
    io:format("Requestor task ID: ~p~n", [get_requestor_task_id(JR)]),
    io:format("Requestor priority: ~p~n", [get_requestor_priority(JR)]),
    io:format("Bounding A Lat: ~p~n", [get_bounding_a_lat(JR)]),
    io:format("Bounding A Lon: ~p~n", [get_bounding_a_lon(JR)]),
    io:format("Bounding B Lat: ~p~n", [get_bounding_b_lat(JR)]),
    io:format("Bounding B Lon: ~p~n", [get_bounding_b_lon(JR)]),
    io:format("Bounding C Lat: ~p~n", [get_bounding_c_lat(JR)]),
    io:format("Bounding C Lon: ~p~n", [get_bounding_c_lon(JR)]),
    io:format("Bounding D Lat: ~p~n", [get_bounding_d_lat(JR)]),
    io:format("Bounding D Lon: ~p~n", [get_bounding_d_lon(JR)]),
    io:format("Radar mode: ~p~n", [get_radar_mode(JR)]),
    io:format("Radar range res: ~p~n",  [get_radar_range_res(JR)]),
    io:format("Radar cross range res: ~p~n", [get_radar_cross_range_res(JR)]),
    io:format("Earliest start year: ~p~n", [get_earliest_start_year(JR)]),
    io:format("Earliest start month: ~p~n", [get_earliest_start_month(JR)]),
    io:format("Earliest start day: ~p~n", [get_earliest_start_day(JR)]),
    io:format("Earliest start hour: ~p~n",[get_earliest_start_hour(JR)]),
    io:format("Earliest start min: ~p~n", [get_earliest_start_min(JR)]),
    io:format("Earliest start sec: ~p~n", [get_earliest_start_sec(JR)]),
    io:format("Allowed delay: ~p~n", [get_allowed_delay(JR)]),
    io:format("Duration: ~p~n", [get_duration(JR)]),
    io:format("Revisit interval: ~p~n", [get_revisit_interval(JR)]),
    io:format("Sensor ID type: ~p~n", [get_sensor_id_type(JR)]),
    io:format("Sensor ID model: ~p~n", [get_sensor_id_model(JR)]),
    io:format("Request type: ~p~n", [get_request_type(JR)]).

%% Accessor functions.

%% @doc Accessor for the requestor ID.
get_requestor_id(#job_req{requestor_id = X}) -> X.

%% @doc Accessor for the requestor task ID.
get_requestor_task_id(#job_req{requestor_task_id = X}) -> X.

%% @doc Accessor for the requestor priority.
get_requestor_priority(#job_req{requestor_priority = X}) -> X.

%% @doc Accessor for the bounding box point A Latitude.
get_bounding_a_lat(#job_req{bounding_a_lat = X}) -> X.

%% @doc Accessor for the bounding box point A Longitude.
get_bounding_a_lon(#job_req{bounding_a_lon = X}) -> X.

%% @doc Accessor for the bounding box point B Latitude.
get_bounding_b_lat(#job_req{bounding_b_lat = X}) -> X.

%% @doc Accessor for the bounding box point B Longitude.
get_bounding_b_lon(#job_req{bounding_b_lon = X}) -> X.

%% @doc Accessor for the bounding box point C Latitude.
get_bounding_c_lat(#job_req{bounding_c_lat = X}) -> X.

%% @doc Accessor for the bounding box point C Longitude.
get_bounding_c_lon(#job_req{bounding_c_lon = X}) -> X.

%% @doc Accessor for the bounding box point D Latitude.
get_bounding_d_lat(#job_req{bounding_d_lat = X}) -> X.

%% @doc Accessor for the bounding box point D Longitude.
get_bounding_d_lon(#job_req{bounding_d_lon = X}) -> X.

%% @doc Accessor for the radar mode. 
get_radar_mode(#job_req{radar_mode = X}) -> X.

%% @doc Accessor for the radar range resolution. 
get_radar_range_res(#job_req{radar_range_res = X}) -> X.

%% @doc Accessor for the radar cross range resolution. 
get_radar_cross_range_res(#job_req{radar_cross_range_res = X}) -> X.

%% @doc Accessor for the earliest start date years field. 
get_earliest_start_year(#job_req{earliest_start_year = X}) -> X.

%% @doc Accessor for the earliest start date month field. 
get_earliest_start_month(#job_req{earliest_start_month = X}) -> X.

%% @doc Accessor for the earliest start date day field. 
get_earliest_start_day(#job_req{earliest_start_day = X}) -> X.

%% @doc Accessor for the earliest start date hour field. 
get_earliest_start_hour(#job_req{earliest_start_hour = X}) -> X.

%% @doc Accessor for the earliest start date min field. 
get_earliest_start_min(#job_req{earliest_start_min = X}) -> X.

%% @doc Accessor for the earliest start date seconds field. 
get_earliest_start_sec(#job_req{earliest_start_sec = X}) -> X.

%% @doc Convenience function to fetch all the start time fields as a single 
%% datetime() object.
-spec get_start_datetime(job_req()) -> calendar:datetime().
get_start_datetime(#job_req{earliest_start_year = Y, 
                            earliest_start_month = Mth,
                            earliest_start_day = D,
                            earliest_start_hour = H,
                            earliest_start_min = Min,
                            earliest_start_sec = S}) ->
    {{Y,Mth,D},{H,Min,S}}.

get_allowed_delay(#job_req{allowed_delay = X}) -> X.

%% @doc Get the duration from a decoded job request segment. 
-spec get_duration(job_req()) -> duration().
get_duration(#job_req{duration = X}) -> X.

%% @doc Get the revisit interval from a decoded job request segment. 
-spec get_revisit_interval(job_req()) -> revisit_interval().
get_revisit_interval(#job_req{revisit_interval = X}) -> X.

get_sensor_id_type(#job_req{sensor_id_type = X}) -> X.
get_sensor_id_model(#job_req{sensor_id_model = X}) -> X.

%% @doc Get the request type from a decoded job request segment. 
-spec get_request_type(job_req()) -> request_type().
get_request_type(#job_req{request_type = X}) -> X.

