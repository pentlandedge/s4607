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
%% @doc Stanag 4607 packets are comprised of segments of various types. This 
%%      module contains generic segment handling functions. Code to handle 
%%      each specific type of segment is delegated to separate modules.

-module(job_req).

-export([decode/1, display/1]).

%% Accessor functions.
-export([
    get_requestor_id/1,
    get_requestor_task_id/1,
    get_requestor_priority/1,
    get_bound_a_lat/1,
    get_bound_a_lon/1,
    get_bound_b_lat/1,
    get_bound_b_lon/1,
    get_bound_c_lat/1,
    get_bound_c_lon/1,
    get_bound_d_lat/1,
    get_bound_d_lon/1,
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

-record(job_req_segment, {
    requestor_id,
    requestor_task_id,
    requestor_priority,
    bound_a_lat,
    bound_a_lon,
    bound_b_lat,
    bound_b_lon,
    bound_c_lat,
    bound_c_lon,
    bound_d_lat,
    bound_d_lon,
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

decode(<<ReqID:10/binary,TaskID:10/binary,_Rest/binary>>) ->
    JRS = #job_req_segment{
        requestor_id = binary_to_list(ReqID),
        requestor_task_id = binary_to_list(TaskID)},
    {ok, JRS}.

display(#job_req_segment{}) ->
    ok.

%% Accessor functions.
get_requestor_id(#job_req_segment{requestor_id = X}) -> X.
get_requestor_task_id(#job_req_segment{requestor_task_id = X}) -> X.
get_requestor_priority(#job_req_segment{requestor_priority = X}) -> X.
get_bound_a_lat(#job_req_segment{bound_a_lat = X}) -> X.
get_bound_a_lon(#job_req_segment{bound_a_lon = X}) -> X.
get_bound_b_lat(#job_req_segment{bound_b_lat = X}) -> X.
get_bound_b_lon(#job_req_segment{bound_b_lon = X}) -> X.
get_bound_c_lat(#job_req_segment{bound_c_lat = X}) -> X.
get_bound_c_lon(#job_req_segment{bound_c_lon = X}) -> X.
get_bound_d_lat(#job_req_segment{bound_d_lat = X}) -> X.
get_bound_d_lon(#job_req_segment{bound_d_lon = X}) -> X.
get_radar_mode(#job_req_segment{radar_mode = X}) -> X.
get_radar_range_res(#job_req_segment{radar_range_res = X}) -> X.
get_radar_cross_range_res(#job_req_segment{radar_cross_range_res = X}) -> X.
get_earliest_start_year(#job_req_segment{earliest_start_year = X}) -> X.
get_earliest_start_month(#job_req_segment{earliest_start_month = X}) -> X.
get_earliest_start_day(#job_req_segment{earliest_start_day = X}) -> X.
get_earliest_start_hour(#job_req_segment{earliest_start_hour = X}) -> X.
get_earliest_start_min(#job_req_segment{earliest_start_min = X}) -> X.
get_earliest_start_sec(#job_req_segment{earliest_start_sec = X}) -> X.
get_allowed_delay(#job_req_segment{allowed_delay = X}) -> X.
get_duration(#job_req_segment{duration = X}) -> X.
get_revisit_interval(#job_req_segment{revisit_interval = X}) -> X.
get_sensor_id_type(#job_req_segment{sensor_id_type = X}) -> X.
get_sensor_id_model(#job_req_segment{sensor_id_model = X}) -> X.
get_request_type(#job_req_segment{request_type = X}) -> X.

