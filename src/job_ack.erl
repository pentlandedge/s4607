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

-export([decode/1]).

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

decode(_) ->
    {ok, #job_ack{}}.

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

