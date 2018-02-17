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

