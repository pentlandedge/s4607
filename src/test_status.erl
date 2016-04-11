%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2016 Pentland Edge Ltd.
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
%% @doc Functions for manipulating test and status segments defined in the 
%%      Stanag 4607 standard.

-module(test_status).

-export([decode/1]).

-record(test_and_status, {
    job_id,
    revisit_index,
    dwell_index,
    dwell_time,
    hardware_status,
    mode_status}).

%% Function to decode a test and status segment.
decode(<<JobID:32,RI:16,DI:16,DT:32,HS:1,MS:1>>) ->
    {ok, #test_and_status{
        job_id = JobID,
        revisit_index = RI,
        dwell_index = DI,
        dwell_time = DT,
        hardware_status = decode_hardware_status(HS),
        mode_status = MS}}.

%% Function to decode the hardware status and return a proplist.
decode_hardware_status(<<Antenna:1,RF:1,Proc:1,Datalink:1,Cal:1,_:3>>) ->
    F = fun hardware_bit/1,
    {{antenna, F(Antenna)}, {rf_electronics, F(RF)}, {processor, F(Proc)},
     {datalink, F(Datalink)}, {calibration_mode, F(Cal)}}. 

%% Function to decode the bit meaning in the harware status byte.
hardware_bit(0) -> pass;
hardware_bit(1) -> fail.

