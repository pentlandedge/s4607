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

-export([
    decode/1,
    get_job_id/1,
    get_revisit_index/1,
    get_dwell_index/1,
    get_dwell_time/1,
    get_antenna_status/1,
    get_rf_electronics_status/1,
    get_processor_status/1,
    get_datalink_status/1,
    get_calibration_mode_status/1]).

-record(test_and_status, {
    job_id,
    revisit_index,
    dwell_index,
    dwell_time,
    hardware_status,
    mode_status}).

%% Function to decode a test and status segment.
decode(<<JobID:32,RI:16,DI:16,DT:32,HS:1/binary,MS:1/binary>>) ->
    {ok, #test_and_status{
        job_id = JobID,
        revisit_index = RI,
        dwell_index = DI,
        dwell_time = DT,
        hardware_status = decode_hardware_status(HS),
        mode_status = decode_mode_list(MS)}}.

%% Function to decode the hardware status and return a proplist.
decode_hardware_status(<<Antenna:1,RF:1,Proc:1,Datalink:1,Cal:1,_:3>>) ->
    F = fun hardware_bit/1,
    [{antenna, F(Antenna)}, {rf_electronics, F(RF)}, {processor, F(Proc)},
     {datalink, F(Datalink)}, {calibration_mode, F(Cal)}]. 

%% Function to decode the bit meaning in the harware status byte.
hardware_bit(0) -> pass;
hardware_bit(1) -> fail.

%% Function to decode the mode status bits and return a proplist.
decode_mode_list(<<Range:1,Azimuth:1,Elev:1,Temp:1,_:4>>) ->
    F = fun mode_status_bit/1,
    [{range_limit, F(Range)}, {azimuth_limit, F(Azimuth)}, 
     {elevation_limit, F(Elev)}, {temperature_limit, F(Temp)}].

%% Function to decode the bit meaning in the in the mode status byte.
mode_status_bit(0) -> within_operational_limit;
mode_status_bit(1) -> outwith_operational_limit.

%% Accessor functions.
get_job_id(#test_and_status{job_id = X}) -> X.
get_revisit_index(#test_and_status{revisit_index = X}) -> X.
get_dwell_index(#test_and_status{dwell_index = X}) -> X.
get_dwell_time(#test_and_status{dwell_time = X}) -> X.

get_antenna_status(#test_and_status{hardware_status = HS}) -> 
    {antenna, Val} = proplists:lookup(antenna, HS),
    Val.

get_rf_electronics_status(#test_and_status{hardware_status = HS}) -> 
    {rf_electronics, Val} = proplists:lookup(rf_electronics, HS),
    Val.

get_processor_status(#test_and_status{hardware_status = HS}) -> 
    {processor, Val} = proplists:lookup(processor, HS),
    Val.

get_datalink_status(#test_and_status{hardware_status = HS}) -> 
    {datalink, Val} = proplists:lookup(datalink, HS),
    Val.

get_calibration_mode_status(#test_and_status{hardware_status = HS}) -> 
    {calibration_mode, Val} = proplists:lookup(calibration_mode, HS),
    Val.

