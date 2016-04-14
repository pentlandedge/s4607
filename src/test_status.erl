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
    get_calibration_mode_status/1,
    get_range_limit_status/1,
    get_azimuth_limit_status/1,
    get_elevation_limit_status/1,
    get_temperature_limit_status/1]).
 
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

%% Functions to extract the hardware status flags.

get_antenna_status(#test_and_status{hardware_status = HS}) -> 
    get_hardware_status_flag(antenna, HS).

get_rf_electronics_status(#test_and_status{hardware_status = HS}) -> 
    get_hardware_status_flag(rf_electronics, HS).

get_processor_status(#test_and_status{hardware_status = HS}) -> 
    get_hardware_status_flag(processor, HS).

get_datalink_status(#test_and_status{hardware_status = HS}) -> 
    get_hardware_status_flag(datalink, HS).

get_calibration_mode_status(#test_and_status{hardware_status = HS}) -> 
    get_hardware_status_flag(calibration_mode, HS).

get_hardware_status_flag(Flag, HS) when is_list(HS) -> 
    {Flag, Val} = proplists:lookup(Flag, HS),
    Val.

%% Functions to extract the mode status flags.

get_range_limit_status(#test_and_status{mode_status = MS}) -> 
    get_mode_status_flag(range_limit, MS).

get_azimuth_limit_status(#test_and_status{mode_status = MS}) -> 
    get_mode_status_flag(azimuth_limit, MS).

get_elevation_limit_status(#test_and_status{mode_status = MS}) -> 
    get_mode_status_flag(elevation_limit, MS).

get_temperature_limit_status(#test_and_status{mode_status = MS}) -> 
    get_mode_status_flag(temperature_limit, MS).

get_mode_status_flag(Flag, ModeStatus) when is_list(ModeStatus) ->  
    {Flag, Val} = proplists:lookup(Flag, ModeStatus),
    Val.

