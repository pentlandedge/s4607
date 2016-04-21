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
    encode/1,
    new/6,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

-type test_and_status() :: #test_and_status{}.

-type job_id() :: non_neg_integer().
-type revisit_index() :: pos_integer().
-type dwell_index() :: pos_integer().
-type dwell_time() :: non_neg_integer().

-type hw_status_key() :: antenna | rf_electronics | processor |datalink | 
    calibration_mode.

-type mode_status_key() :: range_limit | azimuth_limit | elevation_limit | 
    temperature_limit.

%% @doc Decode a test and status segment.
-spec decode(Bin::binary()) -> {ok, test_and_status()}.
decode(<<JobID:32,RI:16,DI:16,DT:32,HS:1/binary,MS:1/binary>>) ->
    {ok, #test_and_status{
        job_id = JobID,
        revisit_index = RI,
        dwell_index = DI,
        dwell_time = DT,
        hardware_status = decode_hardware_status(HS),
        mode_status = decode_mode_list(MS)}}.

%% @doc Encode a test and status segment
-spec encode(test_and_status()) -> binary().
encode(#test_and_status{job_id = JobID, revisit_index = RI, dwell_index = DI,
    dwell_time = DT, hardware_status = HS, mode_status = MS}) -> 
    EncHS = encode_hardware_status(HS),
    EncMS = encode_mode_status(MS),
    <<JobID:32,RI:16,DI:16,DT:32,EncHS:1/binary,EncMS:1/binary>>.

%% @doc Build a new test and status record.
-spec new(JobID, RevisitIndex, DwellIndex, DwellTime, HardwareFaults, 
    ModeStatusFaults) -> test_and_status()
    when JobID :: job_id(), RevisitIndex :: revisit_index(),
        DwellIndex :: dwell_index(), DwellTime :: dwell_time(),
        HardwareFaults :: list(hw_status_key()), 
        ModeStatusFaults :: list(mode_status_key()).

new(JobID, RevisitIndex, DwellIndex, DwellTime, HardwareFaults, 
    ModeStatusFaults) 
    when is_integer(JobID), JobID >= 0,
        is_integer(RevisitIndex), RevisitIndex >= 1, 
        is_integer(DwellIndex), DwellIndex >= 1,
        is_integer(DwellTime), DwellTime >= 0,
        is_list(HardwareFaults), is_list(ModeStatusFaults) ->

    % Process the list of supplied faults into proplists.
    HS = hardware_proplist(HardwareFaults),
    MS = mode_proplist(ModeStatusFaults),

    #test_and_status{job_id = JobID, revisit_index = RevisitIndex, 
        dwell_index = DwellIndex, dwell_time = DwellTime,
        hardware_status = HS,
        mode_status = MS}.

%% Function to decode the hardware status and return a proplist.
decode_hardware_status(<<Antenna:1,RF:1,Proc:1,Datalink:1,Cal:1,_:3>>) ->
    F = fun decode_hardware_status_bit/1,
    [{antenna, F(Antenna)}, {rf_electronics, F(RF)}, {processor, F(Proc)},
     {datalink, F(Datalink)}, {calibration_mode, F(Cal)}]. 

%% Encode a proplist with the hardware status fields to its binary form.
encode_hardware_status(HwProplist) when is_list(HwProplist) ->
    %% Fun to convert the hardware status to a bit value.
    F = fun(Field, List) ->
            HardwareStatus = proplists:get_value(Field, List, pass),
            encode_hardware_status_bit(HardwareStatus)
        end,

    Ant = F(antenna, HwProplist),
    RfE = F(rf_electronics, HwProplist),
    Proc = F(processor, HwProplist),
    Data = F(datalink, HwProplist),
    Cal = F(calibration_mode, HwProplist),
    
    <<Ant:1,RfE:1,Proc:1,Data:1,Cal:1,0:3>>.
        
%% Decode the bit meaning in the harware status byte.
decode_hardware_status_bit(0) -> pass;
decode_hardware_status_bit(1) -> fail.

%% Encode the hardware status as a bit value.
encode_hardware_status_bit(pass) -> 0;
encode_hardware_status_bit(fail) -> 1.

%% Function to create a proplist for the hardware status flags from the list
%% of supplied faults.
hardware_proplist(Faults) when is_list(Faults) ->
    FlagSet = [antenna, rf_electronics, processor, datalink, calibration_mode],
    F = fun(Flag, Acc) ->
            case lists:member(Flag, Faults) of
                true -> 
                    [{Flag, fail}|Acc];
                false -> 
                    [{Flag, pass}|Acc]
            end
        end,
    lists:foldl(F, [], FlagSet).

%% Function to decode the mode status bits and return a proplist.
decode_mode_list(<<Range:1,Azimuth:1,Elev:1,Temp:1,_:4>>) ->
    F = fun decode_mode_status_bit/1,
    [{range_limit, F(Range)}, {azimuth_limit, F(Azimuth)}, 
     {elevation_limit, F(Elev)}, {temperature_limit, F(Temp)}].

%% Encode a proplist with the mode status fields to its binary form.
encode_mode_status(ModeStatusList) when is_list(ModeStatusList) ->
    %% Fun to convert the mode status to a bit value.
    F = fun(Field, List) ->
            ModeStatus = proplists:get_value(Field, List, within_operational_limit), 
            encode_mode_status_bit(ModeStatus)
        end,

    Range = F(range_limit, ModeStatusList),
    Az = F(azimuth_limit, ModeStatusList),
    El = F(elevation_limit, ModeStatusList),
    Temp = F(temperature_limit, ModeStatusList),
    
    <<Range:1,Az:1,El:1,Temp:1,0:4>>.
 
%% Function to decode the bit meaning in the in the mode status byte.
decode_mode_status_bit(0) -> within_operational_limit;
decode_mode_status_bit(1) -> outwith_operational_limit.

%% Encode the mode status as a bit.
encode_mode_status_bit(within_operational_limit) -> 0;
encode_mode_status_bit(outwith_operational_limit) -> 1.
    
%% Create a proplist for the mode status flags from the list of supplied 
%% faults.
mode_proplist(Faults) when is_list(Faults) ->
    FlagSet = [range_limit, azimuth_limit, elevation_limit, temperature_limit],
    F = fun(Flag, Acc) ->
            case lists:member(Flag, Faults) of
                true -> 
                    [{Flag, outwith_operational_limit}|Acc];
                false -> 
                    [{Flag, within_operational_limit}|Acc]
            end
        end,
    lists:foldl(F, [], FlagSet).

%% @doc Accessor for the job ID field.
-spec get_job_id(TS :: test_and_status()) -> job_id().
get_job_id(#test_and_status{job_id = X}) -> X.

%% @doc Accessor for the revisit index.
-spec get_revisit_index(TS :: test_and_status()) -> revisit_index().
get_revisit_index(#test_and_status{revisit_index = X}) -> X.

%% @doc Accessor for the dwell index.
-spec get_dwell_index(TS :: test_and_status()) -> dwell_index().
get_dwell_index(#test_and_status{dwell_index = X}) -> X.

%% @doc Accessor for the dwell time.
-spec get_dwell_time(TS :: test_and_status()) -> dwell_time().
get_dwell_time(#test_and_status{dwell_time = X}) -> X.

%% Functions to extract the hardware status flags.

get_antenna_status(#test_and_status{hardware_status = HS}) -> 
    get_status_flag_from_proplist(antenna, HS).

get_rf_electronics_status(#test_and_status{hardware_status = HS}) -> 
    get_status_flag_from_proplist(rf_electronics, HS).

get_processor_status(#test_and_status{hardware_status = HS}) -> 
    get_status_flag_from_proplist(processor, HS).

get_datalink_status(#test_and_status{hardware_status = HS}) -> 
    get_status_flag_from_proplist(datalink, HS).

get_calibration_mode_status(#test_and_status{hardware_status = HS}) -> 
    get_status_flag_from_proplist(calibration_mode, HS).

%% Functions to extract the mode status flags.

get_range_limit_status(#test_and_status{mode_status = MS}) -> 
    get_status_flag_from_proplist(range_limit, MS).

get_azimuth_limit_status(#test_and_status{mode_status = MS}) -> 
    get_status_flag_from_proplist(azimuth_limit, MS).

get_elevation_limit_status(#test_and_status{mode_status = MS}) -> 
    get_status_flag_from_proplist(elevation_limit, MS).

get_temperature_limit_status(#test_and_status{mode_status = MS}) -> 
    get_status_flag_from_proplist(temperature_limit, MS).

%% Funcition to extract a status flag from the proplist. The key supplied
%% must exist in the list.
get_status_flag_from_proplist(Key, PropList) when is_list(PropList) ->  
    {Key, Val} = proplists:lookup(Key, PropList),
    Val.

