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
-module(dwell).

%% Main API functions.
-export([
    decode/1, 
    encode/1,
    new/1,
    payload_size/1,
    payload_size/2,
    to_dict/1,
    display/1,
    to_csv_iolist/1,
    set_dwell_time/2,
    update_targets/2]).

%% Accessor functions for accessing elements of the dwell segment.
-export([
    get_existence_mask/1,
    get_revisit_index/1,
    get_dwell_index/1,
    get_last_dwell_of_revisit/1,
    get_target_report_count/1,
    get_dwell_time/1,
    get_sensor_lat/1,
    get_sensor_lon/1,
    get_sensor_alt/1,
    get_lat_scale_factor/1,
    get_lon_scale_factor/1,
    get_spu_along_track/1,
    get_spu_cross_track/1,
    get_spu_alt/1,
    get_sensor_track/1,
    get_sensor_speed/1,
    get_sensor_vert_vel/1,
    get_sensor_track_unc/1,
    get_sensor_speed_unc/1,
    get_sensor_vert_vel_unc/1,
    get_platform_heading/1,
    get_platform_pitch/1,
    get_platform_roll/1,
    get_dwell_center_lat/1,
    get_dwell_center_lon/1,
    get_dwell_range_half_extent/1,
    get_dwell_angle_half_extent/1,
    get_sensor_heading/1,
    get_sensor_pitch/1,
    get_sensor_roll/1,
    get_mdv/1,
    get_targets/1]).

-record(dwell_segment, {
    existence_mask,
    revisit_index,
    dwell_index,
    last_dwell_of_revisit,
    target_report_count,
    dwell_time,
    sensor_lat,
    sensor_lon,
    sensor_alt,
    lat_scale_factor,
    lon_scale_factor,
    spu_along_track,
    spu_cross_track,
    spu_alt,
    sensor_track,
    sensor_speed,
    sensor_vert_vel,
    sensor_track_unc,
    sensor_speed_unc,
    sensor_vert_vel_unc,
    platform_heading,
    platform_pitch,
    platform_roll,
    dwell_center_lat,
    dwell_center_lon,
    dwell_range_half_extent,
    dwell_angle_half_extent,
    sensor_heading,
    sensor_pitch,
    sensor_roll,
    mdv,
    targets}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

-opaque dwell_segment() :: #dwell_segment{}.
-export_type([dwell_segment/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dwell segment decoding functions.

decode(<<EM:8/binary,RI:16/integer-unsigned-big,
    DI:16/integer-unsigned-big,LD,TRC:16/integer-unsigned-big,
    DT:32/integer-unsigned-big,SLat:4/binary,SLon:4/binary,SAlt:4/binary,
    Rest/binary>>) ->

    % Fixed part of the dwell segement is pattern matched above, remainder
    % depends on the existence mask.
    EMrec = exist_mask:decode(EM),

    {LatScaleFactor, Bin1} = sutils:conditional_extract(
        Rest, 
        exist_mask:get_lat_scale_factor(EMrec), 
        4, 
        fun stanag_types:sa32_to_float/1, 
        1.0),

    {LonScaleFactor, Bin2} = sutils:conditional_extract(
        Bin1, 
        exist_mask:get_lon_scale_factor(EMrec), 
        4, 
        fun stanag_types:ba32_to_float/1, 
        1.0),
    
    {SpuAlongTrack, Bin3} = sutils:conditional_extract(
        Bin2, 
        exist_mask:get_spu_cross_track(EMrec), 
        4, 
        fun stanag_types:i32_to_integer/1, 
        0),

    {SpuCrossTrack, Bin4} = sutils:conditional_extract(
        Bin3, 
        exist_mask:get_spu_cross_track(EMrec), 
        4, 
        fun stanag_types:i32_to_integer/1, 
        0),

    {SpuAlt, Bin5} = sutils:conditional_extract(
        Bin4, 
        exist_mask:get_spu_alt(EMrec), 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {SensorTrack, Bin6} = sutils:conditional_extract(
        Bin5, 
        exist_mask:get_sensor_track(EMrec), 
        2, 
        fun stanag_types:ba16_to_float/1, 
        0.0),

    {SensorSpeed, Bin7} = sutils:conditional_extract(
        Bin6, 
        exist_mask:get_sensor_speed(EMrec), 
        4, 
        fun stanag_types:i32_to_integer/1, 
        0),

    {SensorVertVel, Bin8} = sutils:conditional_extract(
        Bin7, 
        exist_mask:get_sensor_vert_vel(EMrec), 
        1, 
        fun stanag_types:s8_to_integer/1, 
        0),
    
    {SensorTrackUnc, Bin9} = sutils:conditional_extract(
        Bin8, 
        exist_mask:get_sensor_track_unc(EMrec), 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {SensorSpeedUnc, Bin10} = sutils:conditional_extract(
        Bin9, 
        exist_mask:get_sensor_speed_unc(EMrec), 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {SensorVertVelUnc, Bin11} = sutils:conditional_extract(
        Bin10, 
        exist_mask:get_sensor_vert_vel_unc(EMrec), 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {PlatHeading, Bin12} = sutils:conditional_extract(
        Bin11, 
        exist_mask:get_platform_heading(EMrec), 
        2, 
        fun stanag_types:ba16_to_float/1, 
        0.0),

    {PlatPitch, Bin13} = sutils:conditional_extract(
        Bin12, 
        exist_mask:get_platform_pitch(EMrec), 
        2, 
        fun stanag_types:sa16_to_float/1, 
        0.0),

    {PlatRoll, Bin14} = sutils:conditional_extract(
        Bin13, 
        exist_mask:get_platform_roll(EMrec), 
        2, 
        fun stanag_types:sa16_to_float/1, 
        0.0),

    {DwellCenterLat, Bin15} = sutils:conditional_extract(
        Bin14, 
        exist_mask:get_dwell_center_lat(EMrec), 
        4, 
        fun stanag_types:sa32_to_float/1, 
        0.0),

    {DwellCenterLon, Bin16} = sutils:conditional_extract(
        Bin15, 
        exist_mask:get_dwell_center_lon(EMrec), 
        4, 
        fun stanag_types:ba32_to_float/1, 
        0.0),

    {DwellRangeHalfExtent, Bin17} = sutils:conditional_extract(
        Bin16, 
        exist_mask:get_dwell_range_half_extent(EMrec), 
        2, 
        fun stanag_types:b16_to_float/1, 
        0.0),

    {DwellAngleHalfExtent, Bin18} = sutils:conditional_extract(
        Bin17, 
        exist_mask:get_dwell_angle_half_extent(EMrec), 
        2, 
        fun stanag_types:ba16_to_float/1, 
        0.0),

    {SensorHeading, Bin19} = sutils:conditional_extract(
        Bin18, 
        exist_mask:get_sensor_heading(EMrec), 
        2, 
        fun stanag_types:ba16_to_float/1, 
        0.0),

    {SensorPitch, Bin20} = sutils:conditional_extract(
        Bin19, 
        exist_mask:get_sensor_pitch(EMrec), 
        2, 
        fun stanag_types:sa16_to_float/1, 
        0.0),

    {SensorRoll, Bin21} = sutils:conditional_extract(
        Bin20, 
        exist_mask:get_sensor_roll(EMrec), 
        2, 
        fun stanag_types:sa16_to_float/1, 
        0.0),

    {MDV, Bin22} = sutils:conditional_extract(
        Bin21, 
        exist_mask:get_mdv(EMrec), 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    TgtRepList = decode_target_report_list(Bin22, EMrec, TRC),

    {ok, #dwell_segment{
        existence_mask = EMrec,
        revisit_index = RI,
        dwell_index = DI,
        last_dwell_of_revisit = decode_last_dwell_of_revisit(LD),
        target_report_count = TRC,
        dwell_time = DT,
        sensor_lat = stanag_types:sa32_to_float(SLat),
        sensor_lon = stanag_types:ba32_to_float(SLon),
        sensor_alt = stanag_types:s32_to_integer(SAlt),
        lat_scale_factor = LatScaleFactor,
        lon_scale_factor = LonScaleFactor,
        spu_along_track = SpuAlongTrack,
        spu_cross_track = SpuCrossTrack,
        spu_alt = SpuAlt,
        sensor_track = SensorTrack,
        sensor_speed = SensorSpeed,
        sensor_vert_vel = SensorVertVel,
        sensor_track_unc = SensorTrackUnc,
        sensor_speed_unc = SensorSpeedUnc,
        sensor_vert_vel_unc = SensorVertVelUnc,
        platform_heading = PlatHeading,
        platform_pitch = PlatPitch,
        platform_roll = PlatRoll,
        dwell_center_lat = DwellCenterLat,
        dwell_center_lon = DwellCenterLon,
        dwell_range_half_extent = DwellRangeHalfExtent,
        dwell_angle_half_extent = DwellAngleHalfExtent,
        sensor_heading = SensorHeading,
        sensor_pitch = SensorPitch,
        sensor_roll = SensorRoll,
        mdv = MDV,
        targets = TgtRepList}};
decode(_) ->
    {error, dwell_mismatch}.

%% Function to encode a dwell segment record in its binary form.
encode(DS) ->
    % Extract the existence mask from the incoming dwell segment.
    EM = get_existence_mask(DS),

    % Create a local function to wrap the check of the existence mask and 
    % the parameter encoding/appending.
    % Exploits the trick that the function to access the dwell segment 
    % fields is the same as that to access the corresponding existence mask
    % field.
    F = fun({EvalFun, EncFun}, Acc) ->
            case exist_mask:EvalFun(EM) of 
                1 ->
                    Param = dwell:EvalFun(DS),
                    PB = EncFun(Param),
                    <<Acc/binary,PB/binary>>;
                0 ->
                    Acc 
            end
        end,

    %% Encode does not yet cater for adding the actual target reports.
    ParamTable = [
        {get_revisit_index, fun stanag_types:integer_to_i16/1},
        {get_dwell_index, fun stanag_types:integer_to_i16/1},
        {get_last_dwell_of_revisit, fun encode_last_dwell_of_revisit/1},
        {get_target_report_count, fun stanag_types:integer_to_i16/1},
        {get_dwell_time, fun stanag_types:integer_to_i32/1},
        {get_sensor_lat, fun stanag_types:float_to_sa32/1},
        {get_sensor_lon, fun stanag_types:float_to_ba32/1},
        {get_sensor_alt, fun stanag_types:integer_to_s32/1},
        {get_lat_scale_factor, fun stanag_types:float_to_sa32/1},
        {get_lon_scale_factor, fun stanag_types:float_to_ba32/1},
        {get_spu_along_track, fun stanag_types:integer_to_i32/1},
        {get_spu_cross_track, fun stanag_types:integer_to_i32/1},
        {get_spu_alt, fun stanag_types:integer_to_i16/1},
        {get_sensor_track, fun stanag_types:float_to_ba16/1},
        {get_sensor_speed, fun stanag_types:integer_to_i32/1},
        {get_sensor_vert_vel, fun stanag_types:integer_to_s8/1},
        {get_sensor_track_unc, fun stanag_types:integer_to_i8/1},
        {get_sensor_speed_unc, fun stanag_types:integer_to_i16/1},
        {get_sensor_vert_vel_unc, fun stanag_types:integer_to_i16/1},
        {get_platform_heading, fun stanag_types:float_to_ba16/1},
        {get_platform_pitch, fun stanag_types:float_to_sa16/1},
        {get_platform_roll, fun stanag_types:float_to_sa16/1},
        {get_dwell_center_lat, fun stanag_types:float_to_sa32/1},
        {get_dwell_center_lon, fun stanag_types:float_to_ba32/1},
        {get_dwell_range_half_extent, fun stanag_types:float_to_b16/1},
        {get_dwell_angle_half_extent, fun stanag_types:float_to_ba16/1},
        {get_sensor_heading, fun stanag_types:float_to_sa16/1},
        {get_sensor_pitch, fun stanag_types:float_to_sa16/1},
        {get_sensor_roll, fun stanag_types:float_to_sa16/1},
        {get_mdv, fun stanag_types:integer_to_i8/1}],
       
    EMenc = exist_mask:encode(EM),
    % Produce a binary dwell segment, missing only the target reports.
    Bin1 = lists:foldl(F, EMenc, ParamTable),
    % Append any target reports.
    encode_target_reports(get_target_report_count(DS), get_targets(DS), EM, Bin1).

%% Helper function for the dwell encode: encodes all of the target reports.
%% InitBin can be set to the dwell report prior to adding the target reports
%% so that the target reports are automatically added to the end of the 
%% dwell segment.
encode_target_reports(0, _RepList, _EM, InitBin) -> 
    InitBin;
encode_target_reports(RepCount, RepList, EM, InitBin) 
    when RepCount =:= length(RepList) ->

    F = fun(Rep, Acc) ->
            Bin = tgt_report:encode(Rep, EM),
            <<Acc/binary,Bin/binary>>
        end,
    lists:foldl(F, InitBin, RepList).

%% Function to create a new dwell report structure from the specified fields.
new(Fields) ->
    % Local function to pull the parameter from the list or supply a default
    % value.
    F = fun(P, L, Default) ->
            case lists:keyfind(P, 1, L) of
                {P, V} -> V;
                false  -> Default    
            end
        end,

    #dwell_segment{
        existence_mask = F(existence_mask, Fields, 0),
        revisit_index = F(revisit_index, Fields, 0),
        dwell_index = F(dwell_index, Fields, 0),
        last_dwell_of_revisit = F(last_dwell_of_revisit, Fields, 0),
        target_report_count = F(target_report_count, Fields, 0),
        dwell_time = F(dwell_time, Fields, 0),
        sensor_lat = F(sensor_lat, Fields, 0),
        sensor_lon = F(sensor_lon, Fields, 0),
        sensor_alt = F(sensor_alt, Fields, 0),
        lat_scale_factor = F(lat_scale_factor, Fields, 0),
        lon_scale_factor = F(lon_scale_factor, Fields, 0),
        spu_along_track = F(spu_along_track, Fields, 0),
        spu_cross_track = F(spu_cross_track, Fields, 0),
        spu_alt = F(spu_alt, Fields, 0),
        sensor_track = F(sensor_track, Fields, 0),
        sensor_speed = F(sensor_speed, Fields, 0),
        sensor_vert_vel = F(sensor_vert_vel, Fields, 0),
        sensor_track_unc = F(sensor_track_unc, Fields, 0),
        sensor_speed_unc = F(sensor_speed_unc, Fields, 0),
        sensor_vert_vel_unc = F(sensor_vert_vel_unc, Fields, 0),
        platform_heading = F(platform_heading, Fields, 0),
        platform_pitch = F(platform_pitch, Fields, 0),
        platform_roll = F(platform_roll, Fields, 0),
        dwell_center_lat = F(dwell_center_lat, Fields, 0),
        dwell_center_lon = F(dwell_center_lon, Fields, 0),
        dwell_range_half_extent = F(dwell_range_half_extent, Fields, 0),
        dwell_angle_half_extent = F(dwell_angle_half_extent, Fields, 0),
        sensor_heading = F(sensor_heading, Fields, 0),
        sensor_pitch = F(sensor_pitch, Fields, 0),
        sensor_roll = F(sensor_roll, Fields, 0),
        mdv = F(mdv, Fields, 0),
        targets = F(targets, Fields, [])}.

%% Convenience wrapper to calculate the expected size of a dwell segment
%% once encoded.
payload_size(#dwell_segment{existence_mask = EM, target_report_count = TC}) ->
    payload_size(EM, TC).

%% Function to calculate the size of an encoded dwell segment payload from 
%% the existence mask and the target report count.
payload_size(EM, TgtRepCount) ->
    SizeList = [         
        {get_revisit_index, 2},
        {get_dwell_index, 2},
        {get_last_dwell_of_revisit, 1},
        {get_target_report_count, 2},
        {get_dwell_time, 4},
        {get_sensor_lat, 4},
        {get_sensor_lon, 4},
        {get_sensor_alt, 4},
        {get_lat_scale_factor, 4},
        {get_lon_scale_factor, 4},
        {get_spu_along_track, 4},
        {get_spu_cross_track, 4},
        {get_spu_alt, 2},
        {get_sensor_track, 2},
        {get_sensor_speed, 4},
        {get_sensor_vert_vel, 1},
        {get_sensor_track_unc, 1},
        {get_sensor_speed_unc, 2},
        {get_sensor_vert_vel_unc, 2},
        {get_platform_heading, 2},
        {get_platform_pitch, 2},
        {get_platform_roll, 2},
        {get_dwell_center_lat, 4},
        {get_dwell_center_lon, 4},
        {get_dwell_range_half_extent, 2},
        {get_dwell_angle_half_extent, 2},
        {get_sensor_heading, 2},
        {get_sensor_pitch, 2},
        {get_sensor_roll, 2},
        {get_mdv, 1}],
       
    % Define a function to accumulate the size.
    F = fun({GetF, Size}, Acc) ->
            case exist_mask:GetF(EM) of
                1 -> Acc + Size;
                0 -> Acc
            end
        end,

    % Accumulate the total size for all the included parameters (excluding
    % the target reports). Initial size of 8 is to allow for the existence
    % mask itself.
    DwellSize = lists:foldl(F, 8, SizeList),

    % Calculate the size for the target reports.
    TgtRepSize = TgtRepCount * tgt_report:payload_size(EM),

    % Return the combined total of the dwell and the target reports.
    DwellSize + TgtRepSize.

%% Function to convert the dwell segment into a dictionary.
to_dict(DS) ->
    % Extract the existence mask from the incoming dwell segment.
    EM = get_existence_mask(DS),

    % Table definition with of the form [{Name, Accessor}].
    % The accessor function name is used with both the dwell segment and the
    % existence mask.
    ParamTable = [
        {revisit_index, get_revisit_index},
        {dwell_index, get_dwell_index},
        {last_dwell_of_revisit, get_last_dwell_of_revisit},
        {target_report_count, get_target_report_count},
        {dwell_time, get_dwell_time},
        {sensor_lat, get_sensor_lat},
        {sensor_lon, get_sensor_lon},
        {sensor_alt, get_sensor_alt},
        {lat_scale_factor, get_lat_scale_factor},
        {lon_scale_factor, get_lon_scale_factor},
        {spu_along_track, get_spu_along_track},
        {spu_cross_track, get_spu_cross_track},
        {spu_alt, get_spu_alt},
        {sensor_track, get_sensor_track},
        {sensor_speed, get_sensor_speed},
        {sensor_vert_vel, get_sensor_vert_vel},
        {sensor_track_unc, get_sensor_track_unc},
        {sensor_speed_unc, get_sensor_speed_unc},
        {sensor_vert_vel_unc, get_sensor_vert_vel_unc},
        {platform_heading, get_platform_heading},
        {platform_pitch, get_platform_pitch},
        {platform_roll, get_platform_roll},
        {dwell_center_lat, get_dwell_center_lat},
        {dwell_center_lon, get_dwell_center_lon},
        {dwell_range_half_extent, get_dwell_range_half_extent},
        {dwell_angle_half_extent, get_dwell_angle_half_extent},
        {sensor_heading, get_sensor_heading},
        {sensor_pitch, get_sensor_pitch},
        {sensor_roll, get_sensor_roll},
        {get_mdv, get_mdv}],
    
    % Function to convert each field present in the existence mask into a 
    % dictionary element.
    F = fun({Param, GetFn}, Acc) ->
            case exist_mask:GetFn(EM) of
                1 ->
                   P = dwell:GetFn(DS),
                   dict:store(Param, P, Acc);
                0 ->
                    Acc
            end
        end,

    Dict1 = lists:foldl(F, dict:new(), ParamTable),

    % Add the target reports. Convert a list of records to a list of 
    % dictionaries.
    Targets = dwell:get_targets(DS),
    T = fun(TgtRep) ->
            tgt_report:to_dict(TgtRep, EM)
        end,
    TgtDictList = lists:map(T, Targets),
    dict:store(targets, TgtDictList, Dict1).

decode_last_dwell_of_revisit(0) -> additional_dwells;
decode_last_dwell_of_revisit(1) -> no_additional_dwells.

encode_last_dwell_of_revisit(additional_dwells) -> <<0>>;
encode_last_dwell_of_revisit(no_additional_dwells) -> <<1>>.

%% Function to walk through a binary containing a number of target reports,
%% decoding each returning as a list of reports.
decode_target_report_list(Bin, EM, TgtCount) ->
    decode_target_report_list(Bin, EM, TgtCount, []).

%% Helper function with the accumulator.
decode_target_report_list(_Bin, _EM, 0, AccTgts) ->
    lists:reverse(AccTgts);
decode_target_report_list(Bin, EM, TgtCount, AccTgts) when TgtCount > 0 ->
    {ok, TR, Rem} = tgt_report:decode(Bin, EM),
    decode_target_report_list(Rem, EM, TgtCount-1, [TR|AccTgts]).

display(DS) ->
    io:format("****************************************~n"),
    io:format("** @dwell~n"),
    EM = DS#dwell_segment.existence_mask,
    exist_mask:display(EM),
    io:format("Revisit index: ~p~n", [get_revisit_index(DS)]),
    io:format("Dwell index: ~p~n", [get_dwell_index(DS)]),
    io:format("Last dwell of revisit: ~p~n", [get_last_dwell_of_revisit(DS)]),
    io:format("Target report count: ~p~n", [get_target_report_count(DS)]),
    io:format("Dwell time: ~p~n", [get_dwell_time(DS)]),
    io:format("Sensor Lat.: ~p~n", [get_sensor_lat(DS)]),
    io:format("Sensor Lon.: ~p~n", [get_sensor_lon(DS)]),
    io:format("Sensor alt. (cm): ~p~n", [get_sensor_alt(DS)]),
    sutils:conditional_display("Lat. scale factor: ~p~n", [get_lat_scale_factor(DS)], exist_mask:get_lat_scale_factor(EM)),
    sutils:conditional_display("Lon. scale factor: ~p~n", [get_lon_scale_factor(DS)], exist_mask:get_lon_scale_factor(EM)),
    sutils:conditional_display("SPU along track: ~p~n", [get_spu_along_track(DS)], exist_mask:get_spu_along_track(EM)),
    sutils:conditional_display("SPU cross track: ~p~n", [get_spu_cross_track(DS)], exist_mask:get_spu_cross_track(EM)),
    sutils:conditional_display("SPU alt: ~p~n", [get_spu_alt(DS)], exist_mask:get_spu_alt(EM)),
    sutils:conditional_display("Sensor track: ~p~n", [get_sensor_track(DS)], exist_mask:get_sensor_track(EM)),
    sutils:conditional_display("Sensor speed: ~p~n", [get_sensor_speed(DS)], exist_mask:get_sensor_speed(EM)),
    sutils:conditional_display("Sensor vert. vel.: ~p~n", [get_sensor_vert_vel(DS)], exist_mask:get_sensor_vert_vel(EM)),
    sutils:conditional_display("Sensor track unc.: ~p~n", [get_sensor_track_unc(DS)], exist_mask:get_sensor_track_unc(EM)),
    sutils:conditional_display("Sensor speed unc.: ~p~n", [get_sensor_speed_unc(DS)], exist_mask:get_sensor_speed_unc(EM)),
    sutils:conditional_display("Sensor vert. vel. unc.: ~p~n", [get_sensor_vert_vel_unc(DS)], exist_mask:get_sensor_vert_vel_unc(EM)),
    sutils:conditional_display("Platform heading: ~p~n", [get_platform_heading(DS)], exist_mask:get_platform_heading(EM)),
    sutils:conditional_display("Platform pitch: ~p~n", [get_platform_pitch(DS)], exist_mask:get_platform_pitch(EM)),
    sutils:conditional_display("Platform roll: ~p~n", [get_platform_roll(DS)], exist_mask:get_platform_roll(EM)),
    sutils:conditional_display("Dwell centre Lat.: ~p~n", [get_dwell_center_lat(DS)], exist_mask:get_dwell_center_lat(EM)),
    sutils:conditional_display("Dwell centre Lon.: ~p~n", [get_dwell_center_lon(DS)], exist_mask:get_dwell_center_lon(EM)),
    sutils:conditional_display("Dwell range half extent: ~p~n", [get_dwell_range_half_extent(DS)], exist_mask:get_dwell_range_half_extent(EM)),
    sutils:conditional_display("Dwell angle half extent: ~p~n", [get_dwell_angle_half_extent(DS)], exist_mask:get_dwell_angle_half_extent(EM)),
    sutils:conditional_display("Sensor heading: ~p~n", [get_sensor_heading(DS)], exist_mask:get_sensor_heading(EM)),
    sutils:conditional_display("Sensor pitch: ~p~n", [get_sensor_pitch(DS)], exist_mask:get_sensor_pitch(EM)),
    sutils:conditional_display("Sensor roll: ~p~n", [get_sensor_roll(DS)], exist_mask:get_sensor_roll(EM)),
    sutils:conditional_display("MDV: ~p~n", [get_mdv(DS)], exist_mask:get_mdv(EM)),
    F = fun(TR) -> tgt_report:display(TR, EM) end,
    lists:map(F, DS#dwell_segment.targets).

%% @doc Function to display the contents of a dwell segment.
-spec to_csv_iolist(DS::dwell_segment()) -> iolist().
to_csv_iolist(DS) ->
    EM = dwell:get_existence_mask(DS),
    F = fun({FmtStr, FnName}) ->
            Args = [dwell:FnName(DS)],
            ExistBit = exist_mask:FnName(EM),
            sutils:conditional_format(FmtStr, Args, ExistBit)       
        end,

    Params = 
        [{"~p,", get_revisit_index},
         {"~p,", get_dwell_index},
         {"~p,", get_last_dwell_of_revisit},
         {"~p,", get_target_report_count},
         {"~p,", get_dwell_time},
         {"~p,", get_sensor_lat},
         {"~p,", get_sensor_lon},
         {"~p,", get_sensor_alt},
         {"~p,", get_lat_scale_factor},
         {"~p,", get_lon_scale_factor},
         {"~p,", get_spu_along_track},
         {"~p,", get_spu_cross_track},
         {"~p,", get_spu_alt},
         {"~p,", get_sensor_track},
         {"~p,", get_sensor_speed},
         {"~p,", get_sensor_vert_vel},
         {"~p,", get_sensor_track_unc},
         {"~p,", get_sensor_speed_unc},
         {"~p,", get_sensor_vert_vel_unc},
         {"~p,", get_platform_heading},
         {"~p,", get_platform_pitch},
         {"~p,", get_platform_roll},
         {"~p,", get_dwell_center_lat},
         {"~p,", get_dwell_center_lon},
         {"~p,", get_dwell_range_half_extent},
         {"~p,", get_dwell_angle_half_extent},
         {"~p,", get_sensor_heading},
         {"~p,", get_sensor_pitch},
         {"~p,", get_sensor_roll},
         {"~p", get_mdv}],

    DwellList = lists:map(F, Params),

    Tgts = get_targets(DS),
    TgtIO = [tgt_report:to_csv_iolist(T, EM) || T <- Tgts],

    %% Prefix the line identifier and add all the targets (which will span
    %% multiple lines).
    %% Also add an empty field placeholder for the existence mask.
    ["DS,,"|DwellList] ++ io_lib:format("~n", []) ++ TgtIO.

%% @doc Set the dwell time in an existing dwell segment record. Used for data 
%% replay tasks.
set_dwell_time(#dwell_segment{} = DS, DwellTimeMS) ->
    DS#dwell_segment{dwell_time = DwellTimeMS}.

%% @doc Update the targets in a dwell segment. Updates the target report count 
%% in the dwell segment too.
update_targets(#dwell_segment{} = DS, NewTargets) when is_list(NewTargets) ->
    TgtCount = length(NewTargets),
    DS#dwell_segment{
        targets = NewTargets,  
        target_report_count = TgtCount}.

%% Accessor functions to allow access to the record fields with out creating 
%% client dependencies on the actual structure.
get_existence_mask(#dwell_segment{existence_mask = X}) -> X.
get_revisit_index(#dwell_segment{revisit_index = X}) -> X.
get_dwell_index(#dwell_segment{dwell_index = X}) -> X.
get_last_dwell_of_revisit(#dwell_segment{last_dwell_of_revisit = X}) -> X.
get_target_report_count(#dwell_segment{target_report_count = X}) -> X.
get_dwell_time(#dwell_segment{dwell_time = X}) -> X.
get_sensor_lat(#dwell_segment{sensor_lat = X}) -> X.
get_sensor_lon(#dwell_segment{sensor_lon = X}) -> X.
get_sensor_alt(#dwell_segment{sensor_alt = X}) -> X.
get_lat_scale_factor(#dwell_segment{lat_scale_factor = X}) -> X.
get_lon_scale_factor(#dwell_segment{lon_scale_factor = X}) -> X.
get_spu_along_track(#dwell_segment{spu_along_track = X}) -> X.
get_spu_cross_track(#dwell_segment{spu_cross_track = X}) -> X.
get_spu_alt(#dwell_segment{spu_alt = X}) -> X.
get_sensor_track(#dwell_segment{sensor_track = X}) -> X.
get_sensor_speed(#dwell_segment{sensor_speed = X}) -> X.
get_sensor_vert_vel(#dwell_segment{sensor_vert_vel = X}) -> X.
get_sensor_track_unc(#dwell_segment{sensor_track_unc = X}) -> X.
get_sensor_speed_unc(#dwell_segment{sensor_speed_unc = X}) -> X.
get_sensor_vert_vel_unc(#dwell_segment{sensor_vert_vel_unc = X}) -> X.
get_platform_heading(#dwell_segment{platform_heading = X}) -> X.
get_platform_pitch(#dwell_segment{platform_pitch = X}) -> X.
get_platform_roll(#dwell_segment{platform_roll = X}) -> X.
get_dwell_center_lat(#dwell_segment{dwell_center_lat = X}) -> X.
get_dwell_center_lon(#dwell_segment{dwell_center_lon = X}) -> X.
get_dwell_range_half_extent(#dwell_segment{dwell_range_half_extent = X}) -> X.
get_dwell_angle_half_extent(#dwell_segment{dwell_angle_half_extent = X}) -> X.
get_sensor_heading(#dwell_segment{sensor_heading = X}) -> X.
get_sensor_pitch(#dwell_segment{sensor_pitch = X}) -> X.
get_sensor_roll(#dwell_segment{sensor_roll = X}) -> X.
get_mdv(#dwell_segment{mdv = X}) -> X.
get_targets(#dwell_segment{targets = X}) -> X.

