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

-export([
    decode/1, 
    display/1]).

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

    #dwell_segment{
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
        targets = TgtRepList}.


decode_last_dwell_of_revisit(0) -> additional_dwells;
decode_last_dwell_of_revisit(1) -> no_additional_dwells.

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
    EM = DS#dwell_segment.existence_mask,
    exist_mask:display(EM),
    io:format("Revisit index: ~p~n", [DS#dwell_segment.revisit_index]),
    io:format("Dwell index: ~p~n", [DS#dwell_segment.dwell_index]),
    io:format("Last dwell of revisit: ~p~n", [DS#dwell_segment.last_dwell_of_revisit]),
    io:format("Target report count: ~p~n", [DS#dwell_segment.target_report_count]),
    io:format("Dwell time: ~p~n", [DS#dwell_segment.dwell_time]),
    io:format("Sensor Lat.: ~p~n", [DS#dwell_segment.sensor_lat]),
    io:format("Sensor Lon.: ~p~n", [DS#dwell_segment.sensor_lon]),
    io:format("Sensor alt. (cm): ~p~n", [DS#dwell_segment.sensor_alt]),
    sutils:conditional_display("Lat. scale factor: ~p~n", [DS#dwell_segment.lat_scale_factor], exist_mask:get_lat_scale_factor(EM)),
    sutils:conditional_display("Lon. scale factor: ~p~n", [DS#dwell_segment.lon_scale_factor], exist_mask:get_lon_scale_factor(EM)),
    sutils:conditional_display("SPU along track: ~p~n", [DS#dwell_segment.spu_along_track], exist_mask:get_spu_along_track(EM)),
    sutils:conditional_display("SPU cross track: ~p~n", [DS#dwell_segment.spu_cross_track], exist_mask:get_spu_cross_track(EM)),
    sutils:conditional_display("SPU alt: ~p~n", [DS#dwell_segment.spu_alt], exist_mask:get_spu_alt(EM)),
    sutils:conditional_display("Sensor track: ~p~n", [DS#dwell_segment.sensor_track], exist_mask:get_sensor_track(EM)),
    sutils:conditional_display("Sensor speed: ~p~n", [DS#dwell_segment.sensor_speed], exist_mask:get_sensor_speed(EM)),
    sutils:conditional_display("Sensor vert. vel.: ~p~n", [DS#dwell_segment.sensor_vert_vel], exist_mask:get_sensor_vert_vel(EM)),
    sutils:conditional_display("Sensor track unc.: ~p~n", [DS#dwell_segment.sensor_track_unc], exist_mask:get_sensor_track_unc(EM)),
    sutils:conditional_display("Sensor speed unc.: ~p~n", [DS#dwell_segment.sensor_speed_unc], exist_mask:get_sensor_speed_unc(EM)),
    sutils:conditional_display("Sensor vert. vel. unc.: ~p~n", [DS#dwell_segment.sensor_vert_vel_unc], exist_mask:get_sensor_vert_vel_unc(EM)),
    sutils:conditional_display("Platform heading: ~p~n", [DS#dwell_segment.platform_heading], exist_mask:get_platform_heading(EM)),
    sutils:conditional_display("Platform pitch: ~p~n", [DS#dwell_segment.platform_pitch], exist_mask:get_platform_pitch(EM)),
    sutils:conditional_display("Platform roll: ~p~n", [DS#dwell_segment.platform_roll], exist_mask:get_platform_roll(EM)),
    sutils:conditional_display("Dwell centre Lat.: ~p~n", [DS#dwell_segment.dwell_center_lat], exist_mask:get_dwell_center_lat(EM)),
    sutils:conditional_display("Dwell centre Lon.: ~p~n", [DS#dwell_segment.dwell_center_lon], exist_mask:get_dwell_center_lon(EM)),
    sutils:conditional_display("Dwell range half extent: ~p~n", [DS#dwell_segment.dwell_range_half_extent], exist_mask:get_dwell_range_half_extent(EM)),
    sutils:conditional_display("Dwell angle half extent: ~p~n", [DS#dwell_segment.dwell_angle_half_extent], exist_mask:get_dwell_angle_half_extent(EM)),
    sutils:conditional_display("Sensor heading: ~p~n", [DS#dwell_segment.sensor_heading], exist_mask:get_sensor_heading(EM)),
    sutils:conditional_display("Sensor pitch: ~p~n", [DS#dwell_segment.sensor_pitch], exist_mask:get_sensor_pitch(EM)),
    sutils:conditional_display("Sensor roll: ~p~n", [DS#dwell_segment.sensor_roll], exist_mask:get_sensor_roll(EM)),
    sutils:conditional_display("MDV: ~p~n", [DS#dwell_segment.mdv], exist_mask:get_mdv(EM)),
    F = fun(TR) -> tgt_report:display(TR, EM) end,
    lists:map(F, DS#dwell_segment.targets).


