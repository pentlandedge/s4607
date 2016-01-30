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
    new/1,
    display/1,
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
        targets = TgtRepList}}.

%% Function to create a new dwell report structure from the specified fields.
new(Fields) ->
    % Local function to pull the parameter from the list or supply a default
    % value.
    F = fun(P, L) ->
            case lists:keyfind(P, 1, L) of
                {P, V} -> V;
                false  -> 0
            end
        end,

    #dwell_segment{
        existence_mask = F(existence_mask, Fields),
        revisit_index = F(revisit_index, Fields),
        dwell_index = F(dwell_index, Fields),
        last_dwell_of_revisit = F(last_dwell_of_revisit, Fields),
        target_report_count = F(target_report_count, Fields),
        dwell_time = F(dwell_time, Fields),
        sensor_lat = F(sensor_lat, Fields),
        sensor_lon = F(sensor_lon, Fields),
        sensor_alt = F(sensor_alt, Fields),
        lat_scale_factor = F(lat_scale_factor, Fields),
        lon_scale_factor = F(lon_scale_factor, Fields),
        spu_along_track = F(spu_along_track, Fields),
        spu_cross_track = F(spu_cross_track, Fields),
        spu_alt = F(spu_alt, Fields),
        sensor_track = F(sensor_track, Fields),
        sensor_speed = F(sensor_speed, Fields),
        sensor_vert_vel = F(sensor_vert_vel, Fields),
        sensor_track_unc = F(sensor_track_unc, Fields),
        sensor_speed_unc = F(sensor_speed_unc, Fields),
        sensor_vert_vel_unc = F(sensor_vert_vel_unc, Fields),
        platform_heading = F(platform_heading, Fields),
        platform_pitch = F(platform_pitch, Fields),
        platform_roll = F(platform_roll, Fields),
        dwell_center_lat = F(dwell_center_lat, Fields),
        dwell_center_lon = F(dwell_center_lon, Fields),
        dwell_range_half_extent = F(dwell_range_half_extent, Fields),
        dwell_angle_half_extent = F(dwell_angle_half_extent, Fields),
        sensor_heading = F(sensor_heading, Fields),
        sensor_pitch = F(sensor_pitch, Fields),
        sensor_roll = F(sensor_roll, Fields),
        mdv = F(mdv, Fields),
        targets = F(targets, Fields)}.

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

