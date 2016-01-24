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
-module(exist_mask).

-export([
    decode/1, 
    display/1
    
    
    ]).

-record(exist_mask, {
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
    mti_report_index,
    target_hr_lat,
    target_hr_lon,
    target_delta_lat,
    target_delta_lon,
    geodetic_height,
    target_vel_los,
    target_wrap_velocity,
    target_snr,
    target_classification,
    target_class_prob,
    target_slant_range_unc,
    target_cross_range_unc,
    target_height_unc,
    target_rad_vel_unc,
    truth_tag_app,
    truth_tag_entity,
    target_rcs}).

%% Function to decode the existance mask. Will crash caller if the mask 
%% does not have the mandatory bits set.
decode(<<16#FF, 
    D10:1,D11:1,D12:1,D13:1,D14:1,D15:1,D16:1,D17:1,
    D18:1,D19:1,D20:1,D21:1,D22:1,D23:1,1:1,1:1,
    1:1,1:1,D28:1,D29:1,D30:1,D31:1,D32_1:1,D32_2:1,
    D32_3:1,D32_4:1,D32_5:1,D32_6:1,D32_7:1,D32_8:1,D32_9:1,D32_10:1,
    D32_11:1,D32_12:1,D32_13:1,D32_14:1,D32_15:1,D32_16:1,D32_17:1,D32_18:1,
    _Spare:2/binary>>) ->

    #exist_mask{
        revisit_index = 1,
        dwell_index = 1,
        last_dwell_of_revisit = 1,
        target_report_count = 1,
        dwell_time = 1,
        sensor_lat = 1,
        sensor_lon = 1,
        sensor_alt = 1,
        lat_scale_factor = D10,
        lon_scale_factor = D11,
        spu_along_track = D12,
        spu_cross_track = D13,
        spu_alt = D14,
        sensor_track = D15,
        sensor_speed = D16,
        sensor_vert_vel = D17,
        sensor_track_unc = D18,
        sensor_speed_unc = D19,
        sensor_vert_vel_unc = D20,
        platform_heading = D21,
        platform_pitch = D22,
        platform_roll = D23,
        dwell_center_lat = 1,
        dwell_center_lon = 1,
        dwell_range_half_extent = 1,
        dwell_angle_half_extent = 1,
        sensor_heading = D28,
        sensor_pitch = D29,
        sensor_roll = D30,
        mdv = D31,
        mti_report_index = D32_1,
        target_hr_lat = D32_2,
        target_hr_lon = D32_3,
        target_delta_lat = D32_4,
        target_delta_lon = D32_5,
        geodetic_height = D32_6,
        target_vel_los = D32_7,
        target_wrap_velocity = D32_8,
        target_snr = D32_9,
        target_classification = D32_10,
        target_class_prob = D32_11,
        target_slant_range_unc = D32_12,
        target_cross_range_unc = D32_13,
        target_height_unc = D32_14,
        target_rad_vel_unc = D32_15,
        truth_tag_app = D32_16,
        truth_tag_entity = D32_17,
        target_rcs = D32_18}.

display(EM) ->
    io:format("Revisit index: ~p~n", [EM#exist_mask.revisit_index]),
    io:format("Dwell index: ~p~n", [EM#exist_mask.dwell_index]),
    io:format("Last dwell of revisit: ~p~n", [EM#exist_mask.last_dwell_of_revisit]),
    io:format("Target report count: ~p~n", [EM#exist_mask.target_report_count]),
    io:format("Dwell time: ~p~n", [EM#exist_mask.dwell_time]),
    io:format("Sensor lat.: ~p~n", [EM#exist_mask.sensor_lat]),
    io:format("Sensor lon.: ~p~n", [EM#exist_mask.sensor_lon]),
    io:format("Sensor alt.: ~p~n", [EM#exist_mask.sensor_alt]),
    io:format("Lat. scale factor: ~p~n", [EM#exist_mask.lat_scale_factor]),
    io:format("Lon. scale factor: ~p~n", [EM#exist_mask.lon_scale_factor]),
    io:format("Sensor pos. unc. along track: ~p~n", [EM#exist_mask.spu_along_track]),
    io:format("Sensor pos. unc. cross track: ~p~n", [EM#exist_mask.spu_cross_track]),
    io:format("Sensor pos. altitude: ~p~n", [EM#exist_mask.spu_alt]),
    io:format("Sensor track: ~p~n", [EM#exist_mask.sensor_track]),
    io:format("Sensor speed: ~p~n", [EM#exist_mask.sensor_speed]),
    io:format("Sensor vert. vel.: ~p~n", [EM#exist_mask.sensor_vert_vel]),
    io:format("Sensor track unc.: ~p~n", [EM#exist_mask.sensor_track_unc]),
    io:format("Sensor speed unc.: ~p~n", [EM#exist_mask.sensor_speed_unc]),
    io:format("Sensor vert. vel. unc.: ~p~n", [EM#exist_mask.sensor_vert_vel_unc]),
    io:format("Platform heading: ~p~n", [EM#exist_mask.platform_heading]),
    io:format("Platform pitch: ~p~n", [EM#exist_mask.platform_pitch]),
    io:format("Platform roll: ~p~n", [EM#exist_mask.platform_roll]),
    io:format("Dwell centre Lat.: ~p~n", [EM#exist_mask.dwell_center_lat]),
    io:format("Dwell centre Lon.: ~p~n", [EM#exist_mask.dwell_center_lon]),
    io:format("Dwell range half extent: ~p~n", [EM#exist_mask.dwell_range_half_extent]),
    io:format("Dwell angle half extent: ~p~n", [EM#exist_mask.dwell_angle_half_extent]),
    io:format("Sensor heading: ~p~n", [EM#exist_mask.sensor_heading]),
    io:format("Sensor pitch: ~p~n", [EM#exist_mask.sensor_pitch]),
    io:format("Sensor roll: ~p~n", [EM#exist_mask.sensor_roll]),
    io:format("MDV: ~p~n", [EM#exist_mask.mdv]),
    io:format("MTI report index: ~p~n", [EM#exist_mask.mti_report_index]),
    io:format("Target Hi-Res Lat.: ~p~n", [EM#exist_mask.target_hr_lat]),
    io:format("Target Hi-Res Lon.: ~p~n", [EM#exist_mask.target_hr_lon]),
    io:format("Target delta Lat.: ~p~n", [EM#exist_mask.target_delta_lat]),
    io:format("Target delta Lon.: ~p~n", [EM#exist_mask.target_delta_lon]),
    io:format("Target geodetic height.: ~p~n", [EM#exist_mask.geodetic_height]),
    io:format("Target vel. line of sight: ~p~n", [EM#exist_mask.target_vel_los]),
    io:format("Target wrap velocity: ~p~n", [EM#exist_mask.target_wrap_velocity]),
    io:format("Target SNR: ~p~n", [EM#exist_mask.target_snr]),
    io:format("Target classification: ~p~n", [EM#exist_mask.target_classification]),
    io:format("Target class. prob.: ~p~n", [EM#exist_mask.target_class_prob]),
    io:format("Target slant range unc.: ~p~n", [EM#exist_mask.target_slant_range_unc]),
    io:format("Target cross range unc.: ~p~n", [EM#exist_mask.target_cross_range_unc]),
    io:format("Target height unc.: ~p~n", [EM#exist_mask.target_height_unc]),
    io:format("Target rad. vel. unc.: ~p~n", [EM#exist_mask.target_rad_vel_unc]),
    io:format("Truth tag app.: ~p~n", [EM#exist_mask.truth_tag_app]),
    io:format("Truth tag entity: ~p~n", [EM#exist_mask.truth_tag_entity]),
    io:format("Target RCS: ~p~n", [EM#exist_mask.target_rcs]).


