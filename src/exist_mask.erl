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
    encode/1,
    new/1,
    display/1,
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
    get_mti_report_index/1,
    get_target_hr_lat/1,
    get_target_hr_lon/1,
    get_target_delta_lat/1,
    get_target_delta_lon/1,
    get_geodetic_height/1,
    get_target_vel_los/1,
    get_target_wrap_velocity/1,
    get_target_snr/1,
    get_target_classification/1,
    get_target_class_prob/1,
    get_target_slant_range_unc/1,
    get_target_cross_range_unc/1,
    get_target_height_unc/1,
    get_target_rad_vel_unc/1,
    get_truth_tag_app/1,
    get_truth_tag_entity/1,
    get_target_rcs/1]).

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

-opaque exist_mask() :: #exist_mask{}.
-export_type([exist_mask/0]).

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

%% Function to take an existence mask record and return a binary encoding of
%% the fields.
encode(#exist_mask{
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
    target_rcs = D32_18}) ->
    
    % Encode all of the fields as a binary.
    <<16#FF,D10:1,D11:1,D12:1,D13:1,D14:1,D15:1,D16:1,D17:1,D18:1,D19:1,
      D20:1,D21:1,D22:1,D23:1,1:1,1:1,1:1,1:1,D28:1,D29:1,D30:1,D31:1,
      D32_1:1,D32_2:1,D32_3:1,D32_4:1,D32_5:1,D32_6:1,D32_7:1,D32_8:1,
      D32_9:1,D32_10:1,D32_11:1,D32_12:1,D32_13:1,D32_14:1,D32_15:1,D32_16:1,
      D32_17:1,D32_18:1,0,0>>.

%% Function to allow the caller to construct an existence mask by supplying 
%% a list of atoms specifiying the optional and conditional fields to set.
new(Fields) when is_list(Fields) ->
    % Local function to check if a parameter exists in the list supplied
    % and return 1 if it is and 0 if not.
    F = fun(X) ->
            bool_to_int(lists:member(X, Fields))
        end,

    #exist_mask{
        revisit_index = 1,
        dwell_index = 1,
        last_dwell_of_revisit = 1,
        target_report_count = 1,
        dwell_time = 1,
        sensor_lat = 1,
        sensor_lon = 1,
        sensor_alt = 1,
        lat_scale_factor = F(lat_scale_factor),  
        lon_scale_factor = F(lon_scale_factor),  
        spu_along_track = F(spu_along_track),  
        spu_cross_track = F(spu_cross_track),  
        spu_alt = F(spu_alt),  
        sensor_track = F(sensor_track),  
        sensor_speed = F(sensor_speed),  
        sensor_vert_vel = F(sensor_vert_vel),  
        sensor_track_unc = F(sensor_track_unc),  
        sensor_speed_unc = F(sensor_speed_unc),  
        sensor_vert_vel_unc = F(sensor_vert_vel_unc),  
        platform_heading = F(platform_heading),  
        platform_pitch = F(platform_pitch),  
        platform_roll = F(platform_roll),  
        dwell_center_lat = 1,
        dwell_center_lon = 1,
        dwell_range_half_extent = 1,
        dwell_angle_half_extent = 1,
        sensor_heading = F(sensor_heading),  
        sensor_pitch = F(sensor_pitch),  
        sensor_roll = F(sensor_roll),  
        mdv = F(mdv),  
        mti_report_index = F(mti_report_index),  
        target_hr_lat = F(target_hr_lat),  
        target_hr_lon = F(target_hr_lon),  
        target_delta_lat = F(target_delta_lat),  
        target_delta_lon = F(target_delta_lon),  
        geodetic_height = F(geodetic_height),  
        target_vel_los = F(target_vel_los),  
        target_wrap_velocity = F(target_wrap_velocity),  
        target_snr = F(target_snr),  
        target_classification = F(target_classification),  
        target_class_prob = F(target_class_prob),  
        target_slant_range_unc = F(target_slant_range_unc),  
        target_cross_range_unc = F(target_cross_range_unc),  
        target_height_unc = F(target_height_unc),  
        target_rad_vel_unc = F(target_rad_vel_unc),  
        truth_tag_app = F(truth_tag_app),  
        truth_tag_entity = F(truth_tag_entity),  
        target_rcs = F(target_rcs)}.

bool_to_int(true) -> 1;
bool_to_int(false) -> 0.

display(EM) ->
    io:format("****************************************~n"),
    io:format("** @existence_mask~n"),
    io:format("Revisit index: ~p~n", [get_revisit_index(EM)]),
    io:format("Dwell index: ~p~n", [get_dwell_index(EM)]),
    io:format("Last dwell of revisit: ~p~n", [get_last_dwell_of_revisit(EM)]),
    io:format("Target report count: ~p~n", [get_target_report_count(EM)]),
    io:format("Dwell time: ~p~n", [get_dwell_time(EM)]),
    io:format("Sensor lat.: ~p~n", [get_sensor_lat(EM)]),
    io:format("Sensor lon.: ~p~n", [get_sensor_lon(EM)]),
    io:format("Sensor alt.: ~p~n", [get_sensor_alt(EM)]),
    io:format("Lat. scale factor: ~p~n", [get_lat_scale_factor(EM)]),
    io:format("Lon. scale factor: ~p~n", [get_lon_scale_factor(EM)]),
    io:format("Sensor pos. unc. along track: ~p~n", [get_spu_along_track(EM)]),
    io:format("Sensor pos. unc. cross track: ~p~n", [get_spu_cross_track(EM)]),
    io:format("Sensor pos. altitude: ~p~n", [get_spu_alt(EM)]),
    io:format("Sensor track: ~p~n", [get_sensor_track(EM)]),
    io:format("Sensor speed: ~p~n", [get_sensor_speed(EM)]),
    io:format("Sensor vert. vel.: ~p~n", [get_sensor_vert_vel(EM)]),
    io:format("Sensor track unc.: ~p~n", [get_sensor_track_unc(EM)]),
    io:format("Sensor speed unc.: ~p~n", [get_sensor_speed_unc(EM)]),
    io:format("Sensor vert. vel. unc.: ~p~n", [get_sensor_vert_vel_unc(EM)]),
    io:format("Platform heading: ~p~n", [get_platform_heading(EM)]),
    io:format("Platform pitch: ~p~n", [get_platform_pitch(EM)]),
    io:format("Platform roll: ~p~n", [get_platform_roll(EM)]),
    io:format("Dwell centre Lat.: ~p~n", [get_dwell_center_lat(EM)]),
    io:format("Dwell centre Lon.: ~p~n", [get_dwell_center_lon(EM)]),
    io:format("Dwell range half extent: ~p~n", [get_dwell_range_half_extent(EM)]),
    io:format("Dwell angle half extent: ~p~n", [get_dwell_angle_half_extent(EM)]),
    io:format("Sensor heading: ~p~n", [get_sensor_heading(EM)]),
    io:format("Sensor pitch: ~p~n", [get_sensor_pitch(EM)]),
    io:format("Sensor roll: ~p~n", [get_sensor_roll(EM)]),
    io:format("MDV: ~p~n", [get_mdv(EM)]),
    io:format("MTI report index: ~p~n", [get_mti_report_index(EM)]),
    io:format("Target Hi-Res Lat.: ~p~n", [get_target_hr_lat(EM)]),
    io:format("Target Hi-Res Lon.: ~p~n", [get_target_hr_lon(EM)]),
    io:format("Target delta Lat.: ~p~n", [get_target_delta_lat(EM)]),
    io:format("Target delta Lon.: ~p~n", [get_target_delta_lon(EM)]),
    io:format("Target geodetic height.: ~p~n", [get_geodetic_height(EM)]),
    io:format("Target vel. line of sight: ~p~n", [get_target_vel_los(EM)]),
    io:format("Target wrap velocity: ~p~n", [get_target_wrap_velocity(EM)]),
    io:format("Target SNR: ~p~n", [get_target_snr(EM)]),
    io:format("Target classification: ~p~n", [get_target_classification(EM)]),
    io:format("Target class. prob.: ~p~n", [get_target_class_prob(EM)]),
    io:format("Target slant range unc.: ~p~n", [get_target_slant_range_unc(EM)]),
    io:format("Target cross range unc.: ~p~n", [get_target_cross_range_unc(EM)]),
    io:format("Target height unc.: ~p~n", [get_target_height_unc(EM)]),
    io:format("Target rad. vel. unc.: ~p~n", [get_target_rad_vel_unc(EM)]),
    io:format("Truth tag app.: ~p~n", [get_truth_tag_app(EM)]),
    io:format("Truth tag entity: ~p~n", [get_truth_tag_entity(EM)]),
    io:format("Target RCS: ~p~n", [get_target_rcs(EM)]).

%% Functions to allow external modules to gain access to fields of the 
%% existence mask without knowledge of the internal representation.
get_revisit_index(EM) -> EM#exist_mask.revisit_index.
get_dwell_index(EM) -> EM#exist_mask.dwell_index.
get_last_dwell_of_revisit(EM) -> EM#exist_mask.last_dwell_of_revisit.
get_target_report_count(EM) -> EM#exist_mask.target_report_count.
get_dwell_time(EM) -> EM#exist_mask.dwell_time.
get_sensor_lat(EM) -> EM#exist_mask.sensor_lat.
get_sensor_lon(EM) -> EM#exist_mask.sensor_lon.
get_sensor_alt(EM) -> EM#exist_mask.sensor_alt.
get_lat_scale_factor(EM) -> EM#exist_mask.lat_scale_factor.
get_lon_scale_factor(EM) -> EM#exist_mask.lon_scale_factor.
get_spu_along_track(EM) -> EM#exist_mask.spu_along_track.
get_spu_cross_track(EM) -> EM#exist_mask.spu_cross_track.
get_spu_alt(EM) -> EM#exist_mask.spu_alt.
get_sensor_track(EM) -> EM#exist_mask.sensor_track.
get_sensor_speed(EM) -> EM#exist_mask.sensor_speed.
get_sensor_vert_vel(EM) -> EM#exist_mask.sensor_vert_vel.
get_sensor_track_unc(EM) -> EM#exist_mask.sensor_track_unc.
get_sensor_speed_unc(EM) -> EM#exist_mask.sensor_speed_unc.
get_sensor_vert_vel_unc(EM) -> EM#exist_mask.sensor_vert_vel_unc.
get_platform_heading(EM) -> EM#exist_mask.platform_heading.
get_platform_pitch(EM) -> EM#exist_mask.platform_pitch.
get_platform_roll(EM) -> EM#exist_mask.platform_roll.
get_dwell_center_lat(EM) -> EM#exist_mask.dwell_center_lat.
get_dwell_center_lon(EM) -> EM#exist_mask.dwell_center_lon.
get_dwell_range_half_extent(EM) -> EM#exist_mask.dwell_range_half_extent.
get_dwell_angle_half_extent(EM) -> EM#exist_mask.dwell_angle_half_extent.
get_sensor_heading(EM) -> EM#exist_mask.sensor_heading.
get_sensor_pitch(EM) -> EM#exist_mask.sensor_pitch.
get_sensor_roll(EM) -> EM#exist_mask.sensor_roll.
get_mdv(EM) -> EM#exist_mask.mdv.
get_mti_report_index(EM) -> EM#exist_mask.mti_report_index.
get_target_hr_lat(EM) -> EM#exist_mask.target_hr_lat.
get_target_hr_lon(EM) -> EM#exist_mask.target_hr_lon.
get_target_delta_lat(EM) -> EM#exist_mask.target_delta_lat.
get_target_delta_lon(EM) -> EM#exist_mask.target_delta_lon.
get_geodetic_height(EM) -> EM#exist_mask.geodetic_height.
get_target_vel_los(EM) -> EM#exist_mask.target_vel_los.
get_target_wrap_velocity(EM) -> EM#exist_mask.target_wrap_velocity.
get_target_snr(EM) -> EM#exist_mask.target_snr.
get_target_classification(EM) -> EM#exist_mask.target_classification.
get_target_class_prob(EM) -> EM#exist_mask.target_class_prob.
get_target_slant_range_unc(EM) -> EM#exist_mask.target_slant_range_unc.
get_target_cross_range_unc(EM) -> EM#exist_mask.target_cross_range_unc.
get_target_height_unc(EM) -> EM#exist_mask.target_height_unc.
get_target_rad_vel_unc(EM) -> EM#exist_mask.target_rad_vel_unc.
get_truth_tag_app(EM) -> EM#exist_mask.truth_tag_app.
get_truth_tag_entity(EM) -> EM#exist_mask.truth_tag_entity.
get_target_rcs(EM) -> EM#exist_mask.target_rcs.


