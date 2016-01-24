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
-module(tgt_report).

-export([decode/2, display/2]).

-record(tgt_report, {
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


decode(TrBin, EM) ->
    
    {MRI, Rem1} = sutils:conditional_extract(
        TrBin, 
        EM#exist_mask.mti_report_index, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtHiResLat, Rem2} = sutils:conditional_extract(
        Rem1, 
        EM#exist_mask.target_hr_lat, 
        4, 
        fun stanag_types:sa32_to_float/1, 
        0.0),

    {TgtHiResLon, Rem3} = sutils:conditional_extract(
        Rem2, 
        EM#exist_mask.target_hr_lon, 
        4, 
        fun stanag_types:ba32_to_float/1, 
        0.0),

    {TgtDeltaLat, Rem4} = sutils:conditional_extract(
        Rem3, 
        EM#exist_mask.target_delta_lat, 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {TgtDeltaLon, Rem5} = sutils:conditional_extract(
        Rem4, 
        EM#exist_mask.target_delta_lon, 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {GeodHeight, Rem6} = sutils:conditional_extract(
        Rem5, 
        EM#exist_mask.geodetic_height, 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {TgtVelLos, Rem7} = sutils:conditional_extract(
        Rem6, 
        EM#exist_mask.target_vel_los, 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {TgtWrapVel, Rem8} = sutils:conditional_extract(
        Rem7, 
        EM#exist_mask.target_wrap_velocity, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtSnr, Rem9} = sutils:conditional_extract(
        Rem8, 
        EM#exist_mask.target_snr, 
        1, 
        fun stanag_types:s8_to_integer/1, 
        0),

    {TgtClassification, Rem10} = sutils:conditional_extract(
        Rem9, 
        EM#exist_mask.target_classification, 
        1, 
        fun decode_target_classification/1, 
        no_information_live_target),

    {TgtClassProb, Rem11} = sutils:conditional_extract(
        Rem10, 
        EM#exist_mask.target_class_prob, 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {TgtSlantRgeUnc, Rem12} = sutils:conditional_extract(
        Rem11, 
        EM#exist_mask.target_slant_range_unc, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtCrossRgeUnc, Rem13} = sutils:conditional_extract(
        Rem12, 
        EM#exist_mask.target_cross_range_unc, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtHeightUnc, Rem14} = sutils:conditional_extract(
        Rem13, 
        EM#exist_mask.target_height_unc, 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {TgtRadVelUnc, Rem15} = sutils:conditional_extract(
        Rem14, 
        EM#exist_mask.target_rad_vel_unc, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TruthTagApp, Rem16} = sutils:conditional_extract(
        Rem15, 
        EM#exist_mask.truth_tag_app, 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {TruthTagEnt, Rem17} = sutils:conditional_extract(
        Rem16, 
        EM#exist_mask.truth_tag_entity, 
        4, 
        fun stanag_types:i32_to_integer/1, 
        0),

    {TgtRcs, Rem18} = sutils:conditional_extract(
        Rem17, 
        EM#exist_mask.target_rcs, 
        1, 
        fun stanag_types:s8_to_integer/1, 
        0),
   
    TR = #tgt_report{
        mti_report_index = MRI,
        target_hr_lat = TgtHiResLat,
        target_hr_lon = TgtHiResLon,
        target_delta_lat = TgtDeltaLat,
        target_delta_lon = TgtDeltaLon,
        geodetic_height = GeodHeight,
        target_vel_los = TgtVelLos,
        target_wrap_velocity = TgtWrapVel,
        target_snr = TgtSnr,
        target_classification = TgtClassification,
        target_class_prob = TgtClassProb,
        target_slant_range_unc = TgtSlantRgeUnc,
        target_cross_range_unc = TgtCrossRgeUnc,
        target_height_unc = TgtHeightUnc,
        target_rad_vel_unc = TgtRadVelUnc,
        truth_tag_app = TruthTagApp,
        truth_tag_entity = TruthTagEnt,
        target_rcs = TgtRcs},
        
    {ok, TR, Rem18}.

decode_target_classification(<<Val:8>>) ->
    decode_target_classification(Val);
    
decode_target_classification(0) -> no_information_live_target;
decode_target_classification(1) -> tracked_vehicle_live_target;
decode_target_classification(2) -> wheeled_vehicle_live_target;
decode_target_classification(3) -> rotary_wing_aircraft_live_target;
decode_target_classification(4) -> fixed_wing_aircraft_live_target;
decode_target_classification(5) -> stationary_rotator_live_target;
decode_target_classification(6) -> maritime_live_target;
decode_target_classification(7) -> beacon_live_target;
decode_target_classification(8) -> amphibious_live_target;
decode_target_classification(9) -> person_live_target;
decode_target_classification(10) -> vehicle_live_target;
decode_target_classification(11) -> animal_live_target;
decode_target_classification(12) -> large_multiple_return_live_land_target;
decode_target_classification(13) -> large_multiple_return_live_maritime_target;
decode_target_classification(126) -> other_live_target;
decode_target_classification(127) -> unknown_live_target;
decode_target_classification(128) -> no_information_simulated_target;
decode_target_classification(129) -> tracked_vehicle_simulated_target;
decode_target_classification(130) -> wheeled_vehicle_simulated_target;
decode_target_classification(131) -> rotary_wing_aircraft_simulated_target;
decode_target_classification(132) -> fixed_wing_aircraft_simulated_target;
decode_target_classification(133) -> stationary_rotator_simulated_target;
decode_target_classification(134) -> maritime_simulated_target;
decode_target_classification(135) -> beacon_simulated_target;
decode_target_classification(136) -> amphibious_simulated_target;
decode_target_classification(137) -> person_simulated_target;
decode_target_classification(138) -> vehicle_simulated_target;
decode_target_classification(139) -> animal_simulated_target;
decode_target_classification(140) -> large_multiple_return_simulated_land_target;
decode_target_classification(141) -> large_multiple_return_simulated_maritime_target;
decode_target_classification(143) -> tagging_device;
decode_target_classification(254) -> other_simulated_target;
decode_target_classification(255) -> unknown_simulated_target;
decode_target_classification(_) -> reserved.
 
display(TR, EM) ->
    sutils:conditional_display("MTI report index: ~p~n", [TR#tgt_report.mti_report_index], EM#exist_mask.mti_report_index),
    sutils:conditional_display("Target HR Lat: ~p~n", [TR#tgt_report.target_hr_lat], EM#exist_mask.target_hr_lat),
    sutils:conditional_display("Target HR Lon: ~p~n", [TR#tgt_report.target_hr_lon], EM#exist_mask.target_hr_lon),
    sutils:conditional_display("Target Delta Lat: ~p~n", [TR#tgt_report.target_delta_lat], EM#exist_mask.target_delta_lat),
    sutils:conditional_display("Target Delta Lon: ~p~n", [TR#tgt_report.target_delta_lon], EM#exist_mask.target_delta_lon),
    sutils:conditional_display("Geodetic Height: ~p~n", [TR#tgt_report.geodetic_height], EM#exist_mask.geodetic_height),
    sutils:conditional_display("Target vel. LOS.: ~p~n", [TR#tgt_report.target_vel_los], EM#exist_mask.target_vel_los),
    sutils:conditional_display("Target wrap vel.: ~p~n", [TR#tgt_report.target_wrap_velocity], EM#exist_mask.target_wrap_velocity),
    sutils:conditional_display("Target SNR: ~p~n", [TR#tgt_report.target_snr], EM#exist_mask.target_snr),
    sutils:conditional_display("Target classification: ~p~n", [TR#tgt_report.target_classification], EM#exist_mask.target_classification),
    sutils:conditional_display("Target classification probability: ~p~n", [TR#tgt_report.target_class_prob], EM#exist_mask.target_class_prob),
    sutils:conditional_display("Target slant range unc.: ~p~n", [TR#tgt_report.target_slant_range_unc], EM#exist_mask.target_slant_range_unc),
    sutils:conditional_display("Target cross range unc.: ~p~n", [TR#tgt_report.target_cross_range_unc], EM#exist_mask.target_cross_range_unc),
    sutils:conditional_display("Target height unc.: ~p~n", [TR#tgt_report.target_height_unc], EM#exist_mask.target_height_unc),
    sutils:conditional_display("Target rad. vel. unc.: ~p~n", [TR#tgt_report.target_rad_vel_unc], EM#exist_mask.target_rad_vel_unc),
    sutils:conditional_display("Truth tag application: ~p~n", [TR#tgt_report.truth_tag_app], EM#exist_mask.truth_tag_app),
    sutils:conditional_display("Truth tag entity: ~p~n", [TR#tgt_report.truth_tag_entity], EM#exist_mask.truth_tag_entity),
    sutils:conditional_display("Target RCS: ~p~n", [TR#tgt_report.target_rcs], EM#exist_mask.target_rcs).


