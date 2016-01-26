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
-module(job_def).

-export([
    decode/1, 
    display/1,
    get_job_id/1
    ]).

-record(job_def, {
    job_id,
    sensor_id_type,
    sensor_id_model,
    target_filt_flag,
    priority,
    bounding_a_lat,
    bounding_a_lon,
    bounding_b_lat,
    bounding_b_lon,
    bounding_c_lat,
    bounding_c_lon,
    bounding_d_lat,
    bounding_d_lon,
    radar_mode,
    nom_rev_int,
    ns_pos_unc_along_track,
    ns_pos_unc_cross_track,
    ns_pos_unc_alt,
    ns_pos_unc_heading,
    ns_pos_unc_sensor_speed,
    ns_val_slant_range_std_dev,
    ns_val_cross_range_std_dev,
    ns_val_tgt_vel_los_std_dev,
    ns_val_mdv,
    ns_val_det_prob,
    ns_val_false_alarm_density,
    terr_elev_model,
    geoid_model}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Job definition segment decoding functions.

decode(<<JobID:32,SIDT,SIDM:6/binary,TFF:1/binary,Pri,
    J6:4/binary,J7:4/binary,J8:4/binary,J9:4/binary,J10:4/binary,J11:4/binary,
    J12:4/binary,J13:4/binary,J14,NRI:16,J16:16,J17:16,J18:16,J19,J20:16,
    J21:16,J22:2/binary,J23:16,J24,J25,J26,J27,J28>>) ->

    #job_def{
        job_id = JobID,
        sensor_id_type = decode_sensor_id_type(SIDT),
        sensor_id_model = decode_sensor_id_model(SIDM),
        target_filt_flag = decode_target_filtering_flag(TFF),
        priority = Pri,
        bounding_a_lat = stanag_types:sa32_to_float(J6),
        bounding_a_lon = stanag_types:ba32_to_float(J7),
        bounding_b_lat = stanag_types:sa32_to_float(J8),
        bounding_b_lon = stanag_types:ba32_to_float(J9),
        bounding_c_lat = stanag_types:sa32_to_float(J10),
        bounding_c_lon = stanag_types:ba32_to_float(J11),
        bounding_d_lat = stanag_types:sa32_to_float(J12),
        bounding_d_lon = stanag_types:ba32_to_float(J13),
        radar_mode = decode_radar_mode(J14),
        nom_rev_int = NRI,
        ns_pos_unc_along_track = decode_range_ns(J16, 0, 10000, 65535),
        ns_pos_unc_cross_track = decode_range_ns(J17, 0, 10000, 65535),
        ns_pos_unc_alt = decode_range_ns(J18, 0, 20000, 65535),
        ns_pos_unc_heading = decode_range_ns(J19, 0, 45, 255),
        ns_pos_unc_sensor_speed  = decode_range_ns(J20, 0, 65534, 65535),
        ns_val_slant_range_std_dev = decode_range_ns(J21, 0, 65534, 65535),
        ns_val_cross_range_std_dev = decode_cross_range_std_dev(J22),
        ns_val_tgt_vel_los_std_dev = decode_range_ns(J23, 0, 5000, 65535),
        ns_val_mdv = decode_range_ns(J24, 0, 254, 255),
        ns_val_det_prob = decode_range_ns(J25, 0, 100, 255),
        ns_val_false_alarm_density = decode_range_ns(J26, 0, 254, 255),
        terr_elev_model = decode_terrain_elev_model(J27),
        geoid_model = decode_geoid_model(J28)}.

decode_sensor_id_type(0) -> unidentified;
decode_sensor_id_type(1) -> other;
decode_sensor_id_type(2) -> hisar;
decode_sensor_id_type(3) -> astor;
decode_sensor_id_type(4) -> rotary_wing_radar;
decode_sensor_id_type(5) -> global_hawk_sensor;
decode_sensor_id_type(6) -> horizon;
decode_sensor_id_type(7) -> apy_3;
decode_sensor_id_type(8) -> apy_6;
decode_sensor_id_type(9) -> apy_8;
decode_sensor_id_type(10) -> radarsat2;
decode_sensor_id_type(11) -> asars_2a;
decode_sensor_id_type(12) -> tesar;
decode_sensor_id_type(13) -> mp_rtip;
decode_sensor_id_type(14) -> apg_77;
decode_sensor_id_type(15) -> apg_79;
decode_sensor_id_type(16) -> apg_81;
decode_sensor_id_type(17) -> apg_6v1;
decode_sensor_id_type(18) -> dpy_1;
decode_sensor_id_type(19) -> sidm;
decode_sensor_id_type(20) -> limit;
decode_sensor_id_type(21) -> tcar;
decode_sensor_id_type(22) -> lsrs;
decode_sensor_id_type(23) -> ugs_single_sensor;
decode_sensor_id_type(24) -> ugs_cluster_sensor;
decode_sensor_id_type(25) -> imaster_gmti;
decode_sensor_id_type(26) -> anzpy_1;
decode_sensor_id_type(27) -> vader;
decode_sensor_id_type(255) -> no_statement;
decode_sensor_id_type(_) -> available_future_use.

decode_sensor_id_model(Bin) ->
    sutils:trim_trailing_spaces(binary_to_list(Bin)).

%% Function to decode the bits in the target filtering flag.
decode_target_filtering_flag(<<0>>) -> 
    no_filtering;
decode_target_filtering_flag(<<0:5,B2:1,B1:1,B0:1>>) -> 
    L1 = case B0 of
            1 -> [area_filtering_intersection_dwell_bounding];
            0 -> []
         end,
    L2 = case B1 of
            1 -> [area_blanking_unspecified_area|L1];
            0 -> L1 
         end,
    L3 = case B2 of
            1 -> [sector_blanking_unspecified_area|L2];
            0 -> L2 
         end,
    L3.

%% Function to decode the various radar modes for different systems.
decode_radar_mode(0) -> {unspecified_mode, generic};
decode_radar_mode(1) -> {mti, generic};
decode_radar_mode(2) -> {hrr, generic};
decode_radar_mode(3) -> {uhrr, generic};
decode_radar_mode(4) -> {hur, generic};
decode_radar_mode(5) -> {fti, generic};
decode_radar_mode(11) -> {attack_control_satc, joint_stars};
decode_radar_mode(12) -> {attack_control, joint_stars};
decode_radar_mode(13) -> {satc, joint_stars};
decode_radar_mode(14) -> {attack_planning_satc, joint_stars};
decode_radar_mode(15) -> {attack_planning, joint_stars};
decode_radar_mode(16) -> {med_res_sector_search, joint_stars};
decode_radar_mode(17) -> {low_res_sector_search, joint_stars};
decode_radar_mode(18) -> {wide_area_search_grca, joint_stars};
decode_radar_mode(19) -> {wide_area_search_rrca, joint_stars};
decode_radar_mode(20) -> {attack_plannning_with_tracking, joint_stars};
decode_radar_mode(21) -> {attack_control_with_tracking, joint_stars};
decode_radar_mode(31) -> {wide_area_mti, asars_aip};
decode_radar_mode(32) -> {coarse_res_search, asars_aip};
decode_radar_mode(33) -> {med_res_search, asars_aip};
decode_radar_mode(34) -> {high_res_search, asars_aip};
decode_radar_mode(35) -> {point_imaging, asars_aip};
decode_radar_mode(36) -> {swath_mti, asars_aip};
decode_radar_mode(37) -> {repetititve_point_imaging, asars_aip};
decode_radar_mode(38) -> {monopulse_calibration, asars_aip};
decode_radar_mode(51) -> {search, asars_2};
decode_radar_mode(52) -> {emti_wide_frame_search, asars_2};
decode_radar_mode(53) -> {emti_narrow_frame_search, asars_2};
decode_radar_mode(54) -> {emti_augmented_spot, asars_2};
decode_radar_mode(55) -> {emti_wide_area_mti, asars_2};
decode_radar_mode(61) -> {gmti_ppi_mode, tuav};
decode_radar_mode(62) -> {gmti_expanded_mode, tuav};
decode_radar_mode(63) -> {narrow_sector_search, arl_m};
decode_radar_mode(64) -> {single_beam_scan, arl_m};
decode_radar_mode(65) -> {wide_area, arl_m};
decode_radar_mode(81) -> {grca, reserved};
decode_radar_mode(82) -> {rrca, reserved};
decode_radar_mode(83) -> {sector_search, reserved};
decode_radar_mode(84) -> {horizon_basic, horizon};
decode_radar_mode(85) -> {horizon_high_sensitivity, horizon};
decode_radar_mode(86) -> {horizon_burn_through, horizon};
decode_radar_mode(87) -> {creso_acquisition, creso};
decode_radar_mode(88) -> {creso_count, creso};
decode_radar_mode(94) -> {was_mti_exo, astor};
decode_radar_mode(95) -> {was_mti_endo_exo, astor};
decode_radar_mode(96) -> {ss_mti_exo, astor};
decode_radar_mode(97) -> {ss_mti_endo_exo, astor};
decode_radar_mode(100) -> {test_status_mode, reserved};
decode_radar_mode(101) -> {mti_spot_scan, lynx_i_ii};
decode_radar_mode(102) -> {mti_arc_scan, lynx_i_ii};
decode_radar_mode(103) -> {hrr_mti_spot_scan, lynx_i_ii};
decode_radar_mode(104) -> {hrr_mti_arc_scan, lynx_i_ii};
decode_radar_mode(111) -> {grca, global_hawk};
decode_radar_mode(112) -> {rrca, global_hawk};
decode_radar_mode(113) -> {gmti_hrr, global_hawk};
decode_radar_mode(120) -> {small_area_gmti, vader};
decode_radar_mode(121) -> {wide_area_gmti, vader};
decode_radar_mode(122) -> {dismount_gmti, vader};
decode_radar_mode(123) -> {hrr_gmti, vader};
decode_radar_mode(_) -> {available_for_future_use, reserved}.

%% Function to decode the cross-range standard deviation parameter.
decode_cross_range_std_dev(X) ->
    Val = stanag_types:ba16_to_float(X),
    limit_cross_range_std_dev(Val).

limit_cross_range_std_dev(X) when X >= 180.0 -> no_statement;
limit_cross_range_std_dev(X) when X >= 0.0 -> X.

%% Generic range decode with lower, upper and no statement values.
decode_range_ns(X, _, _, NS) when X =:= NS -> no_statement;
decode_range_ns(X, L, U, _) when X >= L, X =< U -> X.

%% Decode the terrain elevation model parameter.
decode_terrain_elev_model(0) -> none_specified;
decode_terrain_elev_model(1) -> dted0;
decode_terrain_elev_model(2) -> dted1;
decode_terrain_elev_model(3) -> dted2;
decode_terrain_elev_model(4) -> dted3;
decode_terrain_elev_model(5) -> dted4;
decode_terrain_elev_model(6) -> dted5;
decode_terrain_elev_model(7) -> srtm1;
decode_terrain_elev_model(8) -> srtm2;
decode_terrain_elev_model(9) -> dgm50;
decode_terrain_elev_model(10) -> dgm250;
decode_terrain_elev_model(11) -> ithd;
decode_terrain_elev_model(12) -> sthd;
decode_terrain_elev_model(13) -> sedris;
decode_terrain_elev_model(_) -> reserved.

%% Decode the Geoid model parameter.
decode_geoid_model(0) -> none_specified;
decode_geoid_model(1) -> egm96;
decode_geoid_model(2) -> geo96;
decode_geoid_model(3) -> flat_earth;
decode_geoid_model(_) -> reserved.

display(JDS) ->
    io:format("Job ID: ~p~n", [JDS#job_def.job_id]), 
    io:format("Sensor ID type: ~p~n", [JDS#job_def.sensor_id_type]), 
    io:format("Sensor ID model: ~p~n", [JDS#job_def.sensor_id_model]), 
    io:format("Target filtering flag: ~p~n", [JDS#job_def.target_filt_flag]), 
    io:format("Priority: ~p~n", [JDS#job_def.priority]), 
    io:format("Bounding A Lat: ~p~n", [JDS#job_def.bounding_a_lat]), 
    io:format("Bounding A Lon: ~p~n", [JDS#job_def.bounding_a_lon]), 
    io:format("Bounding B Lat: ~p~n", [JDS#job_def.bounding_b_lat]), 
    io:format("Bounding B Lon: ~p~n", [JDS#job_def.bounding_b_lon]), 
    io:format("Bounding C Lat: ~p~n", [JDS#job_def.bounding_c_lat]), 
    io:format("Bounding C Lon: ~p~n", [JDS#job_def.bounding_c_lon]), 
    io:format("Bounding D Lat: ~p~n", [JDS#job_def.bounding_d_lat]), 
    io:format("Bounding D Lon: ~p~n", [JDS#job_def.bounding_d_lon]), 
    io:format("Radar mode: ~p~n", [JDS#job_def.radar_mode]), 
    io:format("Nom. revist interval: ~p~n", [JDS#job_def.nom_rev_int]), 
    io:format("Nom. pos. unc. along track: ~p~n", [JDS#job_def.ns_pos_unc_along_track]), 
    io:format("Nom. pos. unc. cross track: ~p~n", [JDS#job_def.ns_pos_unc_cross_track]), 
    io:format("Nom. pos. unc. altitude: ~p~n", [JDS#job_def.ns_pos_unc_alt]), 
    io:format("Nom. pos. unc. track heading: ~p~n", [JDS#job_def.ns_pos_unc_heading]), 
    io:format("Nom. pos. unc. sensor speed: ~p~n", [JDS#job_def.ns_pos_unc_sensor_speed]), 
    io:format("Nom. slant range std. dev.: ~p~n", [JDS#job_def.ns_val_slant_range_std_dev]),
    io:format("Nom. cross range std. dev.: ~p~n", [JDS#job_def.ns_val_cross_range_std_dev]),
    io:format("Nom. target vel. LOS std. dev.: ~p~n", [JDS#job_def.ns_val_tgt_vel_los_std_dev]),
    io:format("Nom. MDV: ~p~n", [JDS#job_def.ns_val_mdv]),
    io:format("Nom. detection prob.: ~p~n", [JDS#job_def.ns_val_det_prob]),
    io:format("Nom. false alarm density.: ~p~n", [JDS#job_def.ns_val_false_alarm_density]),
    io:format("Terrain elevation model: ~p~n", [JDS#job_def.terr_elev_model]),
    io:format("Geoid model: ~p~n", [JDS#job_def.geoid_model]).

%% Accessor functions to allow clients access to the contents
get_job_id(#job_def{job_id = X}) -> X.
