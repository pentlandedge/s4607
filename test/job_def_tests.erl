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

-module(job_def_tests).

-export([sample_job_def/0]).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the decoding of the mission segment.
job_def_test_() ->
    [job1_checks(), job_def_encode_decode(),job_def_new_default(),
     sensor_id_decode_checks(), sensor_id_encode_checks(),
     radar_mode_decode_checks(), radar_mode_encode_checks(),
     terr_elev_decode_checks(), terr_elev_encode_checks(),
     geoid_decode_checks(), geoid_encode_checks(),
     target_filtering_decode_checks(), target_filtering_encode_checks(),
     end_of_job_priority_checks(), radar_mode_decode_reserved_checks(),
     cross_range_std_dev_checks()].

job1_checks() ->
    {ok, JD1} = job_def:decode(job_def1()),
    [?_assertEqual(16909060, job_def:get_job_id(JD1)),
     ?_assertEqual(global_hawk_sensor, job_def:get_sensor_id_type(JD1)),
     ?_assertEqual("Model1", job_def:get_sensor_id_model(JD1)),
     ?_assertEqual(no_filtering, job_def:get_target_filt_flag(JD1)),
     ?_assertEqual(23, job_def:get_priority(JD1)),
     ?_assertEqual(flat_earth, job_def:get_geoid_model(JD1))].

job_def_encode_decode() ->
    JD = sample_job_def(),
    job_def:display(JD),
    EJD = job_def:encode(JD),
    {ok, DEJD} = job_def:decode(EJD),
    Delta = 0.00001,
    [?_assertEqual(100, job_def:get_job_id(DEJD)),
     ?_assertEqual(rotary_wing_radar, job_def:get_sensor_id_type(DEJD)),
     ?_assertEqual("Heli 1", job_def:get_sensor_id_model(DEJD)),
     ?_assertEqual(no_filtering, job_def:get_target_filt_flag(DEJD)),
     ?_assertEqual(30, job_def:get_priority(DEJD)),
     ?_assert(almost_equal(33.3, job_def:get_bounding_a_lat(DEJD), Delta)),
     ?_assert(almost_equal(3.45, job_def:get_bounding_a_lon(DEJD), Delta)),
     ?_assert(almost_equal(23.4, job_def:get_bounding_b_lat(DEJD), Delta)),
     ?_assert(almost_equal(350.0, job_def:get_bounding_b_lon(DEJD), Delta)),
     ?_assert(almost_equal(-45.0, job_def:get_bounding_c_lat(DEJD), Delta)),
     ?_assert(almost_equal(2.45, job_def:get_bounding_c_lon(DEJD), Delta)),
     ?_assert(almost_equal(-60.0, job_def:get_bounding_d_lat(DEJD), Delta)),
     ?_assert(almost_equal(140.0, job_def:get_bounding_d_lon(DEJD), Delta)),
     ?_assertEqual({monopulse_calibration, asars_aip}, job_def:get_radar_mode(DEJD)),
     ?_assertEqual(65000, job_def:get_nom_rev_int(DEJD)),
     ?_assertEqual(no_statement, job_def:get_ns_pos_unc_along_track(DEJD)),
     ?_assertEqual(5000, job_def:get_ns_pos_unc_cross_track(DEJD)),
     ?_assertEqual(20000, job_def:get_ns_pos_unc_alt(DEJD)),
     ?_assertEqual(45, job_def:get_ns_pos_unc_heading(DEJD)),
     ?_assertEqual(65534, job_def:get_ns_pos_unc_sensor_speed(DEJD)),
     ?_assertEqual(100, job_def:get_ns_val_slant_range_std_dev(DEJD)),
     ?_assertEqual(no_statement, job_def:get_ns_val_cross_range_std_dev(DEJD)),
     ?_assertEqual(4000, job_def:get_ns_val_tgt_vel_los_std_dev(DEJD)),
     ?_assertEqual(no_statement, job_def:get_ns_val_mdv(DEJD)),
     ?_assertEqual(100, job_def:get_ns_val_det_prob(DEJD)),
     ?_assertEqual(254, job_def:get_ns_val_false_alarm_density(DEJD)),
     ?_assertEqual(dgm50, job_def:get_terr_elev_model(DEJD)),
     ?_assertEqual(geo96, job_def:get_geoid_model(DEJD))].

job_def_new_default() ->
    JD = job_def:new([]),
    [?_assertEqual(1, job_def:get_job_id(JD)),
     ?_assertEqual(no_statement, job_def:get_sensor_id_type(JD)),
     ?_assertEqual("", job_def:get_sensor_id_model(JD))].

sensor_id_decode_checks() ->
    IdList = sensor_id_table(),
    F = fun({K, V}) ->
            ?_assertEqual(V, job_def:decode_sensor_id_type(K))
        end,
    lists:map(F, IdList).

sensor_id_encode_checks() ->
    IdList = sensor_id_table(),
    F = fun({K, V}) ->
            ?_assertEqual(<<K>>, job_def:encode_sensor_id_type(V))
        end,
    lists:map(F, IdList).

radar_mode_decode_checks() ->
    ModeList = radar_mode_table(),
    F = fun({K, V}) ->
            ?_assertEqual(V, job_def:decode_radar_mode(K))
        end,
    lists:map(F, ModeList).

radar_mode_encode_checks() ->
    ModeList = radar_mode_table(),
    F = fun({K, V}) ->
            ?_assertEqual(<<K>>, job_def:encode_radar_mode(V))
        end,
    lists:map(F, ModeList).

terr_elev_decode_checks() ->
    F = fun job_def:decode_terrain_elev_model/1,
    [?_assertEqual(none_specified, F(0)),
     ?_assertEqual(dted0, F(1)),
     ?_assertEqual(dted1, F(2)),
     ?_assertEqual(dted2, F(3)),
     ?_assertEqual(dted3, F(4)),
     ?_assertEqual(dted4, F(5)),
     ?_assertEqual(dted5, F(6)),
     ?_assertEqual(srtm1, F(7)),
     ?_assertEqual(srtm2, F(8)),
     ?_assertEqual(dgm50, F(9)),
     ?_assertEqual(dgm250, F(10)),
     ?_assertEqual(ithd, F(11)),
     ?_assertEqual(sthd, F(12)),
     ?_assertEqual(sedris, F(13)),
     ?_assertEqual(reserved, F(14))].

terr_elev_encode_checks() ->
    F = fun job_def:encode_terrain_elev_model/1,
    [?_assertEqual(<<0>>, F(none_specified)),
     ?_assertEqual(<<1>>, F(dted0)),
     ?_assertEqual(<<2>>, F(dted1)),
     ?_assertEqual(<<3>>, F(dted2)),
     ?_assertEqual(<<4>>, F(dted3)),
     ?_assertEqual(<<5>>, F(dted4)),
     ?_assertEqual(<<6>>, F(dted5)),
     ?_assertEqual(<<7>>, F(srtm1)),
     ?_assertEqual(<<8>>, F(srtm2)),
     ?_assertEqual(<<9>>, F(dgm50)),
     ?_assertEqual(<<10>>, F(dgm250)),
     ?_assertEqual(<<11>>, F(ithd)),
     ?_assertEqual(<<12>>, F(sthd)),
     ?_assertEqual(<<13>>, F(sedris))].

geoid_decode_checks() ->
    F = fun job_def:decode_geoid_model/1,
    [?_assertEqual(none_specified, F(0)),
     ?_assertEqual(egm96, F(1)),
     ?_assertEqual(geo96, F(2)),
     ?_assertEqual(flat_earth, F(3)),
     ?_assertEqual(reserved, F(4))].

geoid_encode_checks() ->
    F = fun job_def:encode_geoid_model/1,
    [?_assertEqual(<<0>>, F(none_specified)),
     ?_assertEqual(<<1>>, F(egm96)),
     ?_assertEqual(<<2>>, F(geo96)),
     ?_assertEqual(<<3>>, F(flat_earth))].

target_filtering_decode_checks() ->
    F = fun job_def:decode_target_filtering_flag/1,
    [?_assertEqual([area_filtering_intersection_dwell_bounding], F(<<1>>)),
     ?_assertEqual([area_blanking_unspecified_area], F(<<2>>)),
     ?_assertEqual([sector_blanking_unspecified_area], F(<<4>>)),
     ?_assert(lists:member(area_filtering_intersection_dwell_bounding, F(<<7>>))),
     ?_assert(lists:member(area_blanking_unspecified_area, F(<<7>>))),
     ?_assert(lists:member(sector_blanking_unspecified_area, F(<<7>>)))].

target_filtering_encode_checks() ->
    F = fun job_def:encode_target_filtering_flag/1,
    All = [area_filtering_intersection_dwell_bounding,
           area_blanking_unspecified_area,
           sector_blanking_unspecified_area],

    [?_assertEqual(<<0>>, F(no_filtering)),
     ?_assertEqual(<<1>>, F([area_filtering_intersection_dwell_bounding])),
     ?_assertEqual(<<2>>, F([area_blanking_unspecified_area])),
     ?_assertEqual(<<4>>, F([sector_blanking_unspecified_area])),
     ?_assertEqual(<<5>>, F([sector_blanking_unspecified_area,
                             area_filtering_intersection_dwell_bounding])),
     ?_assertEqual(<<7>>, F(All))].

end_of_job_priority_checks() ->
    % Start with the sample job definition parameters then patch the ones we
    % are interested in.
    P1 = sample_job_def_params(),
    
    % Replace the priority field with an end of job indication.
    P2 = proplists:delete(priority, P1),
    P3 = [{priority, end_of_job}|P2],
    
    % Create a job definition segment.
    JD = job_def:new(P3),

    % Encode it, then decode it again.
    EJD = job_def:encode(JD),
    {ok, DEJD} = job_def:decode(EJD),
    
    % Check that the decoded priority field is correct.
    [?_assertEqual(end_of_job, job_def:get_priority(DEJD))].

radar_mode_decode_reserved_checks() ->
    Val = {available_for_future_use, reserved},
    [?_assertEqual(Val, job_def:decode_radar_mode(119))].

cross_range_std_dev_checks() ->
    % Start with the sample job definition parameters then patch the ones we
    % are interested in.
    P1 = sample_job_def_params(),
    
    % Replace the cross range standard dev parameter. 
    P2 = proplists:delete(ns_val_cross_range_std_dev, P1),
    P3 = [{ns_val_cross_range_std_dev, 179.0}|P2],
    
    % Create a job definition segment.
    JD = job_def:new(P3),

    % Encode it, then decode it again.
    EJD = job_def:encode(JD),
    {ok, DEJD} = job_def:decode(EJD),
    
    % Check that the decoded priority field is correct.
    Delta = 0.001,
    DecodedStdDev = job_def:get_ns_val_cross_range_std_dev(DEJD),
    [?_assert(almost_equal(179.0, DecodedStdDev, Delta))].

job_def1() ->
    <<1,2,3,4, 5, "Model1", 0, 23,
      64,0,0,0, 245,85,85,85, 64,0,0,0, 245,85,85,85, 64,0,0,0,
      245,85,85,85, 64,0,0,0, 245,85,85,85, 1, 1,0, 255,255, 1,0,
      16#27,16#10, 45, 0,128, 255,255, 127,74, 0,100, 5, 90, 3, 1, 3>>.

sample_job_def() ->
    job_def:new(sample_job_def_params()).

sample_job_def_params() ->
    [{job_id, 100}, {sensor_id_type, rotary_wing_radar},
     {sensor_id_model, "Heli 1"}, {target_filt_flag, no_filtering}, {priority, 30},
     {bounding_a_lat, 33.3}, {bounding_a_lon, 3.45},
     {bounding_b_lat, 23.4}, {bounding_b_lon, 350},
     {bounding_c_lat, -45.0}, {bounding_c_lon, 2.45},
     {bounding_d_lat, -60.0}, {bounding_d_lon, 140},
     {radar_mode, {monopulse_calibration, asars_aip}}, {nom_rev_int, 65000},
     {ns_pos_unc_along_track, no_statement},
     {ns_pos_unc_cross_track, 5000}, {ns_pos_unc_alt, 20000},
     {ns_pos_unc_heading, 45}, {ns_pos_unc_sensor_speed, 65534},
     {ns_val_slant_range_std_dev, 100},
     {ns_val_cross_range_std_dev, no_statement},
     {ns_val_tgt_vel_los_std_dev, 4000}, {ns_val_mdv, no_statement},
     {ns_val_det_prob, 100}, {ns_val_false_alarm_density, 254},
     {terr_elev_model, dgm50}, {geoid_model, geo96}].

%% Function to return a proplist with the mapping from sensor ID to the
%% sensor type.
sensor_id_table() ->
    [{0, unidentified},
     {1, other},
     {2, hisar},
     {3, astor},
     {4, rotary_wing_radar},
     {5, global_hawk_sensor},
     {6, horizon},
     {7, apy_3},
     {8, apy_6},
     {9, apy_8},
     {10, radarsat2},
     {11, asars_2a},
     {12, tesar},
     {13, mp_rtip},
     {14, apg_77},
     {15, apg_79},
     {16, apg_81},
     {17, apg_6v1},
     {18, dpy_1},
     {19, sidm},
     {20, limit},
     {21, tcar},
     {22, lsrs},
     {23, ugs_single_sensor},
     {24, ugs_cluster_sensor},
     {25, imaster_gmti},
     {26, anzpy_1},
     {27, vader},
     {255, no_statement}].

%% Function to return a proplist with the radar mode mapping.
radar_mode_table() ->
    [{0, {unspecified_mode, generic}},
     {1, {mti, generic}},
     {2, {hrr, generic}},
     {3, {uhrr, generic}},
     {4, {hur, generic}},
     {5, {fti, generic}},
     {11, {attack_control_satc, joint_stars}},
     {12, {attack_control, joint_stars}},
     {13, {satc, joint_stars}},
     {14, {attack_planning_satc, joint_stars}},
     {15, {attack_planning, joint_stars}},
     {16, {med_res_sector_search, joint_stars}},
     {17, {low_res_sector_search, joint_stars}},
     {18, {wide_area_search_grca, joint_stars}},
     {19, {wide_area_search_rrca, joint_stars}},
     {20, {attack_plannning_with_tracking, joint_stars}},
     {21, {attack_control_with_tracking, joint_stars}},
     {31, {wide_area_mti, asars_aip}},
     {32, {coarse_res_search, asars_aip}},
     {33, {med_res_search, asars_aip}},
     {34, {high_res_search, asars_aip}},
     {35, {point_imaging, asars_aip}},
     {36, {swath_mti, asars_aip}},
     {37, {repetititve_point_imaging, asars_aip}},
     {38, {monopulse_calibration, asars_aip}},
     {51, {search, asars_2}},
     {52, {emti_wide_frame_search, asars_2}},
     {53, {emti_narrow_frame_search, asars_2}},
     {54, {emti_augmented_spot, asars_2}},
     {55, {emti_wide_area_mti, asars_2}},
     {61, {gmti_ppi_mode, tuav}},
     {62, {gmti_expanded_mode, tuav}},
     {63, {narrow_sector_search, arl_m}},
     {64, {single_beam_scan, arl_m}},
     {65, {wide_area, arl_m}},
     {81, {grca, reserved}},
     {82, {rrca, reserved}},
     {83, {sector_search, reserved}},
     {84, {horizon_basic, horizon}},
     {85, {horizon_high_sensitivity, horizon}},
     {86, {horizon_burn_through, horizon}},
     {87, {creso_acquisition, creso}},
     {88, {creso_count, creso}},
     {94, {was_mti_exo, astor}},
     {95, {was_mti_endo_exo, astor}},
     {96, {ss_mti_exo, astor}},
     {97, {ss_mti_endo_exo, astor}},
     {100, {test_status_mode, reserved}},
     {101, {mti_spot_scan, lynx_i_ii}},
     {102, {mti_arc_scan, lynx_i_ii}},
     {103, {hrr_mti_spot_scan, lynx_i_ii}},
     {104, {hrr_mti_arc_scan, lynx_i_ii}},
     {111, {grca, global_hawk}},
     {112, {rrca, global_hawk}},
     {113, {gmti_hrr, global_hawk}},
     {120, {small_area_gmti, vader}},
     {121, {wide_area_gmti, vader}},
     {122, {dismount_gmti, vader}},
     {123, {hrr_gmti, vader}}].

%% Utility function to compare whether floating point values are within a
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.

