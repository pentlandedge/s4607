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
    encode/1,
    new/1,
    payload_size/0,
    display/1,
    get_job_id/1,
    get_sensor_id_type/1,
    get_sensor_id_model/1,
    get_target_filt_flag/1,
    get_priority/1,
    get_bounding_a_lat/1,
    get_bounding_a_lon/1,
    get_bounding_b_lat/1,
    get_bounding_b_lon/1,
    get_bounding_c_lat/1,
    get_bounding_c_lon/1,
    get_bounding_d_lat/1,
    get_bounding_d_lon/1,
    get_radar_mode/1,
    get_nom_rev_int/1,
    get_ns_pos_unc_along_track/1,
    get_ns_pos_unc_cross_track/1,
    get_ns_pos_unc_alt/1,
    get_ns_pos_unc_heading/1,
    get_ns_pos_unc_sensor_speed/1,
    get_ns_val_slant_range_std_dev/1,
    get_ns_val_cross_range_std_dev/1,
    get_ns_val_tgt_vel_los_std_dev/1,
    get_ns_val_mdv/1,
    get_ns_val_det_prob/1,
    get_ns_val_false_alarm_density/1,
    get_terr_elev_model/1,
    get_geoid_model/1]).

%% Export of functions for unit testing.
-ifdef(TEST).
-export([
    decode_sensor_id_type/1,
    encode_sensor_id_type/1,
    decode_radar_mode/1,
    encode_radar_mode/1,
    decode_terrain_elev_model/1,
    encode_terrain_elev_model/1]).
-endif.

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

    {ok, #job_def{
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
        geoid_model = decode_geoid_model(J28)}}.

%% Function to produce a binary encoded version of a job definition segment.
encode(JD) ->
    % Function to encode each parameter in the list and append to an 
    % accumulated binary.
    F = fun({GetFun, EncFun}, Acc) ->
            P = GetFun(JD),
            Bin = EncFun(P),
            <<Acc/binary,Bin/binary>>
        end,

    % Shorthand for the type conversion functions.
    I8Fun = fun stanag_types:integer_to_i8/1,
    I16Fun = fun stanag_types:integer_to_i16/1,
    
    % Generate the encoding functions for the range/no statement encodes.
    J16Fun = gen_encode_range_ns(0, 10000, 65535, I16Fun),
    J17Fun = J16Fun,
    J18Fun = gen_encode_range_ns(0, 20000, 65535, I16Fun),
    J19Fun = gen_encode_range_ns(0, 45, 255, I8Fun),
    J20Fun = gen_encode_range_ns(0, 65534, 65535, I16Fun),
    J21Fun = J20Fun,
    
    J23Fun = gen_encode_range_ns(0, 5000, 65535, I16Fun),
    J24Fun = gen_encode_range_ns(0, 254, 255, I8Fun),
    J25Fun = gen_encode_range_ns(0, 100, 255, I8Fun),
    J26Fun = gen_encode_range_ns(0, 254, 255, I8Fun),

    % List of parameters in the how to fetch/encode.
    ParamList = 
        [{fun get_job_id/1, fun stanag_types:integer_to_i32/1},
         {fun get_sensor_id_type/1, fun encode_sensor_id_type/1},
         {fun get_sensor_id_model/1, fun encode_sensor_id_model/1},
         {fun get_target_filt_flag/1, fun encode_target_filtering_flag/1},
         {fun get_priority/1, fun encode_priority/1},
         {fun get_bounding_a_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_a_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_bounding_b_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_b_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_bounding_c_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_c_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_bounding_d_lat/1, fun stanag_types:float_to_sa32/1},
         {fun get_bounding_d_lon/1, fun stanag_types:float_to_ba32/1},
         {fun get_radar_mode/1, fun encode_radar_mode/1},
         {fun get_nom_rev_int/1, fun stanag_types:integer_to_i16/1},
         {fun get_ns_pos_unc_along_track/1, J16Fun},
         {fun get_ns_pos_unc_cross_track/1, J17Fun},
         {fun get_ns_pos_unc_alt/1, J18Fun},
         {fun get_ns_pos_unc_heading/1, J19Fun},
         {fun get_ns_pos_unc_sensor_speed/1, J20Fun},
         {fun get_ns_val_slant_range_std_dev/1, J21Fun},
         {fun get_ns_val_cross_range_std_dev/1, fun encode_cross_range_std_dev/1}, 
         {fun get_ns_val_tgt_vel_los_std_dev/1, J23Fun},
         {fun get_ns_val_mdv/1, J24Fun},
         {fun get_ns_val_det_prob/1, J25Fun},
         {fun get_ns_val_false_alarm_density/1, J26Fun},
         {fun get_terr_elev_model/1, fun encode_terrain_elev_model/1},
         {fun get_geoid_model/1, fun encode_geoid_model/1}
        ],
    
    lists:foldl(F, <<>>, ParamList).

%% Function to create a new job definition segment from a supplied list of 
%% {parameter, Value} tuples.
new(ParamList) ->
    % Local function to pull the parameter from the list or use a default
    % value.
    F = fun(P, L, D) ->
            case lists:keyfind(P, 1, L) of
                {P, V} -> V;
                false  -> D 
            end
        end,

    #job_def{
        job_id = F(job_id, ParamList, 1),
        sensor_id_type = F(sensor_id_type, ParamList, no_statement),
        sensor_id_model = F(sensor_id_model, ParamList, ""),
        target_filt_flag = F(target_filt_flag, ParamList, no_filtering),
        priority = F(priority, ParamList, 99),
        bounding_a_lat = F(bounding_a_lat, ParamList, 0.0),
        bounding_a_lon = F(bounding_a_lon, ParamList, 0.0),
        bounding_b_lat = F(bounding_b_lat, ParamList, 0.0),
        bounding_b_lon = F(bounding_b_lon, ParamList, 0.0),
        bounding_c_lat = F(bounding_c_lat, ParamList, 0.0),
        bounding_c_lon = F(bounding_c_lon, ParamList, 0.0),
        bounding_d_lat = F(bounding_d_lat, ParamList, 0.0),
        bounding_d_lon = F(bounding_d_lon, ParamList, 0.0),
        radar_mode = F(radar_mode, ParamList, {unspecified_mode, generic}),
        nom_rev_int = F(nom_rev_int, ParamList, 0),
        ns_pos_unc_along_track = F(ns_pos_unc_along_track, ParamList, no_statement),
        ns_pos_unc_cross_track = F(ns_pos_unc_cross_track, ParamList, no_statement),
        ns_pos_unc_alt = F(ns_pos_unc_alt, ParamList, no_statement),
        ns_pos_unc_heading = F(ns_pos_unc_heading, ParamList, no_statement),
        ns_pos_unc_sensor_speed = F(ns_pos_unc_sensor_speed, ParamList, no_statement),
        ns_val_slant_range_std_dev = F(ns_val_slant_range_std_dev, ParamList, no_statement),
        ns_val_cross_range_std_dev = F(ns_val_cross_range_std_dev, ParamList, no_statement),
        ns_val_tgt_vel_los_std_dev = F(ns_val_tgt_vel_los_std_dev, ParamList, no_statement),
        ns_val_mdv = F(ns_val_mdv, ParamList, no_statement),
        ns_val_det_prob = F(ns_val_det_prob, ParamList, no_statement),
        ns_val_false_alarm_density = F(ns_val_false_alarm_density, ParamList, no_statement),
        terr_elev_model = F(terr_elev_model, ParamList, none_specified),
        geoid_model = F(geoid_model, ParamList, none_specified)}.

%% Function to return the size of the job definition segment payload.
payload_size() -> 68.

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
decode_sensor_id_type(255) -> no_statement.

%% Function to encode the sensor ID type as a binary.
encode_sensor_id_type(Type) ->
    Val = esid(Type),
    <<Val>>.

esid(unidentified) -> 0;
esid(other) -> 1;
esid(hisar) -> 2;
esid(astor) -> 3;
esid(rotary_wing_radar) -> 4;
esid(global_hawk_sensor) -> 5;
esid(horizon) -> 6;
esid(apy_3) -> 7;
esid(apy_6) -> 8;
esid(apy_8) -> 9;
esid(radarsat2) -> 10;
esid(asars_2a) -> 11;
esid(tesar) -> 12;
esid(mp_rtip) -> 13;
esid(apg_77) -> 14;
esid(apg_79) -> 15;
esid(apg_81) -> 16;
esid(apg_6v1) -> 17;
esid(dpy_1) -> 18;
esid(sidm) -> 19;
esid(limit) -> 20;
esid(tcar) -> 21;
esid(lsrs) -> 22;
esid(ugs_single_sensor) -> 23;
esid(ugs_cluster_sensor) -> 24;
esid(imaster_gmti) -> 25;
esid(anzpy_1) -> 26;
esid(vader) -> 27;
esid(no_statement) -> 255.

decode_sensor_id_model(Bin) ->
    sutils:trim_trailing_spaces(binary_to_list(Bin)).

encode_sensor_id_model(M) ->
    Pad = sutils:add_trailing_spaces(M, 6),
    list_to_binary(Pad).

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

%% Function to encode the target filtering flag
encode_target_filtering_flag(no_filtering) ->
    <<0>>;
encode_target_filtering_flag(FlagList) ->
    AF = lists:member(area_filtering_intersection_dwell_bounding, FlagList),
    AB = lists:member(area_blanking_unspecified_area, FlagList),
    SB = lists:member(sector_blanking_unspecified_area, FlagList),
    
    % Convert from booleans to integers.
    F = fun(Flag) -> 
            case Flag of 
                true -> 1;
                false -> 0
            end
        end,

    B0 = F(AF),
    B1 = F(AB),
    B2 = F(SB),

    % Combine all bits into a binary.
    <<0:5,B2:1,B1:1,B0:1>>.

% Function to encode the radar priority.
encode_priority(end_of_job) -> <<255>>;
encode_priority(X) when X >= 1, X =< 99 -> <<X>>.

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

%% Function to encode the radar mode as a binary.
encode_radar_mode({_System, _Mode} = RM) ->
    Value = erm(RM),
    <<Value>>.

%% Helper function for encoding the radar mode.
erm({unspecified_mode, generic}) -> 0;
erm({mti, generic}) -> 1;
erm({hrr, generic}) -> 2;
erm({uhrr, generic}) -> 3;
erm({hur, generic}) -> 4;
erm({fti, generic}) -> 5;
erm({attack_control_satc, joint_stars}) -> 11;
erm({attack_control, joint_stars}) -> 12;
erm({satc, joint_stars}) -> 13;
erm({attack_planning_satc, joint_stars}) -> 14;
erm({attack_planning, joint_stars}) -> 15;
erm({med_res_sector_search, joint_stars}) -> 16;
erm({low_res_sector_search, joint_stars}) -> 17;
erm({wide_area_search_grca, joint_stars}) -> 18;
erm({wide_area_search_rrca, joint_stars}) -> 19;
erm({attack_plannning_with_tracking, joint_stars}) -> 20;
erm({attack_control_with_tracking, joint_stars}) -> 21;
erm({wide_area_mti, asars_aip}) -> 31;
erm({coarse_res_search, asars_aip}) -> 32;
erm({med_res_search, asars_aip}) -> 33;
erm({high_res_search, asars_aip}) -> 34;
erm({point_imaging, asars_aip}) -> 35;
erm({swath_mti, asars_aip}) -> 36;
erm({repetititve_point_imaging, asars_aip}) -> 37;
erm({monopulse_calibration, asars_aip}) -> 38;
erm({search, asars_2}) -> 51;
erm({emti_wide_frame_search, asars_2}) -> 52;
erm({emti_narrow_frame_search, asars_2}) -> 53;
erm({emti_augmented_spot, asars_2}) -> 54;
erm({emti_wide_area_mti, asars_2}) -> 55;
erm({gmti_ppi_mode, tuav}) -> 61;
erm({gmti_expanded_mode, tuav}) -> 62;
erm({narrow_sector_search, arl_m}) -> 63;
erm({single_beam_scan, arl_m}) -> 64;
erm({wide_area, arl_m}) -> 65;
erm({grca, reserved}) -> 81;
erm({rrca, reserved}) -> 82;
erm({sector_search, reserved}) -> 83;
erm({horizon_basic, horizon}) -> 84;
erm({horizon_high_sensitivity, horizon}) -> 85;
erm({horizon_burn_through, horizon}) -> 86;
erm({creso_acquisition, creso}) -> 87;
erm({creso_count, creso}) -> 88;
erm({was_mti_exo, astor}) -> 94;
erm({was_mti_endo_exo, astor}) -> 95;
erm({ss_mti_exo, astor}) -> 96;
erm({ss_mti_endo_exo, astor}) -> 97;
erm({test_status_mode, reserved}) -> 100;
erm({mti_spot_scan, lynx_i_ii}) -> 101;
erm({mti_arc_scan, lynx_i_ii}) -> 102;
erm({hrr_mti_spot_scan, lynx_i_ii}) -> 103;
erm({hrr_mti_arc_scan, lynx_i_ii}) -> 104;
erm({grca, global_hawk}) -> 111;
erm({rrca, global_hawk}) -> 112;
erm({gmti_hrr, global_hawk}) -> 113;
erm({small_area_gmti, vader}) -> 120;
erm({wide_area_gmti, vader}) -> 121;
erm({dismount_gmti, vader}) -> 122;
erm({hrr_gmti, vader}) -> 123.

%% Function to decode the cross-range standard deviation parameter.
decode_cross_range_std_dev(X) ->
    Val = stanag_types:ba16_to_float(X),
    limit_cross_range_std_dev(Val).

limit_cross_range_std_dev(X) when X >= 180.0 -> no_statement;
limit_cross_range_std_dev(X) when X >= 0.0 -> X.

% Function to encode the cross range std. dev. parameter.
encode_cross_range_std_dev(no_statement) ->
    stanag_types:float_to_ba16(200.0);
encode_cross_range_std_dev(X) ->
    stanag_types:float_to_ba16(X).

%% Generic range decode with lower, upper and no statement values.
decode_range_ns(X, _, _, NS) when X =:= NS -> no_statement;
decode_range_ns(X, L, U, _) when X >= L, X =< U -> X.

%% Generator function: returns a fun to convert the range value, customised
%% to the particular parameter and range specified.
gen_encode_range_ns(L, U, NS, ConvFun) ->
    fun(X) ->
        ConvFun(erns(X, L, U, NS))
    end.

%% Helper function for encoding the range: checks the limits and for the
%% presence of a no_statement atom.
erns(no_statement, _, _, NS) -> NS;
erns(X, L, U, _) when X >= L, X =< U -> X.

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

%% Encode the terrain elevation parameter as a binary.
encode_terrain_elev_model(X) ->
    Val = etev(X),
    <<Val>>.

%% Helper function with the mapping from model -> integer.
etev(none_specified) -> 0;
etev(dted0) -> 1;
etev(dted1) -> 2;
etev(dted2) -> 3;
etev(dted3) -> 4;
etev(dted4) -> 5;
etev(dted5) -> 6;
etev(srtm1) -> 7;
etev(srtm2) -> 8;
etev(dgm50) -> 9;
etev(dgm250) -> 10;
etev(ithd) -> 11;
etev(sthd) -> 12;
etev(sedris) -> 13;
etev(reserved) -> 255.

%% Decode the Geoid model parameter.
decode_geoid_model(0) -> none_specified;
decode_geoid_model(1) -> egm96;
decode_geoid_model(2) -> geo96;
decode_geoid_model(3) -> flat_earth;
decode_geoid_model(_) -> reserved.

%% Function to encode the Geoid model parameter as a binary.
encode_geoid_model(X) ->
    Val = egm(X),
    <<Val>>.

% Helper functioon to map Geoid models to the integer value.
egm(none_specified) -> 0;
egm(egm96) -> 1;
egm(geo96) -> 2;
egm(flat_earth) -> 3;
egm(reserved) -> 255.

display(JDS) ->
    io:format("****************************************~n"),
    io:format("** @job_def~n"),
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
get_geoid_model(#job_def{geoid_model = X}) -> X.
get_sensor_id_type(#job_def{sensor_id_type = X}) -> X.
get_sensor_id_model(#job_def{sensor_id_model = X}) -> X.
get_target_filt_flag(#job_def{target_filt_flag = X}) -> X.
get_priority(#job_def{priority = X}) -> X.
get_bounding_a_lat(#job_def{bounding_a_lat = X}) -> X.
get_bounding_a_lon(#job_def{bounding_a_lon = X}) -> X.
get_bounding_b_lat(#job_def{bounding_b_lat = X}) -> X.
get_bounding_b_lon(#job_def{bounding_b_lon = X}) -> X.
get_bounding_c_lat(#job_def{bounding_c_lat = X}) -> X.
get_bounding_c_lon(#job_def{bounding_c_lon = X}) -> X.
get_bounding_d_lat(#job_def{bounding_d_lat = X}) -> X.
get_bounding_d_lon(#job_def{bounding_d_lon = X}) -> X.
get_radar_mode(#job_def{radar_mode = X}) -> X.
get_nom_rev_int(#job_def{nom_rev_int = X}) -> X.
get_ns_pos_unc_along_track(#job_def{ns_pos_unc_along_track = X}) -> X.
get_ns_pos_unc_cross_track(#job_def{ns_pos_unc_cross_track = X}) -> X.
get_ns_pos_unc_alt(#job_def{ns_pos_unc_alt = X}) -> X.
get_ns_pos_unc_heading(#job_def{ns_pos_unc_heading = X}) -> X.
get_ns_pos_unc_sensor_speed(#job_def{ns_pos_unc_sensor_speed = X}) -> X.
get_ns_val_slant_range_std_dev(#job_def{ns_val_slant_range_std_dev = X}) -> X.
get_ns_val_cross_range_std_dev(#job_def{ns_val_cross_range_std_dev = X}) -> X.
get_ns_val_tgt_vel_los_std_dev(#job_def{ns_val_tgt_vel_los_std_dev = X}) -> X.
get_ns_val_mdv(#job_def{ns_val_mdv = X}) -> X.
get_ns_val_det_prob(#job_def{ns_val_det_prob = X}) -> X.
get_ns_val_false_alarm_density(#job_def{ns_val_false_alarm_density = X}) -> X.
get_terr_elev_model(#job_def{terr_elev_model = X}) -> X.
 
