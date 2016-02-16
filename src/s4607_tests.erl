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

-module(s4607_tests).

-include_lib("eunit/include/eunit.hrl").

s4607_test_() ->
    [encode_mission_segment_check(), encode_job_def_segment_check(),
     encode_dwell_segment_check1(), encode_dwell_segment_check2(),
     mission_packet_encode(), job_def_packet_encode()].
   
encode_mission_segment_check() ->
    % Create a mission segment.
    MS = mission:new("Drifter 1", "A1234", other, "Build 1", 2016, 2, 5),

    % Create a complete segment with the header and payload.
    Seg = segment:new(mission, MS),

    % Encode the segment.
    {ok, ES} = segment:encode(Seg),

    % Decode the segment again.
    [{_, _, DS}] = segment:decode_segments(ES, []),

    % Check the fields in the decoded segment match what we expect.
    [?_assertEqual("Drifter 1", mission:get_mission_plan(DS)),
     ?_assertEqual("A1234", mission:get_flight_plan(DS)),
     ?_assertEqual(other, mission:get_platform_type(DS)),
     ?_assertEqual("Build 1", mission:get_platform_config(DS)),
     ?_assertEqual(2016, mission:get_year(DS)),
     ?_assertEqual(2, mission:get_month(DS)),
     ?_assertEqual(5, mission:get_day(DS))].

encode_job_def_segment_check() ->
    % Create a job definition segment.
    JD = job_def_tests:sample_job_def(),

    % Create the complete segment record.
    Seg = segment:new(job_definition, JD),
    
    % Encode the segment.
    {ok, ES} = segment:encode(Seg),

    % Decode the segment again.
    [{_, _, DS}] = segment:decode_segments(ES, []),

    % Check a couple of the fields. 
    [?_assertEqual(rotary_wing_radar, job_def:get_sensor_id_type(DS)),
     ?_assertEqual(dgm50, job_def:get_terr_elev_model(DS))].

encode_dwell_segment_check1() ->
    % Create a dwell segment.
    Dwell = dwell_tests:one_target_dwell(),
    
    % Create the complete segment record.
    Seg = segment:new(dwell, Dwell),

    % Encode the segment.
    {ok, ES} = segment:encode(Seg),

    % Decode the segment again.
    [{_, _, DS}] = segment:decode_segments(ES, []),

    % Fetch the target report.
    [TR] = dwell:get_targets(DS),

    % Check a couple of the fields. 
    [?_assertEqual(34, tgt_report:get_mti_report_index(TR)),
     ?_assertEqual(vehicle_live_target, tgt_report:get_target_classification(TR))].

encode_dwell_segment_check2() ->
    % Create a dwell segment.
    Dwell = dwell_tests:minimal_dwell(),

    % Create the complete segment record.
    Seg = segment:new(dwell, Dwell),
    
    % Encode the segment.
    {ok, ES} = segment:encode(Seg),

    % Decode the segment again.
    [{_, _, DS}] = segment:decode_segments(ES, []),

    % Check a couple of the fields. 
    [?_assertEqual(100, dwell:get_revisit_index(DS)),
     ?_assertEqual(-10000, dwell:get_sensor_alt(DS))].

mission_packet_encode() ->
    % Create a mission segment.
    MS = mission:new("Drifter 1", "A1234", other, "Build 1", 2016, 2, 5),

    % Create a complete segment with the header and payload.
    Seg = segment:new(mission, MS),

    % Work out the size of the packet.
    PaySize = s4607:packet_payload_size([Seg]),
    Size = pheader:header_size() + PaySize,

    % List the parameters for the packet header. 
    PL = [{version, {3, 1}}, {packet_size, Size}, {nationality, "UK"},
          {classification, top_secret}, {class_system, "UK"}, 
          {packet_code, rel_9_eyes}, {exercise_ind, operation_real},
          {platform_id, "Pico1"}, {mission_id, 16#11223344},
          {job_id, 16#55667788}],

    % Create a packet header.
    PktHdr = pheader:new(PL),

    % Wrap the segment inside a packet.
    Pack = s4607:new_packet(PktHdr, [Seg]),

    % Encode the packet.
    EP = s4607:encode_packet(Pack),

    % Decode it again.
    [DP] = s4607:decode(EP),

    % Pull out the individual records.
    DPH = s4607:get_packet_header(DP),
    [DPS] = s4607:get_packet_segments(DP),
    SegHdr = segment:get_header(DPS),
    MissSeg = segment:get_data(DPS),

    % Run some checks on the decoded packet and check it is what we expect.
    [?_assertEqual(Size, pheader:get_packet_size(DPH)),
     ?_assertEqual(mission, seg_header:get_segment_type(SegHdr)),
     ?_assertEqual(44, seg_header:get_segment_size(SegHdr)),
     ?_assertEqual(other, mission:get_platform_type(MissSeg))].

job_def_packet_encode() ->

    % List the parameters for the packet header (no need to set size). 
    PL = [{version, {3, 1}}, {nationality, "UK"},
          {classification, top_secret}, {class_system, "UK"}, 
          {packet_code, rel_9_eyes}, {exercise_ind, operation_real},
          {platform_id, "Pico1"}, {mission_id, 16#11223344},
          {job_id, 16#55667788}],

    % Create a generator function
    Gen = s4607:packet_generator(PL),

    % Create a sample job definition segment to test with.
    JD = job_def_tests:sample_job_def(),

    % Create a complete segment with the header and payload.
    Seg = segment:new(job_definition, JD),

    % Create a packet with the new segment.
    Pack = Gen([Seg]), 
    
    % Encode the packet.
    EP = s4607:encode_packet(Pack),

    % Decode it again.
    [DP] = s4607:decode(EP),

    % Pull out the individual records.
    DPH = s4607:get_packet_header(DP),
    [DPS] = s4607:get_packet_segments(DP),
    SegHdr = segment:get_header(DPS),
    DEJD = segment:get_data(DPS),

    [?_assertEqual(105, pheader:get_packet_size(DPH)),
     ?_assertEqual(job_definition, seg_header:get_segment_type(SegHdr)),
     ?_assertEqual(73, seg_header:get_segment_size(SegHdr)),
     ?_assertEqual(100, job_def:get_job_id(DEJD)),
     ?_assertEqual(rotary_wing_radar, job_def:get_sensor_id_type(DEJD)),
     ?_assertEqual("Heli 1", job_def:get_sensor_id_model(DEJD)),
     ?_assertEqual(no_filtering, job_def:get_target_filt_flag(DEJD)),
     ?_assertEqual(30, job_def:get_priority(DEJD)),
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


