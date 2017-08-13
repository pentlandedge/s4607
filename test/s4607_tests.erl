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

-export([east_fortune_packet_list/0]).

s4607_test_() ->
    [encode_mission_segment_check(), encode_job_def_segment_check(),
     encode_dwell_segment_check1(), encode_dwell_segment_check2(),
     encode_free_text_check(), encode_packet_loc_segment_check(),
     mission_packet_encode(), job_def_packet_encode(), dwell_packet_encode(),
     free_text_packet_encode(), platform_loc_packet_encode(),
     packet_list_encode(), display_unsupported_seg(), 
     decode_unrecognised_seg(), encode_unrecocgnised_seg(),
     decode_junk(), check_is_4607(), check_is_not_4607()].

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

encode_packet_loc_segment_check() ->
    % Create a packet location segment.
    PLS = platform_loc:new(45743453, 14.8, 347.9, 123346, 48.1, 94321, 20),

    % Create a complete segment with the header and payload.
    Seg = segment:new(platform_loc, PLS),

    % Encode the segment.
    {ok, ES} = segment:encode(Seg),

    % Decode the segment again.
    [{_, _, DS}] = segment:decode_segments(ES, []),

    % Check the fields in the decoded segment match what we expect.
    [?_assertEqual(45743453, platform_loc:get_location_time(DS)),
     ?_assert(almost_equal(14.8, platform_loc:get_lat(DS), 0.000001)),
     ?_assert(almost_equal(347.9, platform_loc:get_lon(DS), 0.000001)),
     ?_assertEqual(123346, platform_loc:get_alt(DS)),
     ?_assert(almost_equal(48.1, platform_loc:get_platform_track(DS), 0.01)),
     ?_assertEqual(94321, platform_loc:get_platform_speed(DS)),
     ?_assertEqual(20, platform_loc:get_platform_vertical_velocity(DS))].

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

encode_free_text_check() ->
    % Create a new segment record.
    {ok, FT} = free_text:new("Alice", "Bob", "Free text message"),
    Seg = segment:new(free_text, FT),

    % Encode the segment.
    {ok, ES} = segment:encode(Seg),

    % Decode the segment again.
    [{_, _, DS}] = segment:decode_segments(ES, []),

    % Check the fields in the decoded segment match what we expect.
    [?_assertEqual("Alice     ", free_text:get_originator(DS)),
     ?_assertEqual("Bob       ", free_text:get_recipient(DS)),
     ?_assertEqual("Free text message", free_text:get_text(DS))].

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

dwell_packet_encode() ->
    % List the parameters for the packet header (no need to set size).
    PL = [{version, {3, 1}}, {nationality, "UK"},
          {classification, top_secret}, {class_system, "UK"},
          {packet_code, rel_9_eyes}, {exercise_ind, operation_real},
          {platform_id, "Pico1"}, {mission_id, 16#11223344},
          {job_id, 16#55667788}],

    % Create a generator function
    Gen = s4607:packet_generator(PL),

    % Create a dwell segment to encode.
    DS = dwell_tests:one_target_dwell(),

    % Create a complete segment with the header and payload.
    Seg = segment:new(dwell, DS),

    % Create a packet with the new segment.
    Pack = Gen([Seg]),

    % Encode the packet.
    EP = s4607:encode_packet(Pack),

    % Decode it again.
    [DP] = s4607:decode(EP),

    % Pull out the individual records.
    [DPS] = s4607:get_packet_segments(DP),
    SegHdr = segment:get_header(DPS),
    DEDS = segment:get_data(DPS),
    EM = dwell:get_existence_mask(DEDS),

    [?_assertEqual(dwell, seg_header:get_segment_type(SegHdr)),
     ?_assertEqual(1, exist_mask:get_revisit_index(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_index(EM)),
     ?_assertEqual(1, exist_mask:get_last_dwell_of_revisit(EM)),
     ?_assertEqual(1, exist_mask:get_target_report_count(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_time(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_lat(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_lon(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_alt(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_range_half_extent(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_angle_half_extent(EM)),

     ?_assertEqual(0, exist_mask:get_spu_along_track(EM)),
     ?_assertEqual(0, exist_mask:get_spu_alt(EM)),
     ?_assertEqual(1, exist_mask:get_mti_report_index(EM)),
     ?_assertEqual(1, exist_mask:get_target_hr_lat(EM)),
     ?_assertEqual(1, exist_mask:get_target_hr_lon(EM)),
     ?_assertEqual(0, exist_mask:get_mdv(EM)),

     ?_assertEqual(100, dwell:get_revisit_index(DEDS)),
     ?_assertEqual(20000, dwell:get_dwell_index(DEDS)),
     ?_assertEqual(no_additional_dwells, dwell:get_last_dwell_of_revisit(DS)),
     ?_assertEqual(1, dwell:get_target_report_count(DEDS)),
     ?_assertEqual(1000000, dwell:get_dwell_time(DEDS)),
     ?_assert(almost_equal(-45.0, dwell:get_sensor_lat(DEDS), 0.0000001)),
     ?_assert(almost_equal(350.0, dwell:get_sensor_lon(DEDS), 0.0000001)),
     ?_assertEqual(-10000, dwell:get_sensor_alt(DEDS)),
     ?_assert(almost_equal(255.0, dwell:get_dwell_range_half_extent(DEDS), 0.0000001)),
     ?_assert(almost_equal(350.0, dwell:get_dwell_angle_half_extent(DEDS), 0.1))].

free_text_packet_encode() ->

    % List the parameters for the packet header (no need to set size).
    PL = [{version, {3, 1}}, {nationality, "UK"},
          {classification, top_secret}, {class_system, "UK"},
          {packet_code, rel_9_eyes}, {exercise_ind, operation_real},
          {platform_id, "Pico1"}, {mission_id, 16#11223344},
          {job_id, 16#55667788}],

    % Create a generator function
    Gen = s4607:packet_generator(PL),

    % Create a new segment record.
    {ok, FT} = free_text:new("Alice", "Bob", "Free text message"),
    Seg = segment:new(free_text, FT),

    % Create a packet with the new segment.
    Pack = Gen([Seg]),

    % Encode the packet.
    EP = s4607:encode_packet(Pack),

    % Decode it again.
    [DP] = s4607:decode(EP),

    % Pull out the individual records.
    [DPS] = s4607:get_packet_segments(DP),
    DEFT = segment:get_data(DPS),

    % Check the fields in the decoded segment match what we expect.
    [?_assertEqual("Alice     ", free_text:get_originator(DEFT)),
     ?_assertEqual("Bob       ", free_text:get_recipient(DEFT)),
     ?_assertEqual("Free text message", free_text:get_text(DEFT))].

platform_loc_packet_encode() ->
    % Create a mission segment.
    PLS = platform_loc:new(45743453, 14.8, 347.9, 123346, 48.1, 94321, 20),

    % Create a complete segment with the header and payload.
    Seg = segment:new(platform_loc, PLS),

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
    DEPLS = segment:get_data(DPS),

    % Run some checks on the decoded packet and check it is what we expect.
    [?_assertEqual(Size, pheader:get_packet_size(DPH)),
     ?_assertEqual(platform_loc, seg_header:get_segment_type(SegHdr)),
     ?_assertEqual(28, seg_header:get_segment_size(SegHdr)),
     ?_assertEqual(45743453, platform_loc:get_location_time(DEPLS)),
     ?_assert(almost_equal(14.8, platform_loc:get_lat(DEPLS), 0.000001)),
     ?_assert(almost_equal(347.9, platform_loc:get_lon(DEPLS), 0.000001)),
     ?_assertEqual(123346, platform_loc:get_alt(DEPLS)),
     ?_assert(almost_equal(48.1, platform_loc:get_platform_track(DEPLS), 0.01)),
     ?_assertEqual(94321, platform_loc:get_platform_speed(DEPLS)),
     ?_assertEqual(20, platform_loc:get_platform_vertical_velocity(DEPLS))].

packet_list_encode() ->
    % List the parameters for the packet header (no need to set size).
    PL = [{version, {3, 1}}, {nationality, "UK"},
          {classification, top_secret}, {class_system, "UK"},
          {packet_code, rel_9_eyes}, {exercise_ind, operation_real},
          {platform_id, "Pico1"}, {mission_id, 16#11223344},
          {job_id, 16#55667788}],

    % Create a generator function
    Gen = s4607:packet_generator(PL),

    % Create a dwell segment to encode.
    DS = dwell_tests:one_target_dwell(),

    % Create a complete segment with the header and payload.
    Seg = segment:new(dwell, DS),

    % Create a packet with the new segment.
    Pack = Gen([Seg]),

    % Create a mission segment.
    MS = mission:new("Drifter 1", "A1234", other, "Build 1", 2016, 2, 5),

    % Create a complete segment with the header and payload.
    MissionSeg = segment:new(mission, MS),

    % Create a packet containing the mission segment
    Pack2 = Gen([MissionSeg]),

    % Create a job definition segment.
    JD = job_def_tests:sample_job_def(),

    % Create a complete segment with the header and payload.
    JobDefSeg = segment:new(job_definition, JD),

    % Create a packet with the new segment.
    Pack3 = Gen([JobDefSeg]),

    % Create a list containing two packets.
    PackList = [Pack3, Pack2, Pack],

    % Display it
    s4607:display_packets(PackList),

    % Encode it.
    [_,EncodedMissionPacket|_] = s4607:encode_packets(PackList),

    % Decode it again.
    DecodedMissionPacket = s4607:decode(EncodedMissionPacket),

    % Filter the packet list to get the dwell segments.
    [DwellSeg] = s4607:get_segments_by_type([dwell], PackList),
    [MissSeg] = s4607:get_segments_by_type([mission], DecodedMissionPacket),
    DwellData = segment:get_data(DwellSeg),
    MissData = segment:get_data(MissSeg),
    [?_assertEqual(100, dwell:get_revisit_index(DwellData)),
     ?_assertEqual("Drifter 1", mission:get_mission_plan(MissData))].

%% Checks that we get an error when attempting to display a segment of an
%% unsupported or invalid type.
display_unsupported_seg() ->
    % Create a segment header for a segment we don't yet support.
    SH = seg_header:new(group, 20), 
    % Create some dummy segment data.
    SegData = {1,2,3,4},
    % Dirty hack to build the segment since the new function will refuse to 
    % build something it doesn't recognise.
    Seg = {segment, SH, SegData},
    % Try to display it, expect an error.
    Ret = segment:display(Seg),
    [?_assertEqual({error, unsupported_segment_type}, Ret)]. 

%% Checks that an attempt to decode an unrecognised segment type returns an 
%% error and the original binary.
decode_unrecognised_seg() ->
    %% Create a binary segment header with an invalid segment type.
    %% Set type = 200, seg size = 9 (hdr = 5, payload = 4).
    SegHdrBin = <<200,0,0,0,9>>,
    SegDataBin = <<1,2,3,4>>,
    SegBin = <<SegHdrBin/binary, SegDataBin/binary>>,
    Ret = segment:decode_segments(SegBin, []),
    [SegRec] = Ret,
    SegHdr = segment:get_header(SegRec),
    SegData = segment:get_data(SegRec),
    [?_assertEqual(reserved, seg_header:get_segment_type(SegHdr)),
     ?_assertEqual(SegDataBin, SegData)]. 

%% Check that an attempt to encode an unrecognised segment type fails
%% gracefully.
encode_unrecocgnised_seg() ->
    %% Create a fictional segment type header and data record.
    SH = seg_header:new(madeup, 9),
    SegData = {madeup, 1, 2, 3, 4},
    % Hack time again to get around the protections on segment:new().
    Seg = {segment, SH, SegData},
    Ret = segment:encode(Seg),
    [?_assertEqual({error, unsupported_segment_type}, Ret)]. 

%% Check that feeding junk into the decode returns an error.
decode_junk() ->
    Bin = <<"Some garbage to decode.">>,
    {Code, Reason} = s4607:decode(Bin),
    [?_assertEqual({error, failed_decode}, {Code, Reason})]. 

%% Check that the valid 4607 is detected as such.
check_is_4607() -> 
    PacketList = east_fortune_packet_list(),
    % List of binaries is not flattened, so pick the first packet.
    [Bin|_] = s4607:encode_packets(PacketList),
    [?_assertEqual(true, s4607:is_4607(Bin))]. 
    
%% Check that a string of bytes is not detected as 4607. 
check_is_not_4607() -> 
    Bin = <<"12345678901234567890123456789012">>,
    [?_assertEqual(false, s4607:is_4607(Bin))]. 

%% Function to create a list of packets containing dwells based on
%% East Fortune runway.
east_fortune_packet_list() ->
    % List the parameters for the packet header (no need to set size).
    PL = [{version, {3, 1}}, {nationality, "UK"},
          {classification, unclassified}, {class_system, "UK"},
          {packet_code, none}, {exercise_ind, exercise_real},
          {platform_id, "Pico1"}, {mission_id, 16#11223344},
          {job_id, 16#55667788}],

    % Create a generator function
    Gen = s4607:packet_generator(PL),

    % Create a mission segment.
    MS = mission:new("EastFortune1", "A1234", other, "Build 1", 2016, 4, 7),

    % Create a complete segment with the header and payload.
    MissionSeg = segment:new(mission, MS),

    % Create a dwell segment with a target in it.

    % Create a list of fields for the existence mask (excluding the target
    % report).
    F = [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit,
         target_report_count, dwell_time, sensor_lat, sensor_lon,
         sensor_alt, dwell_center_lat, dwell_center_lon,
         dwell_range_half_extent, dwell_angle_half_extent, targets],

    TgtParams = east_fortune_target_params(),

    % Extract the list of fields in the target report.
    FieldList = [K || {K, _V} <- TgtParams],

    % Splice together all the fields that make up the existence mask.
    Efields = F ++ FieldList,

    % Create the existence mask.
    EM = exist_mask:new(Efields),

    % Create the target report.
    TgtRep = tgt_report:new(TgtParams),

    % Set the fields of the dwell segment.
    P = [{existence_mask, EM}, {revisit_index, 0}, {dwell_index, 0},
         {last_dwell_of_revisit, no_additional_dwells}, {target_report_count, 1},
         {dwell_time, 36000000}, {sensor_lat, 55.9975}, {sensor_lon, -2.725},
         {sensor_alt, 100000}, {dwell_center_lat, 55.9990},
         {dwell_center_lon, -2.713}, {dwell_range_half_extent, 1.0},
         {dwell_angle_half_extent, 10}, {targets, [TgtRep]}],

    % Create the dwell segment.
    DS = dwell:new(P),

    % Create a complete segment with the header and payload.
    DwellSeg = segment:new(dwell, DS),

    % Create a packet with the new segment.
    Pack1 = Gen([MissionSeg]),
    Pack2 = Gen([DwellSeg]),

    % Create a list containing two packets.
    [Pack1, Pack2].

%% Helper function to build target reports around East Fortune.
east_fortune_target_params() ->
    % The default fields of the target report.
    [{target_hr_lat, 55.9987}, {target_hr_lon, -2.710},
     {geodetic_height, 54}].

%% Utility function to compare whether floating point values are within a
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
