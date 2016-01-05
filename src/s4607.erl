-module(s4607).

-export([
    read_file/1,
    extract_packet_header/1,
    extract_packet_data/2,
    display_packets/1,
    decode_packet_header/1,
    decode_us_packet_code/1,
    display_packet_header/1,
    decode_segment_header/1,
    display_segment_header/1,
    decode_mission_segment/1,
    display_mission_segment/1,
    header_test/0,
    mission_test/0,
    decode_dwell_segment/1,
    display_dwell_segment/1,
    display_existence_mask/1,
    trim_trailing_spaces/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Record definitions.

-record(pheader, {
    version, 
    packet_size,
    nationality, 
    classification, 
    class_system, 
    packet_code, 
    exercise_ind, 
    platform_id, 
    mission_id, 
    job_id}).

-record(seg_header, {type, size}).

-record(mission_segment, {
    mission_plan, 
    flight_plan, 
    platform_type,
    platform_config,
    year,
    month,
    day}).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File handling functions.

%% Function to read a file in Stanag 4607 format.
read_file(File) ->
    {ok, Bin} = file:read_file(File),
    Bin.

%% Packet processing loop, prints out decoded information.
display_packets(<<>>) ->
    ok;
display_packets(Bin) ->
    {ok, Hdr, R1} = extract_packet_header(Bin),
    H1 = s4607:decode_packet_header(Hdr),
    %s4607:display_packet_header(H1),
    io:format("~n"),
    % The size in the header includes the header itself.
    PayloadSize = H1#pheader.packet_size - byte_size(Hdr),
    io:format("size ~p, len ~p~n", [byte_size(R1), PayloadSize]),
    % Get the packet data payload.
    {ok, PktData, R2} = extract_packet_data(R1, PayloadSize),

    % Loop through all the segments in the packet.
    display_segments(PktData),

    % Loop over any remaining packets.
    display_packets(R2).

%% Display all the segments within a packet.
display_segments(<<>>) ->
    ok;
display_segments(Bin) ->
    % Get the segment header.
    {ok, SegHdr, SRem} = extract_segment_header(Bin),
    SH = decode_segment_header(SegHdr),
    display_segment_header(SH),
    io:format("~n"),

    % The size in the header includes the header itself.
    PayloadSize = SH#seg_header.size - byte_size(SegHdr),

    % Get the packet data payload.
    {ok, SegData, SRem2} = extract_segment_data(SRem, PayloadSize),

    % Switch on the segment type
    case SH#seg_header.type of
        mission -> 
            MS = decode_mission_segment(SegData),
            display_mission_segment(MS);
        dwell   ->
            DS = decode_dwell_segment(SegData),
            display_dwell_segment(DS);
        _       -> 
            ok
    end,

    % Loop over any remaining segments contained in this packet.
    display_segments(SRem2).

%% Extracts the first portion of the binary of the size required for a packet
%% header. Returns the unused portion to allow further processing.
extract_packet_header(<<Hdr:32/binary,Rest/binary>>) ->
    {ok, Hdr, Rest}.

%% Extracts the data payload from a packet from the supplied binary
%% (which should have had the header removed already).
extract_packet_data(Bin, Len) ->
    extract_data(Bin, Len).

%% Extracts the first binary portion associated with a segment header.
extract_segment_header(<<Hdr:5/binary,Rest/binary>>) ->
    {ok, Hdr, Rest}.

%% Extracts the segment payload from the supplied binary
%% (which should have had the header removed already).
extract_segment_data(Bin, Len) ->
    extract_data(Bin, Len).

%% Generic function to take the first part of a binary and return the rest.
extract_data(Bin, Len) ->
    Data = binary:part(Bin, 0, Len),
    Rem = binary:part(Bin, Len, (byte_size(Bin) - Len)),
    {ok, Data, Rem}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Packet header decoding functions.

%% Function to decode a Stanag 4607 packet header. 
decode_packet_header(<<P1:2/binary, PktSize:32/integer-unsigned-big, 
    P3:2/binary, P4, P5:2/binary, P6:16/integer-unsigned-big, P7, 
    P8:10/binary, P9:4/binary, P10:4/binary>>) ->

    Ver = decode_version(P1),
    Nat = decode_nationality(P3),

    % Don't crash if we don't recognise the classification.
    Class = case decode_classification(P4) of
                {ok, X} -> X;
                {unknown_classification, _} -> unknown_classification
            end,

    {ok, Sys} = decode_class_system(P5), 
    {ok, Code} = decode_us_packet_code(P6),
    {ok, Ex} = decode_exercise_indicator(P7),
    PlatId = decode_platform_id(P8),
    MissId = stanag_types:i32_to_integer(P9),
    JobId = stanag_types:i32_to_integer(P10),
    
    #pheader{version = Ver, packet_size = PktSize, nationality = Nat, 
        classification = Class, class_system = Sys, packet_code = Code, 
        exercise_ind = Ex, platform_id = PlatId, mission_id = MissId, 
        job_id = JobId}.

decode_version(<<M,N>>) ->
    {M - $0, N - $0}.

decode_nationality(<<X:2/binary>>) ->
    binary_to_list(X).

decode_classification(1) -> {ok, top_secret};
decode_classification(2) -> {ok, secret};
decode_classification(3) -> {ok, confidential};
decode_classification(4) -> {ok, restricted};
decode_classification(5) -> {ok, unclassified};
decode_classification(X) -> {unknown_classification, X}.

decode_class_system(<<"  ">>) ->
    {ok, none};
decode_class_system(<<X:2/binary>>) ->
    {ok, binary_to_list(X)}.

decode_us_packet_code(16#0000) -> {ok, none};
decode_us_packet_code(16#0001) -> {ok, nocontract};
decode_us_packet_code(16#0002) -> {ok, orcon};
decode_us_packet_code(16#0004) -> {ok, propin};
decode_us_packet_code(16#0008) -> {ok, wnintel};
decode_us_packet_code(16#0010) -> {ok, national_only};
decode_us_packet_code(16#0020) -> {ok, limdis};
decode_us_packet_code(16#0040) -> {ok, fouo};
decode_us_packet_code(16#0080) -> {ok, efto};
decode_us_packet_code(16#0100) -> {ok, lim_off_use};
decode_us_packet_code(16#0200) -> {ok, noncompartment};
decode_us_packet_code(16#0400) -> {ok, special_control};
decode_us_packet_code(16#0800) -> {ok, special_intel};
decode_us_packet_code(16#1000) -> {ok, warning_notice};
decode_us_packet_code(16#2000) -> {ok, rel_nato};
decode_us_packet_code(16#4000) -> {ok, rel_4_eyes};
decode_us_packet_code(16#8000) -> {ok, rel_9_eyes};
decode_us_packet_code(_) -> {error, unknown_packet_code}.

decode_exercise_indicator(0) -> {ok, operation_real};
decode_exercise_indicator(1) -> {ok, operation_simulated};
decode_exercise_indicator(2) -> {ok, operation_synthesized};
decode_exercise_indicator(128) -> {ok, exercise_real};
decode_exercise_indicator(129) -> {ok, exercise_simulated};
decode_exercise_indicator(130) -> {ok, exercise_synthesized};
decode_exercise_indicator(_) -> {error, reserved}.

decode_platform_id(<<X:10/binary>>) ->
    trim_trailing_spaces(binary_to_list(X)).

display_packet_header(PktHdr) ->
    io:format("Version: ~p~n", [PktHdr#pheader.version]),
    io:format("Packet size: ~p~n", [PktHdr#pheader.packet_size]), 
    io:format("Nationality: ~p~n", [PktHdr#pheader.nationality]),
    io:format("Classification: ~p~n", [PktHdr#pheader.classification]),
    io:format("Classification System: ~p~n", [PktHdr#pheader.class_system]),
    io:format("Packet code: ~p~n", [PktHdr#pheader.packet_code]),
    io:format("Exercise Indication: ~p~n", [PktHdr#pheader.exercise_ind]),
    io:format("Platform ID: ~p~n", [PktHdr#pheader.platform_id]),
    io:format("Mission ID: ~p~n", [PktHdr#pheader.mission_id]),
    io:format("Job ID: ~p~n", [PktHdr#pheader.job_id]).

header_test() ->
    <<"12",0,0,0,32, "UK", 1, "  ", 0, 1, 128, "ABCDEFGHIJ", 0, 0, 0, 5, 0, 0, 0, 6>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Segment header decoding functions.

decode_segment_header(<<S1, SegSize:32/integer-unsigned-big>>) ->
    SegType = decode_segment_type(S1),
    #seg_header{type = SegType, size = SegSize}.

decode_segment_type(1) -> mission;
decode_segment_type(2) -> dwell;
decode_segment_type(3) -> hrr;
decode_segment_type(4) -> reserved;
decode_segment_type(5) -> job_definition;
decode_segment_type(6) -> free_text;
decode_segment_type(7) -> low_reflectivity_index;
decode_segment_type(8) -> group;
decode_segment_type(9) -> attached_target;
decode_segment_type(10) -> test_and_status;
decode_segment_type(11) -> system_specific;
decode_segment_type(12) -> processing_history;
decode_segment_type(13) -> platform_location;
decode_segment_type(101) -> job_request;
decode_segment_type(102) -> job_acknowledge;
decode_segment_type(_) -> reserved.

display_segment_header(SegHdr) ->
    io:format("Segment type: ~p~n", [SegHdr#seg_header.type]),
    io:format("Segment size: ~p~n", [SegHdr#seg_header.size]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mission segment decoding functions.

decode_mission_segment(<<M1:12/binary, M2:12/binary, M3, M4:10/binary, 
    Year:16/integer-unsigned-big, Month, Day>>) ->

    Mission = trim_trailing_spaces(binary_to_list(M1)),
    Flight = trim_trailing_spaces(binary_to_list(M2)),
    Type = decode_platform_type(M3),
    Config = trim_trailing_spaces(binary_to_list(M4)),

    #mission_segment{mission_plan = Mission, flight_plan = Flight, 
        platform_type = Type, platform_config = Config, year = Year,
        month = Month, day = Day}.

decode_platform_type(0) -> unidentfified;
decode_platform_type(1) -> acs;
decode_platform_type(2) -> arl_m;
decode_platform_type(3) -> sentinel;
decode_platform_type(4) -> rotary_wing_radar;
decode_platform_type(5) -> global_hawk_navy;
decode_platform_type(6) -> horizon;
decode_platform_type(7) -> e_8;
decode_platform_type(8) -> p_3c;
decode_platform_type(9) -> predator;
decode_platform_type(10) -> radarsat2;
decode_platform_type(11) -> u_2;
decode_platform_type(12) -> e_10;
decode_platform_type(13) -> ugs_single;
decode_platform_type(14) -> ugs_cluster;
decode_platform_type(15) -> ground_based;
decode_platform_type(16) -> uav_army;
decode_platform_type(17) -> uav_marines;
decode_platform_type(18) -> uav_navy;
decode_platform_type(19) -> uav_air_force;
decode_platform_type(20) -> global_hawk_air_force;
decode_platform_type(21) -> global_hawk_australia;
decode_platform_type(22) -> global_hawk_germany;
decode_platform_type(23) -> paul_revere;
decode_platform_type(24) -> mariner_uav;
decode_platform_type(25) -> bac_111;
decode_platform_type(26) -> coyote;
decode_platform_type(27) -> king_air;
decode_platform_type(28) -> limit;
decode_platform_type(29) -> nrl_np_3b;
decode_platform_type(30) -> solstar_x;
decode_platform_type(31) -> watchkeeper;
decode_platform_type(32) -> alliance_ground_surveillance;
decode_platform_type(33) -> stryker;
decode_platform_type(34) -> ags_hale_uav;
decode_platform_type(35) -> sidm;
decode_platform_type(36) -> reaper;
decode_platform_type(37) -> warrior_a;
decode_platform_type(38) -> warrior;
decode_platform_type(39) -> twin_otter;
decode_platform_type(255) -> other;
decode_platform_type(_) -> future_use.

mission_test() ->
    <<"Global Domin","Fly By      ",36,"Skynet v12",16#07, 16#DF, 12, 31>>.

display_mission_segment(MSeg) ->
    io:format("Mission Plan: ~p~n", [MSeg#mission_segment.mission_plan]),
    io:format("Flight Plan: ~p~n", [MSeg#mission_segment.flight_plan]),
    io:format("Plat. Type: ~p~n", [MSeg#mission_segment.platform_type]),
    io:format("Plat. Config: ~p~n", [MSeg#mission_segment.platform_config]),
    io:format("Year: ~p~n", [MSeg#mission_segment.year]),
    io:format("Month: ~p~n", [MSeg#mission_segment.month]),
    io:format("Day: ~p~n", [MSeg#mission_segment.day]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dwell segment decoding functions.

decode_dwell_segment(<<EM:8/binary,RI:16/integer-unsigned-big,
    DI:16/integer-unsigned-big,LD,TRC:16/integer-unsigned-big,
    DT:32/integer-unsigned-big,SLat:4/binary,SLon:4/binary,SAlt:4/binary,
    Rest/binary>>) ->

    % Fixed part of the dwell segement is pattern matched above, remainder
    % depends on the existence mask.
    EMrec = decode_existence_mask(EM),

    {LatScaleFactor, Bin1} = conditional_extract(
        Rest, 
        EMrec#exist_mask.lat_scale_factor, 
        4, 
        fun stanag_types:sa32_to_float/1, 
        1.0),

    {LonScaleFactor, Bin2} = conditional_extract(
        Bin1, 
        EMrec#exist_mask.lon_scale_factor, 
        4, 
        fun stanag_types:ba32_to_float/1, 
        1.0),
    
    {SpuAlongTrack, Bin3} = conditional_extract(
        Bin2, 
        EMrec#exist_mask.spu_cross_track, 
        4, 
        fun stanag_types:i32_to_integer/1, 
        0),

    {SpuCrossTrack, Bin4} = conditional_extract(
        Bin3, 
        EMrec#exist_mask.spu_cross_track, 
        4, 
        fun stanag_types:i32_to_integer/1, 
        0),

    {SpuAlt, Bin5} = conditional_extract(
        Bin4, 
        EMrec#exist_mask.spu_alt, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {SensorTrack, Bin6} = conditional_extract(
        Bin5, 
        EMrec#exist_mask.sensor_track, 
        2, 
        fun stanag_types:ba16_to_float/1, 
        0.0),

    {SensorSpeed, Bin7} = conditional_extract(
        Bin6, 
        EMrec#exist_mask.sensor_speed, 
        4, 
        fun stanag_types:i32_to_integer/1, 
        0),

    {SensorVertVel, Bin8} = conditional_extract(
        Bin7, 
        EMrec#exist_mask.sensor_vert_vel, 
        1, 
        fun stanag_types:s8_to_integer/1, 
        0),
    
    {SensorTrackUnc, Bin9} = conditional_extract(
        Bin8, 
        EMrec#exist_mask.sensor_track_unc, 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {SensorSpeedUnc, Bin10} = conditional_extract(
        Bin9, 
        EMrec#exist_mask.sensor_speed_unc, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {SensorVertVelUnc, Bin11} = conditional_extract(
        Bin10, 
        EMrec#exist_mask.sensor_vert_vel_unc, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {PlatHeading, Bin12} = conditional_extract(
        Bin11, 
        EMrec#exist_mask.platform_heading, 
        2, 
        fun stanag_types:ba16_to_float/1, 
        0.0),

    {PlatPitch, Bin13} = conditional_extract(
        Bin12, 
        EMrec#exist_mask.platform_pitch, 
        2, 
        fun stanag_types:sa16_to_float/1, 
        0.0),

    {PlatRoll, Bin14} = conditional_extract(
        Bin13, 
        EMrec#exist_mask.platform_roll, 
        2, 
        fun stanag_types:sa16_to_float/1, 
        0.0),

    {DwellCenterLat, Bin15} = conditional_extract(
        Bin14, 
        EMrec#exist_mask.dwell_center_lat, 
        4, 
        fun stanag_types:sa32_to_float/1, 
        0.0),

    {DwellCenterLon, Bin16} = conditional_extract(
        Bin15, 
        EMrec#exist_mask.dwell_center_lon, 
        4, 
        fun stanag_types:ba32_to_float/1, 
        0.0),

    {DwellRangeHalfExtent, Bin17} = conditional_extract(
        Bin16, 
        EMrec#exist_mask.dwell_range_half_extent, 
        2, 
        fun stanag_types:b16_to_float/1, 
        0.0),

    {DwellAngleHalfExtent, Bin18} = conditional_extract(
        Bin17, 
        EMrec#exist_mask.dwell_angle_half_extent, 
        2, 
        fun stanag_types:ba16_to_float/1, 
        0.0),

    {SensorHeading, Bin19} = conditional_extract(
        Bin18, 
        EMrec#exist_mask.sensor_heading, 
        2, 
        fun stanag_types:ba16_to_float/1, 
        0.0),

    {SensorPitch, Bin20} = conditional_extract(
        Bin19, 
        EMrec#exist_mask.sensor_pitch, 
        2, 
        fun stanag_types:sa16_to_float/1, 
        0.0),

    {SensorRoll, Bin21} = conditional_extract(
        Bin20, 
        EMrec#exist_mask.sensor_roll, 
        2, 
        fun stanag_types:sa16_to_float/1, 
        0.0),

    {MDV, Bin22} = conditional_extract(
        Bin21, 
        EMrec#exist_mask.mdv, 
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


%% Function to decode the existance mask. Will crash caller if the mask 
%% does not have the mandatory bits set.
decode_existence_mask(<<16#FF, 
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

display_existence_mask(EM) ->
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
    {ok, TR, Rem} = decode_target_report(Bin, EM),
    decode_target_report_list(Rem, EM, TgtCount-1, [TR|AccTgts]).

decode_target_report(TrBin, EM) ->
    
    {MRI, Rem1} = conditional_extract(
        TrBin, 
        EM#exist_mask.mti_report_index, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtHiResLat, Rem2} = conditional_extract(
        Rem1, 
        EM#exist_mask.target_hr_lat, 
        4, 
        fun stanag_types:sa32_to_float/1, 
        0.0),

    {TgtHiResLon, Rem3} = conditional_extract(
        Rem2, 
        EM#exist_mask.target_hr_lon, 
        4, 
        fun stanag_types:ba32_to_float/1, 
        0.0),

    {TgtDeltaLat, Rem4} = conditional_extract(
        Rem3, 
        EM#exist_mask.target_delta_lat, 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {TgtDeltaLon, Rem5} = conditional_extract(
        Rem4, 
        EM#exist_mask.target_delta_lon, 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {GeodHeight, Rem6} = conditional_extract(
        Rem5, 
        EM#exist_mask.geodetic_height, 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {TgtVelLos, Rem7} = conditional_extract(
        Rem6, 
        EM#exist_mask.target_vel_los, 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {TgtWrapVel, Rem8} = conditional_extract(
        Rem7, 
        EM#exist_mask.target_wrap_velocity, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtSnr, Rem9} = conditional_extract(
        Rem8, 
        EM#exist_mask.target_snr, 
        1, 
        fun stanag_types:s8_to_integer/1, 
        0),

    {TgtClassification, Rem10} = conditional_extract(
        Rem9, 
        EM#exist_mask.target_classification, 
        1, 
        fun decode_target_classification/1, 
        no_information_live_target),

    {TgtClassProb, Rem11} = conditional_extract(
        Rem10, 
        EM#exist_mask.target_class_prob, 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {TgtSlantRgeUnc, Rem12} = conditional_extract(
        Rem11, 
        EM#exist_mask.target_slant_range_unc, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtCrossRgeUnc, Rem13} = conditional_extract(
        Rem12, 
        EM#exist_mask.target_cross_range_unc, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtHeightUnc, Rem14} = conditional_extract(
        Rem13, 
        EM#exist_mask.target_height_unc, 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {TgtRadVelUnc, Rem15} = conditional_extract(
        Rem14, 
        EM#exist_mask.target_rad_vel_unc, 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TruthTagApp, Rem16} = conditional_extract(
        Rem15, 
        EM#exist_mask.truth_tag_app, 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {TruthTagEnt, Rem17} = conditional_extract(
        Rem16, 
        EM#exist_mask.truth_tag_entity, 
        4, 
        fun stanag_types:i32_to_integer/1, 
        0),

    {TgtRcs, Rem18} = conditional_extract(
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
    
display_dwell_segment(DS) ->
    EM = DS#dwell_segment.existence_mask,
    display_existence_mask(EM),
    io:format("Revisit index: ~p~n", [DS#dwell_segment.revisit_index]),
    io:format("Dwell index: ~p~n", [DS#dwell_segment.dwell_index]),
    io:format("Last dwell of revisit: ~p~n", [DS#dwell_segment.last_dwell_of_revisit]),
    io:format("Target report count: ~p~n", [DS#dwell_segment.target_report_count]),
    io:format("Dwell time: ~p~n", [DS#dwell_segment.dwell_time]),
    io:format("Sensor Lat.: ~p~n", [DS#dwell_segment.sensor_lat]),
    io:format("Sensor Lon.: ~p~n", [DS#dwell_segment.sensor_lon]),
    io:format("Sensor alt. (cm): ~p~n", [DS#dwell_segment.sensor_alt]),
    conditional_display("Lat. scale factor: ~p~n", [DS#dwell_segment.lat_scale_factor], EM#exist_mask.lat_scale_factor),
    conditional_display("Lon. scale factor: ~p~n", [DS#dwell_segment.lon_scale_factor], EM#exist_mask.lon_scale_factor),
    conditional_display("SPU along track: ~p~n", [DS#dwell_segment.spu_along_track], EM#exist_mask.spu_along_track),
    conditional_display("SPU cross track: ~p~n", [DS#dwell_segment.spu_cross_track], EM#exist_mask.spu_cross_track),
    conditional_display("SPU alt: ~p~n", [DS#dwell_segment.spu_alt], EM#exist_mask.spu_alt),
    conditional_display("Sensor track: ~p~n", [DS#dwell_segment.sensor_track], EM#exist_mask.sensor_track),
    conditional_display("Sensor speed: ~p~n", [DS#dwell_segment.sensor_speed], EM#exist_mask.sensor_speed),
    conditional_display("Sensor vert. vel.: ~p~n", [DS#dwell_segment.sensor_vert_vel], EM#exist_mask.sensor_vert_vel),
    conditional_display("Sensor track unc.: ~p~n", [DS#dwell_segment.sensor_track_unc], EM#exist_mask.sensor_track_unc),
    conditional_display("Sensor speed unc.: ~p~n", [DS#dwell_segment.sensor_speed_unc], EM#exist_mask.sensor_speed_unc),
    conditional_display("Sensor vert. vel. unc.: ~p~n", [DS#dwell_segment.sensor_vert_vel_unc], EM#exist_mask.sensor_vert_vel_unc),
    conditional_display("Platform heading: ~p~n", [DS#dwell_segment.platform_heading], EM#exist_mask.platform_heading),
    conditional_display("Platform pitch: ~p~n", [DS#dwell_segment.platform_pitch], EM#exist_mask.platform_pitch),
    conditional_display("Platform roll: ~p~n", [DS#dwell_segment.platform_roll], EM#exist_mask.platform_roll),
    conditional_display("Dwell centre Lat.: ~p~n", [DS#dwell_segment.dwell_center_lat], EM#exist_mask.dwell_center_lat),
    conditional_display("Dwell centre Lon.: ~p~n", [DS#dwell_segment.dwell_center_lon], EM#exist_mask.dwell_center_lon),
    conditional_display("Dwell range half extent: ~p~n", [DS#dwell_segment.dwell_range_half_extent], EM#exist_mask.dwell_range_half_extent),
    conditional_display("Dwell angle half extent: ~p~n", [DS#dwell_segment.dwell_angle_half_extent], EM#exist_mask.dwell_angle_half_extent),
    conditional_display("Sensor heading: ~p~n", [DS#dwell_segment.sensor_heading], EM#exist_mask.sensor_heading),
    conditional_display("Sensor pitch: ~p~n", [DS#dwell_segment.sensor_pitch], EM#exist_mask.sensor_pitch),
    conditional_display("Sensor roll: ~p~n", [DS#dwell_segment.sensor_roll], EM#exist_mask.sensor_roll),
    conditional_display("MDV: ~p~n", [DS#dwell_segment.mdv], EM#exist_mask.mdv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility functions

trim_trailing_spaces(Str) ->
    Rev = lists:reverse(Str),
    F = fun(C) -> C =:= $\  end,
    RStrip = lists:dropwhile(F, Rev),
    lists:reverse(RStrip).

%% Function to conditionally extract a paramater from the front of a binary
%% based on the state of a mask bit.
conditional_extract(Bin, MaskBit, Size, ConvFn, Default) ->
    case MaskBit of
        1 -> 
            {ok, Param, Bin1} = extract_data(Bin, Size),
            {ConvFn(Param), Bin1};
        0 ->
            {Default, Bin}
    end.

%% Function to conditionally display a parameter based on a mask bit
conditional_display(FmtStr, Params, MaskBit) ->
    case MaskBit of
        1 ->
            io:format(FmtStr, Params),
            ok;
        0 ->
            ok
    end.

