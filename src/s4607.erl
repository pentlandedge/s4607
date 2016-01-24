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
-module(s4607).

-export([
    read_file/1,
    decode/1,
    extract_packet_header/1,
    extract_packet_data/2,
    display_packets/1,
    decode_packet_header/1,
    decode_us_packet_code/1,
    display_packet_header/1,
    get_version_id/1,
    get_packet_size/1,
    get_nationality/1,
    get_classification/1,
    get_class_system/1,
    get_packet_code/1,
    get_exercise_indicator/1,
    get_platform_id/1,
    get_mission_id/1,
    get_job_id/1,
    decode_dwell_segment/1,
    display_dwell_segment/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Record definitions.

-record(packet, {header, segments}).

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

%-record(segment, {header, data}).

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

%-record(decode_status, {status, error_list}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File handling functions.

%% Function to read a file in Stanag 4607 format.
read_file(File) ->
    {ok, Bin} = file:read_file(File),
    Bin.

%% Function to decode binary data in Stanag 4607 packet format and return 
%% a structured representation i.e. a list of packets with nested segments
%% as appropriate.
decode(Bin) ->
    decode_packets(Bin, []).

decode_packets(<<>>, Acc) ->
    lists:reverse(Acc);
decode_packets(Bin, Acc) ->
    {ok, Hdr, R1} = extract_packet_header(Bin),
    H1 = s4607:decode_packet_header(Hdr),
    
    % The size in the header includes the header itself.
    PayloadSize = H1#pheader.packet_size - byte_size(Hdr),
    
    % Get the packet data payload.
    {ok, PktData, R2} = extract_packet_data(R1, PayloadSize),

    % Loop through all the segments in the packet.
    SegRecList = decode_segments(PktData, []),

    % Build the packet structure.
    Pkt = #packet{header = H1, segments = SegRecList}, 

    % Loop over any remaining packets, adding each to the list. 
    decode_packets(R2, [Pkt|Acc]).

decode_segments(<<>>, Acc) ->
    lists:reverse(Acc);
decode_segments(Bin, Acc) ->
    % Get the segment header.
    {ok, SegHdr, SRem} = extract_segment_header(Bin),
    SH = seg_header:decode(SegHdr),

    % The size in the header includes the header itself.
    PayloadSize = seg_header:get_segment_size(SH) - byte_size(SegHdr),

    % Get the packet data payload.
    {ok, SegData, SRem2} = extract_segment_data(SRem, PayloadSize),

    % Switch on the segment type
    case seg_header:get_segment_type(SH) of
        mission -> 
            SegRec = {ok, SH, mission:decode(SegData)};
        dwell   ->
            SegRec = {ok, SH, decode_dwell_segment(SegData)};
        job_definition ->
            SegRec = {ok, SH, job_def:decode(SegData)};
        _       -> 
            SegRec = {unknown_segment, SH, SegData}
    end,

    % Loop over any remaining segments contained in this packet.
    decode_segments(SRem2, [SegRec|Acc]).

%display_packets2(PktLst) ->
%    lists:map(fun display_packet/1, PktList).

%display_packet(Pkt) ->
    
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
    SH = seg_header:decode(SegHdr),
    seg_header:display(SH),
    io:format("~n"),

    % The size in the header includes the header itself.
    PayloadSize = seg_header:get_segment_size(SH) - byte_size(SegHdr),

    % Get the packet data payload.
    {ok, SegData, SRem2} = extract_segment_data(SRem, PayloadSize),

    % Switch on the segment type
    case seg_header:get_segment_type(SH) of
        mission -> 
            MS = mission:decode(SegData),
            mission:display(MS);
        dwell   ->
            DS = decode_dwell_segment(SegData),
            display_dwell_segment(DS);
        job_definition ->
            JD = job_def:decode(SegData),
            job_def:display(JD);
        _       -> 
            ok
    end,

    % Loop over any remaining segments contained in this packet.
    display_segments(SRem2).

%% Function to display a segment. Segment should have been decoded prior to 
%% calling this function.
display_segment(SegHdr, SegRec) ->
    seg_header:display(SegHdr),
    
    % Switch on the segment type and display the segment data.
    case seg_header:get_segment_type(SegHdr) of
        mission -> 
            mission:display(SegRec);
        dwell   ->
            display_dwell_segment(SegRec);
        job_definition ->
            job_def:display(SegRec); 
        _       -> 
            ok
    end. 

%% Extracts the first portion of the binary of the size required for a packet
%% header. Returns the unused portion to allow further processing.
extract_packet_header(<<Hdr:32/binary,Rest/binary>>) ->
    {ok, Hdr, Rest}.

%% Extracts the data payload from a packet from the supplied binary
%% (which should have had the header removed already).
extract_packet_data(Bin, Len) ->
    sutils:extract_data(Bin, Len).

%% Extracts the first binary portion associated with a segment header.
extract_segment_header(<<Hdr:5/binary,Rest/binary>>) ->
    {ok, Hdr, Rest}.

%% Extracts the segment payload from the supplied binary
%% (which should have had the header removed already).
extract_segment_data(Bin, Len) ->
    sutils:extract_data(Bin, Len).

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
    sutils:trim_trailing_spaces(binary_to_list(X)).

display_packet_header(PktHdr) ->
    io:format("Version: ~p~n", [get_version_id(PktHdr)]),
    io:format("Packet size: ~p~n", [get_packet_size(PktHdr)]), 
    io:format("Nationality: ~p~n", [get_nationality(PktHdr)]),
    io:format("Classification: ~p~n", [get_classification(PktHdr)]),
    io:format("Classification System: ~p~n", [get_class_system(PktHdr)]),
    io:format("Packet code: ~p~n", [get_packet_code(PktHdr)]),
    io:format("Exercise Indication: ~p~n", [get_exercise_indicator(PktHdr)]),
    io:format("Platform ID: ~p~n", [get_platform_id(PktHdr)]),
    io:format("Mission ID: ~p~n", [get_mission_id(PktHdr)]),
    io:format("Job ID: ~p~n", [get_job_id(PktHdr)]).

%% Get the version ID from a packet header
get_version_id(#pheader{version = V}) -> V.

%% Get the packet size from the header. 
get_packet_size(#pheader{packet_size = S}) -> S.

%% Get the nationality from a header structure.
get_nationality(#pheader{nationality = N}) -> N.

%% Get the classification level
get_classification(#pheader{classification = C}) -> C.

%% Get the classification system from the header. 
get_class_system(#pheader{class_system = X}) -> X.

%% Get the packet security code from the header.
get_packet_code(#pheader{packet_code = X}) -> X.

%% Get the exercise indicator from the header structure.
get_exercise_indicator(#pheader{exercise_ind = X}) -> X.

%% Get the platform ID from the header structure.
get_platform_id(#pheader{platform_id = X}) -> X.

%% Get the mission ID from the header structure.
get_mission_id(#pheader{mission_id = X}) -> X.

%% Get the job ID from the header structure.
get_job_id(#pheader{job_id = X}) -> X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dwell segment decoding functions.

decode_dwell_segment(<<EM:8/binary,RI:16/integer-unsigned-big,
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

display_dwell_segment(DS) ->
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


