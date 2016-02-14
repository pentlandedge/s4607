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
    decode_segments/2,
    display_packets/1,
    display_packet/1,
    display_segments/1,
    display_segment/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Record definitions.

-record(packet, {header, segments}).

-record(segment, {header, data}).

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
    {ok, H1} = pheader:decode(Hdr),
    
    % The size in the header includes the header itself.
    PayloadSize = pheader:get_packet_size(H1) - byte_size(Hdr),
    
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
    {ok, SH} = seg_header:decode(SegHdr),

    % The size in the header includes the header itself.
    PayloadSize = seg_header:get_segment_size(SH) - byte_size(SegHdr),

    % Get the packet data payload.
    {ok, SegData, SRem2} = extract_segment_data(SRem, PayloadSize),

    % Switch on the segment type
    case seg_header:get_segment_type(SH) of
        mission -> 
            {ok, SegRec} = mission:decode(SegData),
            Seg = #segment{header = SH, data = SegRec};
        dwell   ->
            {ok, SegRec} = dwell:decode(SegData),
            Seg = #segment{header = SH, data = SegRec};
        job_definition ->
            {ok, SegRec} = job_def:decode(SegData),
            Seg = #segment{header = SH, data = SegRec};
        _       ->
            % Leave the data in binary form if we don't know how to decode it.
            Seg = #segment{header = SH, data = SegData}

    end,

    % Loop over any remaining segments contained in this packet.
    decode_segments(SRem2, [Seg|Acc]).

display_packet(#packet{header = H, segments = Slist}) ->
    pheader:display(H),
    display_segments(Slist).

%% Packet processing loop, prints out decoded information.
display_packets(PktLst) when is_list(PktLst) ->
    lists:map(fun display_packet/1, PktLst),
    ok.

%% Function to display a list of segments
display_segments(Slist) ->
    lists:map(fun display_segment/1, Slist),
    ok.
    
%% Function to display a segment.
display_segment(#segment{header = H, data = D}) ->
    display_segment(H, D).

%% Function to display a segment. Segment should have been decoded prior to 
%% calling this function.
display_segment(SegHdr, SegRec) ->
    seg_header:display(SegHdr),
    
    % Switch on the segment type and display the segment data.
    case seg_header:get_segment_type(SegHdr) of
        mission -> 
            mission:display(SegRec);
        dwell   ->
            dwell:display(SegRec);
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

%% Calculates the size of a segment from the supplied binary, adjusting for
%% the segment header. The value calculated should be used in the segment
%% header.
%calculate_segment_size(SegBin) when is_binary(SegBin) ->
%    SegHdrSize = 32,
%    SegBinSize = byte_size(SegBin),
%    SegHdrSize + SegBinSize.
      
