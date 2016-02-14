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
    encode_packet/1,
    new_packet/2,
    packet_payload_size/1,
    extract_packet_header/1,
    extract_packet_data/2,
    display_packets/1,
    display_packet/1,
    display_segments/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Record definitions.

-record(packet, {header, segments}).

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
    SegRecList = segment:decode_segments(PktData, []),

    % Build the packet structure.
    Pkt = #packet{header = H1, segments = SegRecList}, 

    % Loop over any remaining packets, adding each to the list. 
    decode_packets(R2, [Pkt|Acc]).

encode_packet(_Pkt) ->
    ok.

new_packet(PktHdr, SegList) ->
    #packet{header = PktHdr, segments = SegList}.

packet_payload_size(SegList) ->
    F = fun(Seg, Acc) ->
            SegHdr = segment:get_header(Seg),
            SegSize = segment:get_segment_size(SegHdr),
            Acc + SegSize
        end,

    lists:foldl(F, 0, SegList).
            
display_packet(#packet{header = H, segments = Slist}) ->
    pheader:display(H),
    display_segments(Slist).

%% Packet processing loop, prints out decoded information.
display_packets(PktLst) when is_list(PktLst) ->
    lists:map(fun display_packet/1, PktLst),
    ok.

%% Function to display a list of segments
display_segments(Slist) ->
    lists:map(fun segment:display/1, Slist),
    ok.
    
%% Extracts the first portion of the binary of the size required for a packet
%% header. Returns the unused portion to allow further processing.
extract_packet_header(<<Hdr:32/binary,Rest/binary>>) ->
    {ok, Hdr, Rest}.

%% Extracts the data payload from a packet from the supplied binary
%% (which should have had the header removed already).
extract_packet_data(Bin, Len) ->
    sutils:extract_data(Bin, Len).

%% Calculates the size of a segment from the supplied binary, adjusting for
%% the segment header. The value calculated should be used in the segment
%% header.
%calculate_segment_size(SegBin) when is_binary(SegBin) ->
%    SegHdrSize = 32,
%    SegBinSize = byte_size(SegBin),
%    SegHdrSize + SegBinSize.
      
