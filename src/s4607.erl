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
    encode_packets/1,
    encode_packet/1,
    new_packet/2,
    packet_payload_size/1,
    extract_packet_header/1,
    extract_packet_data/2,
    display_packets/1,
    display_packet/1,
    display_segments/1,
    get_packet_header/1,
    get_packet_segments/1,
    update_properties/2,
    packet_generator/1]).

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

%% Function to encode a list of packets. Returns a list of binaries, does
%% not flatten them.
encode_packets(PktList) when is_list(PktList) ->
    lists:map(fun encode_packet/1, PktList).

%% Function to encode a packet as a binary.
encode_packet(#packet{header = H, segments = S}) ->
    HdrBin = pheader:encode(H),
    F = fun(Seg, Acc) ->
            {ok, SegBin} = segment:encode(Seg),
            <<Acc/binary, SegBin/binary>>
        end,

    % Fold up the packet header and the list of segments as a single 
    % binary. This is not strictly necessary:could leave as a list of 
    % binaries and let the IO routines flatten the data.
    lists:foldl(F, HdrBin, S).

%% Function to create a new packet structure.
new_packet(PktHdr, SegList) ->
    #packet{header = PktHdr, segments = SegList}.

%% Function to calculate the expected size of a list of segments after 
%% encoding.
packet_payload_size(SegList) ->
    F = fun(Seg, Acc) ->
            SegHdr = segment:get_header(Seg),
            SegSize = seg_header:get_segment_size(SegHdr),
            Acc + SegSize
        end,

    lists:foldl(F, 0, SegList).
    
%% Function to display a packet.
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

%% Accessor functions for pulling out the fields of the packet.
get_packet_header(#packet{header = X}) -> X.
get_packet_segments(#packet{segments = X}) -> X.

%% Function to update a set of options in a property list.
update_properties(NewProperties, PropList) ->
    %% Local function to update a property in a list.
    D = fun({K,_V} = New, AccList) ->
            Reduced = proplists:delete(K, AccList),
            [New|Reduced]
        end,

    %% Update all the new settings in the list.
    lists:foldl(D, PropList, NewProperties).

%% Function that returns a function that will generate a packet from a list
%% of segments, patching the packet_size field in the packet header each
%% time it is called. The other parameters specified in HeaderParams will 
%% be used for each packet generated.
packet_generator(HeaderParams) ->
    fun(SegList) ->
        % Work out the size of the packet.
        PaySize = s4607:packet_payload_size(SegList),
        Size = pheader:header_size() + PaySize,

        % Update the size of the packet in the header parameters.
        HP = update_properties([{packet_size, Size}], HeaderParams),

        % Create a packet header.
        PktHdr = pheader:new(HP),

        % Wrap the segments inside a packet.
        s4607:new_packet(PktHdr, SegList)
    end.
    
