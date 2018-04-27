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
    write_file/2,
    decode/1,
    encode_packets/1,
    encode_packet/1,
    new_packet/2,
    packet_payload_size/1,
    extract_packet_header/1,
    extract_packet_data/2,
    display_packets/1,
    display_packet/1,
    packet_to_csv_iolist/1,
    packets_to_csv_iolist/1,
    display_segments/1,
    get_packet_header/1,
    get_packet_segments/1,
    update_properties/2,
    packet_generator/1,
    get_segments/1,
    get_segment_types/1,
    get_segments_by_type/2,
    update_segments_in_packet/2,
    is_4607/1]).

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

%% Function to write a file in Stanag 4607 format. Assumes that the supplied
%% binary (which may be a list of binaries) is in the correct format.
write_file(File, BinList) ->
    file:write_file(File, BinList).

%% Function to decode binary data in Stanag 4607 packet format and return 
%% a structured representation i.e. a list of packets with nested segments
%% as appropriate.
decode(Bin) ->
    try decode_packets(Bin, [])
    catch
        _:_ -> {error, failed_decode}
    end.

decode_packets(<<>>, Acc) ->
    lists:reverse(Acc);
decode_packets(Bin, Acc) ->
    {ok, Hdr, R1} = extract_packet_header(Bin),
    {ok, H1} = pheader:decode(Hdr),
   
    % The size in the header includes the header itself.
    PayloadSize = pheader:get_packet_size(H1) - byte_size(Hdr),
   
    % Get the packet data payload.
    case extract_packet_data(R1, PayloadSize) of
        {ok, PktData, R2} ->
            % Loop through all the segments in the packet.
            SegRecList = segment:decode_segments(PktData, []),

            % Build the packet structure.
            Pkt = #packet{header = H1, segments = SegRecList}, 

            % Loop over any remaining packets, adding each to the list. 
            decode_packets(R2, [Pkt|Acc]);
        {error, _} ->
            io:format("Error: insufficient data in packet. Terminating decode.~n"),
            lists:reverse(Acc)
    end.

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

%% Function to convert a packet to CSV iolist.
packet_to_csv_iolist(#packet{header = H, segments = Slist}) ->
    pheader:to_csv_iolist(H) ++ segment:to_csv_iolist(Slist).

%% Function to convert a list of packets to a CSV iolist.
packets_to_csv_iolist(PktList) when is_list(PktList) ->
    lists:map(fun packet_to_csv_iolist/1, PktList).

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
extract_packet_data(Bin, Len) when byte_size(Bin) >= Len ->
    sutils:extract_data(Bin, Len);
extract_packet_data(_, _) ->
    {error, insufficient_data}.

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
   

%% Function to extract a list of segments from a list of packets.
get_segments(PacketList) when is_list(PacketList) ->
    % Local function to extract the segments from a single packet.
    F = fun(#packet{segments = Segs}) ->
            Segs
        end,

    % Extract a nested list of segments from the packet list.
    NestSegs = lists:map(F, PacketList),
    % Flatten the list.
    lists:flatten(NestSegs).

%% Function to extract a list of segment types present in a list of packets.
get_segment_types(PacketList) ->

    % Get a list of segments. If necessary, this step could be eliminated.
    SegList = get_segments(PacketList),

    % Extract the segment type and add it to a set. 
    F = fun(Seg, Set) ->
            SH = segment:get_header(Seg),
            T = seg_header:get_segment_type(SH),
            sets:add_element(T, Set)
        end,

    TypeSet = lists:foldl(F, sets:new(), SegList),
    sets:to_list(TypeSet).

%% Function to extract a list of segments of specified types from a list of
%% packets.
get_segments_by_type(SegTypes, PacketList) when is_list(SegTypes), 
    is_list(PacketList) ->

    % Get a list of segments. If necessary, this step could be eliminated.
    SegList = get_segments(PacketList),

    % Filter the list.
    F = fun(Seg) ->
            SH = segment:get_header(Seg),
            T = seg_header:get_segment_type(SH),
            lists:member(T, SegTypes)
        end,

    lists:filter(F, SegList).

%% @doc Update the list of segments in a packet and amend the packet header.
update_segments_in_packet(#packet{header = Hdr}, Segs) when is_list(Segs) ->
    % Work out the size of the packet.
    PaySize = s4607:packet_payload_size(Segs),
    Size = pheader:header_size() + PaySize,

    % Update the packet header.
    PktHdr = pheader:update_size(Hdr, Size),

    % Wrap the segments inside a new packet.
    s4607:new_packet(PktHdr, Segs).

%% @doc Test whether a binary looks like 4607 data. Attempts to decode the 
%% packet header.
is_4607(Bin) ->
    try 
        {ok, Hdr, _R1} = extract_packet_header(Bin),
        {ok, _H1} = pheader:decode(Hdr),
        true
    catch
        _:_ -> 
            false
    end.

