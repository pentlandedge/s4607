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
-module(segment).

-export([
    decode_segments/2,
    encode/1, 
    new/2, 
    display/1, 
    display/2, 
    get_header/1, 
    get_data/1]).

-record(segment, {header, data}).

%% Function to decode a list of segments contained within the payload of 
%% a packet.
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

    % Extract the type and see if we know how to process it. 
    SegType = seg_header:get_segment_type(SH), 
    case seg_type_to_module(SegType) of
        {ok, ModName} -> 
            {ok, SegRec} = ModName:decode(SegData),
            Seg = #segment{header = SH, data = SegRec};
        _ ->
            % Leave the data in binary form if we don't know how to decode it.
            Seg = #segment{header = SH, data = SegData}

    end,

    % Loop over any remaining segments contained in this packet.
    decode_segments(SRem2, [Seg|Acc]).

%% Function to create an encoded segment from a segment record.
encode(#segment{header = SH, data = SegRec}) ->
    % Extract the type and see if we know how to process it. 
    SegType = seg_header:get_segment_type(SH), 
    case seg_type_to_module(SegType) of
        {ok, ModName} -> 
            HdrBin = seg_header:encode(SH),
            DataBin = ModName:encode(SegRec),
            {ok, <<HdrBin/binary,DataBin/binary>>};
        _ ->
            {error, unsupported_segment_type}
    end.
 
%% Function to create a new segment record for the specified segment type.
new(SegType, SegRec) ->
    % Check we support the segment type then build it.
    {ok, _} = seg_type_to_module(SegType),
    build_segment(SegType, SegRec).

%% Variant that takes a pre-constructed segment header.
new0(SegHdr, SegRec) ->
    #segment{header = SegHdr, data = SegRec}.

%% Helper function to build the segment.
build_segment(SegType, SegRec) when is_atom(SegType) ->
    % Create a segment header.
    SH = build_seg_header(SegType, SegRec),
    
    % Create the complete segment record.
    new0(SH, SegRec).
    
%% Helper function to construct the segment header. Calculates the size of 
%% the segment from the supplied segment record.
build_seg_header(SegType, SegRec) ->
    {ok, ModName} = seg_type_to_module(SegType),
    SegSize = seg_header:header_size() + ModName:payload_size(SegRec),
    seg_header:new(SegType, SegSize).

%% Function to display a segment.
display(#segment{header = H, data = D}) ->
    display(H, D).

%% Function to display a segment. Segment should have been decoded prior to 
%% calling this function.
display(SegHdr, SegRec) ->
    seg_header:display(SegHdr),
    
    % Extract the type and see if we know how to process it. 
    SegType = seg_header:get_segment_type(SegHdr), 
    case seg_type_to_module(SegType) of
        {ok, ModName} -> 
            ModName:display(SegRec);
        _ -> 
            ok
    end. 

%% Extracts the first binary portion associated with a segment header.
extract_segment_header(<<Hdr:5/binary,Rest/binary>>) ->
    {ok, Hdr, Rest}.

%% Extracts the segment payload from the supplied binary
%% (which should have had the header removed already).
extract_segment_data(Bin, Len) ->
    sutils:extract_data(Bin, Len).

%% Accessor functions.
get_header(#segment{header = H}) -> H.
get_data(#segment{data = D}) -> D.

%% Function to map segment types to the module containing the processing 
%% functions. Before adding a segment to this list, please ensure that it 
%% supports the functions used above, i.e.
%% decode/1, encode/1, payload_size/1, display/1.
seg_type_to_module(mission)         -> {ok, mission};
seg_type_to_module(dwell)           -> {ok, dwell};
seg_type_to_module(job_definition)  -> {ok, job_def};
seg_type_to_module(free_text)       -> {ok, free_text};
seg_type_to_module(_)               -> {error, unsupported_segment_type}.

