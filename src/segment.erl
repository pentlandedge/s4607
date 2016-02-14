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

%% Function to create an encoded segment from a segment record.
encode(#segment{header = SH, data = SegRec}) ->
    case seg_header:get_segment_type(SH) of
        mission -> 
            HdrBin = seg_header:encode(SH),
            DataBin = mission:encode(SegRec),
            {ok, <<HdrBin/binary,DataBin/binary>>};
        job_definition ->
            HdrBin = seg_header:encode(SH),
            DataBin = job_def:encode(SegRec),
            {ok, <<HdrBin/binary,DataBin/binary>>};
        dwell ->
            HdrBin = seg_header:encode(SH),
            DataBin = dwell:encode(SegRec),
            {ok, <<HdrBin/binary,DataBin/binary>>};
        _       ->
            {error, unsupported_segment_type}
    end.
 
%% Function to create a new segment record. 
%% Caller can either supply the segment header record or simply pass the 
%% segment type, and the function will create the segment header.
new(job_definition, SegRec) ->
    % Create a segment header.
    SegSize = seg_header:header_size() + job_def:payload_size(),
    SH = seg_header:new(job_definition, SegSize),

    % Create the complete segment record.
    new(SH, SegRec);

new(mission, SegRec) ->
    % Create a segment header.
    SegSize = seg_header:header_size() + mission:payload_size(),
    SH = seg_header:new(mission, SegSize),

    % Create the complete segment record.
    new(SH, SegRec);

new(dwell, SegRec) ->
    % Create a segment header.
    SegSize = seg_header:header_size() + dwell:payload_size(SegRec),
    SH = seg_header:new(dwell, SegSize),

    % Create the complete segment record.
    new(SH, SegRec);

%% Variant that takes a pre-constructed segment header.
new(SegHdr, SegRec) ->
    #segment{header = SegHdr, data = SegRec}.

%% Function to display a segment.
display(#segment{header = H, data = D}) ->
    display(H, D).

%% Function to display a segment. Segment should have been decoded prior to 
%% calling this function.
display(SegHdr, SegRec) ->
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

