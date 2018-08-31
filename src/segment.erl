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
%% @doc Stanag 4607 packets are comprised of segments of various types. This 
%%      module contains generic segment handling functions. Code to handle 
%%      each specific type of segment is delegated to separate modules.

-module(segment).

-export([
    decode_segments/2,
    decode_segment/1,
    encode/1,
    new/2,
    new0/2,
    display/1,
    display/2,
    to_csv_iolist/1,
    update_segment_data/2,
    get_header/1,
    get_data/1]).

-record(segment, {header, data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

-type segment() :: #segment{}. 

%% @doc Decode a binary encoded list of segments contained within the payload 
%% of a packet.
-spec decode_segments(Bin::binary(), Acc::list()) -> [segment()].
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

%% Decode a single segment. Returns any left over binary data.
-spec decode_segment(Bin::binary()) -> Ret when
    _SecRec :: segment(),
    Ret :: {ok, _SegRec, Rem} | {error, Reason, Rem},
    Rem :: binary(),
    Reason :: atom().
decode_segment(Bin) -> 
    {error, not_implemented, Bin}.

%% @doc Create a binary encoded segment from a segment record.
-spec encode(Seg::segment()) -> 
    {ok, Bin::binary()} | {error, unsupported_segment_type}.
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

%% @doc Create a new segment record for the specified segment type.
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

%% @doc Function to display a segment passed as a segment record.
-spec display(Seg::segment()) -> ok.
display(#segment{header = H, data = D}) ->
    display(H, D).

%% @doc Function to display a segment passed as separate header and payload. 
%% Segment should have been decoded prior to calling this function.
display(SegHdr, SegRec) ->
    seg_header:display(SegHdr),

    % Extract the type and see if we know how to process it.
    SegType = seg_header:get_segment_type(SegHdr),
    case seg_type_to_module(SegType) of
        {ok, ModName} -> ModName:display(SegRec);
        Error         -> Error 
    end.

%% @doc Convert the segment to a CSV iolist. Not all segment types are 
%% supported at present.
-spec to_csv_iolist(Seg::segment() | [segment()]) -> iolist().
to_csv_iolist(#segment{header = H, data = D}) ->
    HdrIO = seg_header:to_csv_iolist(H),
    SegType = seg_header:get_segment_type(H),
    DataIO = case SegType of
                mission        -> mission:to_csv_iolist(D);
                dwell          -> dwell:to_csv_iolist(D);
                free_text      -> free_text:to_csv_iolist(D);
                job_definition -> job_def:to_csv_iolist(D);
                _              -> []
             end,
    HdrIO ++ DataIO;
to_csv_iolist(Segs) when is_list(Segs) ->
    lists:map(fun to_csv_iolist/1, Segs).

%% Extracts the first binary portion associated with a segment header.
extract_segment_header(<<Hdr:5/binary,Rest/binary>>) ->
    {ok, Hdr, Rest}.

%% Extracts the segment payload from the supplied binary
%% (which should have had the header removed already).
extract_segment_data(Bin, Len) ->
    sutils:extract_data(Bin, Len).

%% Update the data in a segment.
update_segment_data(#segment{header = SegHdr}, SegRec) ->
    SegType = seg_header:get_segment_type(SegHdr),
    new(SegType, SegRec).

%% Accessor functions.

%% @doc Retrieve the segment header from a segment structure.
get_header(#segment{header = H}) -> H.

%% @doc Retrieve the data payload from a segment structure.
get_data(#segment{data = D}) -> D.

%% Function to map segment types to the module containing the processing
%% functions. Before adding a segment to this list, please ensure that it
%% supports the functions used above, i.e.
%% decode/1, encode/1, payload_size/1, display/1.
seg_type_to_module(mission)         -> {ok, mission};
seg_type_to_module(dwell)           -> {ok, dwell};
seg_type_to_module(hrr)             -> {ok, hrr};
seg_type_to_module(job_definition)  -> {ok, job_def};
seg_type_to_module(free_text)       -> {ok, free_text};
seg_type_to_module(platform_loc)    -> {ok, platform_loc};
seg_type_to_module(job_request)     -> {ok, job_req};
seg_type_to_module(job_acknowledge) -> {ok, job_ack};
seg_type_to_module(_)               -> {error, unsupported_segment_type}.

