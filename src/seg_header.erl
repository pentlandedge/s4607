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
%% @doc This module provides functions for manipulating segment headers 
%%      defined in the Stanag 4607 standard.

-module(seg_header).

-export([
    decode/1,
    encode/1,
    new/2,
    header_size/0,
    display/1,
    decode_segment_type/1,
    get_segment_type/1,
    get_segment_size/1]).

-record(seg_header, {type, size}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Segment header decoding functions.

%% @doc Decode the segment header binary.
decode(<<S1, SegSize:32/integer-unsigned-big>>) ->
    SegType = decode_segment_type(S1),
    {ok, #seg_header{type = SegType, size = SegSize}}.

%% @doc Encode a segment header as a binary.
encode(#seg_header{type = T, size = S}) ->
    TypeBin = encode_segment_type(T),
    <<TypeBin/binary, S:32/integer-unsigned-big>>.

%% @doc Create a new segment header record.
new(Type, Size) ->
    #seg_header{type = Type, size = Size}.

%% @doc Return the size of the seg header in bytes.
header_size() -> 5.

%% @doc Decodes the segment type.
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
decode_segment_type(13) -> platform_loc;
decode_segment_type(101) -> job_request;
decode_segment_type(102) -> job_acknowledge;
decode_segment_type(_) -> reserved.

%% @doc Encode the segment type field as a binary.
encode_segment_type(T) ->
    Val = encode_type(T),
    <<Val>>.

%% Helper function with the type mappings.
encode_type(mission) -> 1;
encode_type(dwell) -> 2;
encode_type(hrr) -> 3;
encode_type(job_definition) -> 5;
encode_type(free_text) -> 6;
encode_type(low_reflectivity_index) -> 7;
encode_type(group) -> 8;
encode_type(attached_target) -> 9;
encode_type(test_and_status) -> 10;
encode_type(system_specific) -> 11;
encode_type(processing_history) -> 12;
encode_type(platform_loc) -> 13;
encode_type(job_request) -> 101;
encode_type(job_acknowledge) -> 102.

%% @doc Display the contents of a segment header.
display(SegHdr) ->
    io:format("****************************************~n"),
    io:format("** @seg_header~n"),
    io:format("Segment type: ~p~n", [get_segment_type(SegHdr)]),
    io:format("Segment size: ~p~n", [get_segment_size(SegHdr)]).

%% @doc Get the segment type from the seg header structure.
get_segment_type(#seg_header{type = T}) -> T.

%% @doc Get the segment size from the seg header structure.
get_segment_size(#seg_header{size = S}) -> S.

