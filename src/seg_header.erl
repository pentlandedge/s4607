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
-module(seg_header).

-export([
    decode/1, 
    display/1, 
    decode_segment_type/1,
    get_segment_type/1,
    get_segment_size/1]).

-record(seg_header, {type, size}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Segment header decoding functions.

%% Decode the segment header binary. 
decode(<<S1, SegSize:32/integer-unsigned-big>>) ->
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

display(SegHdr) ->
    io:format("****************************************~n"),
    io:format("** @seg_header~n"),
    io:format("Segment type: ~p~n", [get_segment_type(SegHdr)]),
    io:format("Segment size: ~p~n", [get_segment_size(SegHdr)]).

%% Function to get the segment type from the seg header structure.
get_segment_type(#seg_header{type = T}) -> T.

%% Function to get the segment size from the seg header structure.
get_segment_size(#seg_header{size = S}) -> S.

