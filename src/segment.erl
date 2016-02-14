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

-export([encode/1, new/2]).

-record(segment, {header, data}).

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
