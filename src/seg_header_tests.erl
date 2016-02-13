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

-module(seg_header_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the decoding of the segment header. 
seg_header_test_() ->
    [header_check1(), header_check2(), new_checks()].

header_check1() ->
    Hdr = seg_header:decode(sample_seg_header1()),
    [?_assertEqual(free_text, seg_header:get_segment_type(Hdr)),
     ?_assertEqual(100, seg_header:get_segment_size(Hdr))].

header_check2() ->
    Hdr = seg_header:decode(sample_seg_header2()),
    [?_assertEqual(job_request, seg_header:get_segment_type(Hdr)),
     ?_assertEqual(16909060, seg_header:get_segment_size(Hdr))].

new_checks() ->
    SH = seg_header:new(job_definition, 100),
    [?_assertEqual(job_definition, seg_header:get_segment_type(SH)),
     ?_assertEqual(100, seg_header:get_segment_size(SH))].

sample_seg_header1() ->
    <<6,0,0,0,100>>.

sample_seg_header2() ->
    <<101,1,2,3,4>>.
