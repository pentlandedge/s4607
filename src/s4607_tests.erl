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

-module(s4607_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the decoding of the packet header fields. 
pheader_test_() ->
    [version_checks(), size_checks(), nationality_checks(), class_checks(), 
     class_sys_checks(), sec_code_checks(), exercise_ind_checks(), 
     platform_id_checks(), mission_id_checks(), job_id_checks()].

version_checks() ->
    Hdr1 = s4607:decode_packet_header(sample_header1()),
    [?_assertEqual({1,2}, s4607:get_version_id(Hdr1))].

nationality_checks() ->
    Hdr1 = s4607:decode_packet_header(sample_header1()),
    [?_assertEqual("UK", s4607:get_nationality(Hdr1))].

size_checks() ->
    Hdr1 = s4607:decode_packet_header(sample_header1()),
    [?_assertEqual(32, s4607:get_packet_size(Hdr1))].
    
class_checks() -> 
    Hdr1 = s4607:decode_packet_header(sample_header1()),
    [?_assertEqual(top_secret, s4607:get_classification(Hdr1))].

class_sys_checks() -> 
    Hdr1 = s4607:decode_packet_header(sample_header1()),
    [?_assertEqual("XN", s4607:get_class_system(Hdr1))].

sec_code_checks() -> 
    Hdr1 = s4607:decode_packet_header(sample_header1()),
    [?_assertEqual(nocontract, s4607:get_packet_code(Hdr1))].

exercise_ind_checks() ->
    Hdr1 = s4607:decode_packet_header(sample_header1()),
    [?_assertEqual(exercise_real, s4607:get_exercise_indicator(Hdr1))].

platform_id_checks() ->
    Hdr1 = s4607:decode_packet_header(sample_header1()),
    [?_assertEqual("ABCDEFGHIJ", s4607:get_platform_id(Hdr1))].

mission_id_checks() ->
    Hdr1 = s4607:decode_packet_header(sample_header1()),
    [?_assertEqual(5, s4607:get_mission_id(Hdr1))].
    
job_id_checks() ->
    Hdr1 = s4607:decode_packet_header(sample_header1()),
    [?_assertEqual(6, s4607:get_job_id(Hdr1))].

%% Sample packet header for test data.
sample_header1() ->
    <<"12",0,0,0,32, "UK", 1, "XN", 0, 1, 128, "ABCDEFGHIJ", 0, 0, 0, 5, 0, 0, 0, 6>>.

%% Define a test generator for the segment header decoding.
seg_header_test_() ->
    [seg_type_checks(), seg_size_checks()].

%% Function to check the decoding of fields in the segment header
seg_type_checks() ->
    SH1 = s4607:decode_segment_header(sample_seg_header1()),
    SH2 = s4607:decode_segment_header(sample_seg_header2()),
    SH3 = s4607:decode_segment_header(sample_seg_header3()),
    SHR4 = s4607:decode_segment_header(seg_header_res_4()),
    SHJD = s4607:decode_segment_header(seg_header_job_def()),
    SHJR = s4607:decode_segment_header(seg_header_job_req()),
    [?_assertEqual(mission, s4607:get_segment_type(SH1)),
     ?_assertEqual(dwell, s4607:get_segment_type(SH2)),
     ?_assertEqual(hrr, s4607:get_segment_type(SH3)),
     ?_assertEqual(reserved, s4607:get_segment_type(SHR4)),
     ?_assertEqual(job_definition, s4607:get_segment_type(SHJD)),
     ?_assertEqual(job_request, s4607:get_segment_type(SHJR))].

%% Function to check the decoding of the segment header size field.
seg_size_checks() ->
    SH1 = s4607:decode_segment_header(sample_seg_header1()),
    SH3 = s4607:decode_segment_header(sample_seg_header3()),
    [?_assertEqual(8, s4607:get_segment_size(SH1)),
     ?_assertEqual(16777216, s4607:get_segment_size(SH3))].

%% Test segment header data.
sample_seg_header1() -> <<1,0,0,0,8>>.
sample_seg_header2() -> <<2,0,0,80,8>>.
sample_seg_header3() -> <<3,1,0,0,0>>.
seg_header_res_4()   -> <<4,1,0,0,0>>.
seg_header_job_def() -> <<5,1,0,0,0>>.
seg_header_job_req() -> <<101,1,0,0,0>>.

