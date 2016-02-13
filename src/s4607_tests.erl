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

%% Define a test generator for the segment header decoding.
seg_header_test_() ->
    [seg_type_checks(), seg_size_checks()].

%% Function to check the decoding of fields in the segment header
seg_type_checks() ->
    {ok, SH1} = seg_header:decode(sample_seg_header1()),
    {ok, SH2} = seg_header:decode(sample_seg_header2()),
    {ok, SH3} = seg_header:decode(sample_seg_header3()),
    {ok, SHR4} = seg_header:decode(seg_header_res_4()),
    {ok, SHJD} = seg_header:decode(seg_header_job_def()),
    {ok, SHFT} = seg_header:decode(seg_header_free_text()),
    {ok, SHLR} = seg_header:decode(seg_header_low_refl()),
    {ok, SHGP} = seg_header:decode(seg_header_group()),
    {ok, SHAT} = seg_header:decode(seg_header_att_tar()),
    {ok, SHTS} = seg_header:decode(seg_header_test()),
    {ok, SHSS} = seg_header:decode(seg_header_sys_spec()),
    {ok, SHPH} = seg_header:decode(seg_header_proc_his()),
    {ok, SHPL} = seg_header:decode(seg_header_plat_loc()),
    {ok, SHR14} = seg_header:decode(seg_header_res_14()),
    {ok, SHR100} = seg_header:decode(seg_header_res_100()),
    {ok, SHJR} = seg_header:decode(seg_header_job_req()),
    {ok, SHJA} = seg_header:decode(seg_header_job_ack()),
    {ok, SHRF} = seg_header:decode(seg_header_res_fut()),
    {ok, SHRE} = seg_header:decode(seg_header_res_ext()),
    {ok, SHRE2} = seg_header:decode(seg_header_res_ext2()),
    [?_assertEqual(mission, seg_header:get_segment_type(SH1)),
     ?_assertEqual(dwell, seg_header:get_segment_type(SH2)),
     ?_assertEqual(hrr, seg_header:get_segment_type(SH3)),
     ?_assertEqual(reserved, seg_header:get_segment_type(SHR4)),
     ?_assertEqual(job_definition, seg_header:get_segment_type(SHJD)),
     ?_assertEqual(free_text, seg_header:get_segment_type(SHFT)),
     ?_assertEqual(low_reflectivity_index, seg_header:get_segment_type(SHLR)),
     ?_assertEqual(group, seg_header:get_segment_type(SHGP)),
     ?_assertEqual(attached_target, seg_header:get_segment_type(SHAT)),
     ?_assertEqual(test_and_status, seg_header:get_segment_type(SHTS)),
     ?_assertEqual(system_specific, seg_header:get_segment_type(SHSS)),
     ?_assertEqual(processing_history, seg_header:get_segment_type(SHPH)),
     ?_assertEqual(platform_location, seg_header:get_segment_type(SHPL)),
     ?_assertEqual(reserved, seg_header:get_segment_type(SHR14)),
     ?_assertEqual(reserved, seg_header:get_segment_type(SHR100)),
     ?_assertEqual(job_request, seg_header:get_segment_type(SHJR)),
     ?_assertEqual(job_acknowledge, seg_header:get_segment_type(SHJA)),
     ?_assertEqual(reserved, seg_header:get_segment_type(SHRF)),
     ?_assertEqual(reserved, seg_header:get_segment_type(SHRE)),
     ?_assertEqual(reserved, seg_header:get_segment_type(SHRE2))].

%% Function to check the decoding of the segment header size field.
seg_size_checks() ->
    {ok, SH1} = seg_header:decode(sample_seg_header1()),
    {ok, SH2} = seg_header:decode(sample_seg_header2()),
    {ok, SH3} = seg_header:decode(sample_seg_header3()),
    {ok, SHFT} = seg_header:decode(seg_header_free_text()),
    {ok, SHSS} = seg_header:decode(seg_header_sys_spec()),
    [?_assertEqual(8, seg_header:get_segment_size(SH1)),
     ?_assertEqual(20488, seg_header:get_segment_size(SH2)),
     ?_assertEqual(16777216, seg_header:get_segment_size(SH3)),
     ?_assertEqual(16777217, seg_header:get_segment_size(SHFT)),
     ?_assertEqual(33620225, seg_header:get_segment_size(SHSS))].

%% Test segment header data.
sample_seg_header1() -> <<1,0,0,0,8>>.
sample_seg_header2() -> <<2,0,0,80,8>>.
sample_seg_header3() -> <<3,1,0,0,0>>.
seg_header_res_4()   -> <<4,1,0,0,0>>.
seg_header_job_def() -> <<5,1,0,0,0>>.
seg_header_free_text() -> <<6,1,0,0,1>>.
seg_header_low_refl() -> <<7,1,1,1,1>>.
seg_header_group() -> <<8,2,1,1,1>>.
seg_header_att_tar() -> <<9,2,1,1,1>>.
seg_header_test() -> <<10,2,1,1,1>>.
seg_header_sys_spec() -> <<11,2,1,1,1>>.
seg_header_proc_his() -> <<12,2,1,1,1>>.
seg_header_plat_loc() -> <<13,2,1,1,1>>.
seg_header_res_14() -> <<14,2,1,1,1>>.
seg_header_res_100() -> <<100,2,1,1,1>>.
seg_header_job_req() -> <<101,1,0,0,0>>.
seg_header_job_ack() -> <<102,1,0,0,0>>.
seg_header_res_fut() -> <<103,1,0,0,0>>.
seg_header_res_ext() -> <<128,1,0,0,0>>.
seg_header_res_ext2() -> <<255,1,0,0,0>>.


