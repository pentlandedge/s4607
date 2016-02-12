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

-module(pheader_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the decoding of the packet header fields. 
pheader_test_() ->
    [version_checks(), size_checks(), nationality_checks(), class_checks(), 
     class_sys_checks(), sec_code_checks(), exercise_ind_checks(), 
     platform_id_checks(), mission_id_checks(), job_id_checks(), 
     encode_decode_check1()].

version_checks() ->
    Hdr1 = pheader:decode(sample_header1()),
    [?_assertEqual({1,2}, pheader:get_version_id(Hdr1))].

nationality_checks() ->
    Hdr1 = pheader:decode(sample_header1()),
    [?_assertEqual("UK", pheader:get_nationality(Hdr1))].

size_checks() ->
    Hdr1 = pheader:decode(sample_header1()),
    [?_assertEqual(32, pheader:get_packet_size(Hdr1))].
    
class_checks() -> 
    Hdr1 = pheader:decode(sample_header1()),
    [?_assertEqual(top_secret, pheader:get_classification(Hdr1))].

class_sys_checks() -> 
    Hdr1 = pheader:decode(sample_header1()),
    [?_assertEqual("XN", pheader:get_class_system(Hdr1))].

sec_code_checks() -> 
    Hdr1 = pheader:decode(sample_header1()),
    [?_assertEqual(nocontract, pheader:get_packet_code(Hdr1))].

exercise_ind_checks() ->
    Hdr1 = pheader:decode(sample_header1()),
    [?_assertEqual(exercise_real, pheader:get_exercise_indicator(Hdr1))].

platform_id_checks() ->
    Hdr1 = pheader:decode(sample_header1()),
    [?_assertEqual("ABCDEFGHIJ", pheader:get_platform_id(Hdr1))].

mission_id_checks() ->
    Hdr1 = pheader:decode(sample_header1()),
    [?_assertEqual(5, pheader:get_mission_id(Hdr1))].
    
job_id_checks() ->
    Hdr1 = pheader:decode(sample_header1()),
    [?_assertEqual(6, pheader:get_job_id(Hdr1))].

encode_decode_check1() ->
    PL = [{version, {3, 1}}, {packet_size, 250}, {nationality, "UK"},
          {classification, top_secret}, {class_system, "UK"}, 
          {packet_code, rel_9_eyes}, {exercise_ind, operation_real},
          {platform_id, "Pico1"}, {mission_id, 16#11223344},
          {job_id, 16#55667788}],

    % Construct a new packet header from the parameter list.
    PH = pheader:new(PL),   

    % Encode the header into binary form.
    EPH = pheader:encode(PH),

    % Decode it again
    DEPH = pheader:decode(EPH),

    % Check the parameters in the decoded version are as expected.
    [?_assertEqual({3, 1}, pheader:get_version_id(DEPH)),
     ?_assertEqual(250, pheader:get_packet_size(DEPH)),
     ?_assertEqual("UK", pheader:get_nationality(DEPH)),
     ?_assertEqual(16#55667788, pheader:get_job_id(DEPH))].
    
%% Sample packet header for test data.
sample_header1() ->
    <<"12",0,0,0,32, "UK", 1, "XN", 0, 1, 128, "ABCDEFGHIJ", 0, 0, 0, 5, 0, 0, 0, 6>>.


