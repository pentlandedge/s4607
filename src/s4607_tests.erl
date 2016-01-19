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

%% Define a test generator for the unsigned integer types.
pheader_test_() ->
    [version_checks(), size_checks(), nationality_checks(), class_checks(), 
     class_sys_checks(), sec_code_checks()].

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

%% Sample packet header for test data.
sample_header1() ->
    <<"12",0,0,0,32, "UK", 1, "XN", 0, 1, 128, "ABCDEFGHIJ", 0, 0, 0, 5, 0, 0, 0, 6>>.
