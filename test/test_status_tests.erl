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

-module(test_status_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the test and status segment. 
test_status_test_() ->
    [decoding_checks()].

decoding_checks() ->
    Bin = sample_test_and_status_seg(),
    {ok, TSS} = test_status:decode(Bin),
    [?_assertEqual(5, test_status:get_job_id(TSS))]. 

%% Sample test and status segment.
%% Job ID: 5, revisit index: 256, dwell index: 133, dwell time: 1024,
%% all hardware and mode flags set.
sample_test_and_status_seg() ->
    <<0,0,0,5, 1,0, 1,5, 0,1,0,0, 16#F8, 16#F0>>.
