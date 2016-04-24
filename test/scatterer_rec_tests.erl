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

-module(scatterer_rec_tests).

-include_lib("eunit/include/eunit.hrl").

-export([sample_record1/0]).

%% Define a test generator for scatterer records.
scatterer_rec_test_() ->
    [creation_checks(), payloadsize_checks(), encode_decode_checks()].


%% Create a HRR Scatterer Record and check all the fields in the record
creation_checks() ->
    creation_checks1(),
    creation_checks2(),
    creation_checks3(),
    creation_checks4().

creation_checks1() ->
    {_,_,_, R1} = sample_record1(),

    [?_assertEqual(34, scatterer_rec:get_scatterer_magnitude(R1)),
     ?_assertEqual(0, scatterer_rec:get_scatterer_phase(R1)),
     ?_assertEqual(0, scatterer_rec:get_range_index(R1)),
     ?_assertEqual(0, scatterer_rec:get_doppler_index(R1))].

creation_checks2() ->
    {_,_,_, R2} = sample_record2(),

    [?_assertEqual(834, scatterer_rec:get_scatterer_magnitude(R2)),
     ?_assertEqual(133, scatterer_rec:get_scatterer_phase(R2)),
     ?_assertEqual(0, scatterer_rec:get_range_index(R2)),
     ?_assertEqual(0, scatterer_rec:get_doppler_index(R2))].

creation_checks3() ->
    {_,_,_, R3} = sample_record3(),

    [?_assertEqual(834, scatterer_rec:get_scatterer_magnitude(R3)),
     ?_assertEqual(1133, scatterer_rec:get_scatterer_phase(R3)),
     ?_assertEqual(2341, scatterer_rec:get_range_index(R3)),
     ?_assertEqual(0, scatterer_rec:get_doppler_index(R3))].

creation_checks4() ->
    {_,_,_, R4} = sample_record4(),

    [?_assertEqual(34, scatterer_rec:get_scatterer_magnitude(R4)),
     ?_assertEqual(133, scatterer_rec:get_scatterer_phase(R4)),
     ?_assertEqual(10320, scatterer_rec:get_range_index(R4)),
     ?_assertEqual(10432, scatterer_rec:get_doppler_index(R4))].

%% Checks of the encode/decode functions.
encode_decode_checks() ->
    encode_decode_checks1(),
    encode_decode_checks2(),
    encode_decode_checks3(),
    encode_decode_checks4().

encode_decode_checks1() ->
    % Create a sample record.
    {EM, SM, SP, R1} = sample_record1(),

    % Encode it
    Bin = scatterer_rec:encode(R1, EM, SM, SP),

    % Decode it again
    {ok, SR} = scatterer_rec:decode(Bin, EM, SM, SP),

    [?_assertEqual(34, scatterer_rec:get_scatterer_magnitude(SR)),
     ?_assertEqual(0, scatterer_rec:get_scatterer_phase(SR)),
     ?_assertEqual(0, scatterer_rec:get_range_index(SR)),
     ?_assertEqual(0, scatterer_rec:get_doppler_index(SR))].

encode_decode_checks2() ->
    % Create a sample record.
    {EM, SM, SP, R2} = sample_record2(),

    % Encode it
    Bin = scatterer_rec:encode(R2, EM, SM, SP),

    % Decode it again
    {ok, SR} = scatterer_rec:decode(Bin, EM, SM, SP),

    [?_assertEqual(834, scatterer_rec:get_scatterer_magnitude(SR)),
     ?_assertEqual(133, scatterer_rec:get_scatterer_phase(SR)),
     ?_assertEqual(0, scatterer_rec:get_range_index(SR)),
     ?_assertEqual(0, scatterer_rec:get_doppler_index(SR))].

encode_decode_checks3() ->
    % Create a sample record.
    {EM, SM, SP, R3} = sample_record3(),

    % Encode it
    Bin = scatterer_rec:encode(R3, EM, SM, SP),

    % Decode it again
    {ok, SR} = scatterer_rec:decode(Bin, EM, SM, SP),

    [?_assertEqual(834, scatterer_rec:get_scatterer_magnitude(SR)),
     ?_assertEqual(1133, scatterer_rec:get_scatterer_phase(SR)),
     ?_assertEqual(2341, scatterer_rec:get_range_index(SR)),
     ?_assertEqual(0, scatterer_rec:get_doppler_index(SR))].

encode_decode_checks4() ->
    % Create a sample record.
    {EM, SM, SP, R4} = sample_record4(),

    % Encode it
    Bin = scatterer_rec:encode(R4, EM, SM, SP),

    % Decode it again
    {ok, SR} = scatterer_rec:decode(Bin, EM, SM, SP),

    [?_assertEqual(34, scatterer_rec:get_scatterer_magnitude(SR)),
     ?_assertEqual(133, scatterer_rec:get_scatterer_phase(SR)),
     ?_assertEqual(10320, scatterer_rec:get_range_index(SR)),
     ?_assertEqual(10432, scatterer_rec:get_doppler_index(SR))].

%% Create a HRR Scatterer Record and check the payload size of the record
payloadsize_checks() ->
    payloadsize_checks1(),
    payloadsize_checks2(),
    payloadsize_checks3(),
    payloadsize_checks4().

payloadsize_checks1() ->
    {EM,SM,SP,_} = sample_record1(),

    [?_assertEqual(1, scatterer_rec:payload_size(EM, SM, SP))].

payloadsize_checks2() ->
    {EM,SM,SP,_} = sample_record2(),

    [?_assertEqual(3, scatterer_rec:payload_size(EM, SM, SP))].

payloadsize_checks3() ->
    {EM,SM,SP,_} = sample_record3(),

    [?_assertEqual(6, scatterer_rec:payload_size(EM, SM, SP))].

payloadsize_checks4() ->
    {EM,SM,SP,_} = sample_record4(),

    [?_assertEqual(7, scatterer_rec:payload_size(EM, SM, SP))].

%% Create a sample scatterer record. Sets mandatory field to a value.
sample_record1() ->
    Params = [{scatterer_magnitude, 34}],

    % Return a suitable existence mask as well as the actual record.
    EM = {1,0,0,0},
    SM = 1,
    SP = 0,
    {EM, SM, SP, scatterer_rec:new(Params)}.

%% Create a sample target report. Sets first two fields to a value.
sample_record2() ->
    Params = [{scatterer_magnitude, 834}, {scatterer_phase, 113}],

    % Return a suitable existence mask as well as the actual record.
    EM = {1,1,0,0},
    SM = 2,
    SP = 1,
    {EM, SM, SP, scatterer_rec:new(Params)}.

%% Create a sample target report. Sets first three fields to a value.
sample_record3() ->
    Params = [{scatterer_magnitude, 834}, {scatterer_phase, 1133},
              {range_index, 2341}],

    % Return a suitable existence mask as well as the actual record.
    EM = {1,1,1,0},
    SM = 2,
    SP = 2,
    {EM, SM, SP, scatterer_rec:new(Params)}.

%% Create a sample target report. Sets all fields to a value.
sample_record4() ->
    Params = [{scatterer_magnitude, 34}, {scatterer_phase, 133},
              {range_index, 10320}, {doppler_index, 10432}],

    % Return a suitable existence mask as well as the actual record.
    EM = {1,1,1,1},
    SM = 1,
    SP = 2,
    {EM, SM, SP, scatterer_rec:new(Params)}.

