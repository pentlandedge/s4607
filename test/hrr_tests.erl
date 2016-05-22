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

-module(hrr_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the decoding of the hrr segment.
hrr_test_() ->
    [creation_checks1()].

creation_checks1() ->
    HRR = minimal_hrr(),
    [?_assertEqual(<<16#7:3, 0:3, 1:1, 0:1,16#1F:5, 0:1, 16#F:4,
            0:3, 16#F:4, 0:5, 1:1, 0:3, 0:6>>, hrr:get_existence_mask(HRR)),
     ?_assertEqual(100, hrr:get_revisit_index(HRR)),
     ?_assertEqual(20000, hrr:get_dwell_index(HRR)),
     ?_assertEqual(no_additional_dwells, hrr:get_last_dwell_of_revisit(HRR)),
     ?_assertEqual(0, hrr:get_num_of_target_scatterers(HRR)),
     ?_assertEqual(0, hrr:get_num_of_doppler_samples(HRR)),
     ?_assertEqual(100, hrr:get_detection_threshold(HRR)),
     ?_assertEqual(45.16, hrr:get_range_resolution(HRR)),
     ?_assertEqual(99.99, hrr:get_range_bin_spacing(HRR)),
     ?_assertEqual(1000.50, hrr:get_doppler_resolution(HRR)),
     ?_assertEqual(123.456, hrr:get_doppler_bin_spacing(HRR)),
     ?_assertEqual(no_compression, hrr:get_compression_flag(HRR)),
     ?_assertEqual(no_statement, hrr:get_range_weighting_type(HRR)),
     ?_assertEqual(taylor_weighting, hrr:get_doppler_weighting_type(HRR)),
     ?_assertEqual(-100.0, hrr:get_maximum_pixel_power(HRR)),
     ?_assertEqual(other, hrr:get_type_of_hrr(HRR)),
     ?_assertEqual(0, hrr:get_processing_mask(HRR)),
     ?_assertEqual(1, hrr:get_num_bytes_magnitude(HRR)),
     ?_assertEqual(0, hrr:get_num_bytes_phase(HRR)),
     ?_assertEqual([], hrr:get_hrr_scatter_records(HRR))].

%% Create a sample HRR Segment with only the mandatory fields set.
minimal_hrr() ->
    % Create a list of fields for the existence mask.
    EM = <<16#7:3, 0:3, 1:1, 0:1,16#1F:5, 0:1, 16#F:4,
            0:3, 16#F:4, 0:5, 1:1, 0:3, 0:6>>,

    % Set the fields of the dwell segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000},
         {last_dwell_of_revisit, no_additional_dwells},
         {num_of_target_scatterers, 0}, {num_of_doppler_samples, 0},
         {detection_threshold, 100}, {range_resolution, 45.16},
         {range_bin_spacing, 99.99}, {doppler_resolution, 1000.50},
         {doppler_bin_spacing, 123.456}, {compression_flag, no_compression},
         {range_weighting_type, no_statement},
         {doppler_weighting_type, taylor_weighting},
         {maximum_pixel_power, -100.0}, {type_of_hrr, other},
         {processing_mask, 0},
         {num_bytes_magnitude, 1}, {num_bytes_phase, 0}],

    % Use the parameters to construct a new dwell segment.
    hrr:new(P).

%% Utility function to compare whether floating point values are within a
%% specified range.
%% almost_equal(V1, V2, Delta) ->
%%    abs(V1 - V2) =< Delta.
