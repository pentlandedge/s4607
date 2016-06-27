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
    [creation_checks(), encode_decode_checks()].

creation_checks() ->
    creation_checks1(),
    creation_checks2(),
    creation_checks3().

creation_checks1() ->
    {HRR, PM} = minimal_hrr(),

    % Display the segment
    hrr:display(HRR),

    EM = hrr:get_existence_mask(HRR),

    [?_assertEqual(1, hrr_exist_mask:get_revisit_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_dwell_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_last_dwell_of_revisit(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_target_scatterers(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_doppler_samples(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_detection_threshold(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_compression_flag(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_maximum_pixel_power(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_type_of_hrr(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_processing_mask(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_magnitude(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_phase(EM)),

     ?_assertEqual(0, hrr_exist_mask:get_num_of_range_samples(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_center_frequency(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_maximum_rcs(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_doppler_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_extent_pixels(EM)),

     ?_assertEqual(36, hrr:payload_size(HRR)),

     ?_assertEqual(100, hrr:get_revisit_index(HRR)),
     ?_assertEqual(20000, hrr:get_dwell_index(HRR)),
     ?_assertEqual(no_additional_dwells, hrr:get_last_dwell_of_revisit(HRR)),
     ?_assertEqual(0, hrr:get_num_of_target_scatterers(HRR)),
     ?_assertEqual(0, hrr:get_num_of_doppler_samples(HRR)),
     ?_assertEqual(100, hrr:get_detection_threshold(HRR)),
     ?_assert(almost_equal(45.16, hrr:get_range_resolution(HRR), 0.01)),
     ?_assert(almost_equal(99.99, hrr:get_range_bin_spacing(HRR), 0.01)),
     ?_assert(almost_equal(1000.50, hrr:get_doppler_resolution(HRR), 0.001)),
     ?_assert(almost_equal(123.456, hrr:get_doppler_bin_spacing(HRR), 0.001)),
     ?_assertEqual(no_compression, hrr:get_compression_flag(HRR)),
     ?_assertEqual(no_statement, hrr:get_range_weighting_type(HRR)),
     ?_assertEqual(taylor_weighting, hrr:get_doppler_weighting_type(HRR)),
     ?_assert(almost_equal(-100.0, hrr:get_maximum_pixel_power(HRR), 0.01)),
     ?_assertEqual(other, hrr:get_type_of_hrr(HRR)),
     ?_assertEqual(PM, hrr:get_processing_mask(HRR)),
     ?_assertEqual(1, hrr:get_num_bytes_magnitude(HRR)),
     ?_assertEqual(0, hrr:get_num_bytes_phase(HRR)),
     ?_assertEqual([], hrr:get_hrr_scatter_records(HRR))].

creation_checks2() ->
    {HRR, PM} = sample_hrr1(),

    % Display the segment
    hrr:display(HRR),

    EM = hrr:get_existence_mask(HRR),

    [?_assertEqual(1, hrr_exist_mask:get_revisit_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_dwell_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_last_dwell_of_revisit(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_target_scatterers(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_doppler_samples(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_detection_threshold(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_compression_flag(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_maximum_pixel_power(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_type_of_hrr(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_processing_mask(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_magnitude(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_phase(EM)),

     ?_assertEqual(1, hrr_exist_mask:get_num_of_range_samples(EM)),

     ?_assertEqual(0, hrr_exist_mask:get_center_frequency(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_maximum_rcs(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_doppler_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_extent_pixels(EM)),

     ?_assertEqual(45, hrr:payload_size(HRR)),

     ?_assertEqual(100, hrr:get_revisit_index(HRR)),
     ?_assertEqual(20000, hrr:get_dwell_index(HRR)),
     ?_assertEqual(no_additional_dwells, hrr:get_last_dwell_of_revisit(HRR)),
     ?_assertEqual(0, hrr:get_num_of_target_scatterers(HRR)),
     ?_assertEqual(0, hrr:get_num_of_doppler_samples(HRR)),
     ?_assertEqual(100, hrr:get_detection_threshold(HRR)),
     ?_assert(almost_equal(45.16, hrr:get_range_resolution(HRR), 0.01)),
     ?_assert(almost_equal(99.99, hrr:get_range_bin_spacing(HRR), 0.01)),
     ?_assert(almost_equal(1000.50, hrr:get_doppler_resolution(HRR), 0.001)),
     ?_assert(almost_equal(123.456, hrr:get_doppler_bin_spacing(HRR), 0.001)),
     ?_assertEqual(threshold_decomposition_x10, hrr:get_compression_flag(HRR)),
     ?_assertEqual(taylor_weighting, hrr:get_range_weighting_type(HRR)),
     ?_assertEqual(other, hrr:get_doppler_weighting_type(HRR)),
     ?_assert(almost_equal(-100.0, hrr:get_maximum_pixel_power(HRR), 0.01)),
     ?_assertEqual(sparse_hrr_chip, hrr:get_type_of_hrr(HRR)),
     ?_assertEqual(PM, hrr:get_processing_mask(HRR)),
     ?_assertEqual(1, hrr:get_num_bytes_magnitude(HRR)),
     ?_assertEqual(2, hrr:get_num_bytes_phase(HRR)),
     ?_assertEqual([{hrr_scatterer_record,34,133,10320,10432}], hrr:get_hrr_scatter_records(HRR))].

creation_checks3() ->
    {HRR, PM} = sample_hrr2(),

    % Display the segment
    hrr:display(HRR),

    EM = hrr:get_existence_mask(HRR),

    [?_assertEqual(1, hrr_exist_mask:get_revisit_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_dwell_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_last_dwell_of_revisit(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_num_of_target_scatterers(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_doppler_samples(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_detection_threshold(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_compression_flag(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_maximum_pixel_power(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_type_of_hrr(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_processing_mask(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_magnitude(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_phase(EM)),

     ?_assertEqual(1, hrr_exist_mask:get_num_of_range_samples(EM)),

     ?_assertEqual(0, hrr_exist_mask:get_center_frequency(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_maximum_rcs(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_doppler_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_extent_pixels(EM)),

     % Payload is 36 + 7 for each of the two Scatterer Records
     ?_assertEqual(50, hrr:payload_size(HRR)),

     ?_assertEqual(100, hrr:get_revisit_index(HRR)),
     ?_assertEqual(20000, hrr:get_dwell_index(HRR)),
     ?_assertEqual(additional_dwells, hrr:get_last_dwell_of_revisit(HRR)),
     ?_assertEqual(2, hrr:get_num_of_range_samples(HRR)),
     ?_assertEqual(1, hrr:get_num_of_doppler_samples(HRR)),
     ?_assertEqual(100, hrr:get_detection_threshold(HRR)),
     ?_assert(almost_equal(45.16, hrr:get_range_resolution(HRR), 0.01)),
     ?_assert(almost_equal(99.99, hrr:get_range_bin_spacing(HRR), 0.01)),
     ?_assert(almost_equal(1000.50, hrr:get_doppler_resolution(HRR), 0.001)),
     ?_assert(almost_equal(123.456, hrr:get_doppler_bin_spacing(HRR), 0.001)),
     ?_assertEqual(threshold_decomposition_x10, hrr:get_compression_flag(HRR)),
     ?_assertEqual(other, hrr:get_range_weighting_type(HRR)),
     ?_assertEqual(no_statement, hrr:get_doppler_weighting_type(HRR)),
     ?_assert(almost_equal(-100.0, hrr:get_maximum_pixel_power(HRR), 0.01)),
     ?_assertEqual(oversized_hrr_chip, hrr:get_type_of_hrr(HRR)),
     ?_assertEqual(PM, hrr:get_processing_mask(HRR)),
     ?_assertEqual(1, hrr:get_num_bytes_magnitude(HRR)),
     ?_assertEqual(2, hrr:get_num_bytes_phase(HRR)),
     ?_assertEqual([{hrr_scatterer_record,34,133,10320,10432},
                    {hrr_scatterer_record,34,133,10320,10432}],
                    hrr:get_hrr_scatter_records(HRR))].

encode_decode_checks() ->
    encode_decode_checks1(),
    encode_decode_checks2(),
    encode_decode_checks3().

encode_decode_checks1() ->
    {HRR, PM} = minimal_hrr(),
    HRREncoded = hrr:encode(HRR),
    {ok, HRRDecoded} = hrr:decode(HRREncoded),
    EM = hrr:get_existence_mask(HRR),

    [?_assertEqual(1, hrr_exist_mask:get_revisit_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_dwell_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_last_dwell_of_revisit(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_target_scatterers(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_doppler_samples(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_detection_threshold(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_compression_flag(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_maximum_pixel_power(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_type_of_hrr(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_processing_mask(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_magnitude(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_phase(EM)),

     ?_assertEqual(0, hrr_exist_mask:get_num_of_range_samples(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_center_frequency(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_maximum_rcs(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_doppler_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_extent_pixels(EM)),

     ?_assertEqual(100, hrr:get_revisit_index(HRRDecoded)),
     ?_assertEqual(20000, hrr:get_dwell_index(HRRDecoded)),
     ?_assertEqual(no_additional_dwells, hrr:get_last_dwell_of_revisit(HRRDecoded)),
     ?_assertEqual(0, hrr:get_num_of_target_scatterers(HRRDecoded)),
     ?_assertEqual(0, hrr:get_num_of_doppler_samples(HRRDecoded)),
     ?_assertEqual(100, hrr:get_detection_threshold(HRRDecoded)),
     ?_assert(almost_equal(45.16, hrr:get_range_resolution(HRRDecoded), 0.01)),
     ?_assert(almost_equal(99.99, hrr:get_range_bin_spacing(HRRDecoded), 0.01)),
     ?_assert(almost_equal(1000.50, hrr:get_doppler_resolution(HRRDecoded), 0.001)),
     ?_assert(almost_equal(123.456, hrr:get_doppler_bin_spacing(HRRDecoded), 0.001)),
     ?_assertEqual(no_compression, hrr:get_compression_flag(HRRDecoded)),
     ?_assertEqual(no_statement, hrr:get_range_weighting_type(HRRDecoded)),
     ?_assertEqual(taylor_weighting, hrr:get_doppler_weighting_type(HRRDecoded)),
     ?_assert(almost_equal(-100.0, hrr:get_maximum_pixel_power(HRRDecoded), 0.01)),
     ?_assertEqual(other, hrr:get_type_of_hrr(HRRDecoded)),
     ?_assertEqual(PM, hrr:get_processing_mask(HRRDecoded)),
     ?_assertEqual(1, hrr:get_num_bytes_magnitude(HRRDecoded)),
     ?_assertEqual(0, hrr:get_num_bytes_phase(HRRDecoded)),
     ?_assertEqual([], hrr:get_hrr_scatter_records(HRRDecoded))].

encode_decode_checks2() ->
    {HRR, PM} = sample_hrr1(),
    HRREncoded = hrr:encode(HRR),
    {ok, HRRDecoded} = hrr:decode(HRREncoded),
    EM = hrr:get_existence_mask(HRR),

    [?_assertEqual(1, hrr_exist_mask:get_revisit_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_dwell_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_last_dwell_of_revisit(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_target_scatterers(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_doppler_samples(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_detection_threshold(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_compression_flag(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_maximum_pixel_power(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_type_of_hrr(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_processing_mask(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_magnitude(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_phase(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_range_samples(EM)),

     ?_assertEqual(0, hrr_exist_mask:get_center_frequency(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_maximum_rcs(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_doppler_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_extent_pixels(EM)),

     ?_assertEqual(100, hrr:get_revisit_index(HRRDecoded)),
     ?_assertEqual(20000, hrr:get_dwell_index(HRRDecoded)),
     ?_assertEqual(no_additional_dwells, hrr:get_last_dwell_of_revisit(HRRDecoded)),
     ?_assertEqual(0, hrr:get_num_of_target_scatterers(HRRDecoded)),
     ?_assertEqual(0, hrr:get_num_of_doppler_samples(HRRDecoded)),
     ?_assertEqual(100, hrr:get_detection_threshold(HRRDecoded)),
     ?_assert(almost_equal(45.16, hrr:get_range_resolution(HRRDecoded), 0.01)),
     ?_assert(almost_equal(99.99, hrr:get_range_bin_spacing(HRRDecoded), 0.01)),
     ?_assert(almost_equal(1000.50, hrr:get_doppler_resolution(HRRDecoded), 0.001)),
     ?_assert(almost_equal(123.456, hrr:get_doppler_bin_spacing(HRRDecoded), 0.001)),
     ?_assertEqual(threshold_decomposition_x10, hrr:get_compression_flag(HRRDecoded)),
     ?_assertEqual(taylor_weighting, hrr:get_range_weighting_type(HRRDecoded)),
     ?_assertEqual(other, hrr:get_doppler_weighting_type(HRRDecoded)),
     ?_assert(almost_equal(-100.0, hrr:get_maximum_pixel_power(HRRDecoded), 0.01)),
     ?_assertEqual(sparse_hrr_chip, hrr:get_type_of_hrr(HRRDecoded)),
     ?_assertEqual(PM, hrr:get_processing_mask(HRRDecoded)),
     ?_assertEqual(1, hrr:get_num_bytes_magnitude(HRRDecoded)),
     ?_assertEqual(2, hrr:get_num_bytes_phase(HRRDecoded)),
     ?_assertEqual([{hrr_scatterer_record,34,133,10320,10432}], hrr:get_hrr_scatter_records(HRRDecoded))].

encode_decode_checks3() ->
    {HRR, PM} = sample_hrr2(),
    HRREncoded = hrr:encode(HRR),
    {ok, HRRDecoded} = hrr:decode(HRREncoded),
    EM = hrr:get_existence_mask(HRR),

    [?_assertEqual(1, hrr_exist_mask:get_revisit_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_dwell_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_last_dwell_of_revisit(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_num_of_target_scatterers(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_doppler_samples(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_detection_threshold(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_resolution(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_bin_spacing(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_compression_flag(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_doppler_weighting_type(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_maximum_pixel_power(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_type_of_hrr(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_processing_mask(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_magnitude(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_bytes_phase(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_range_samples(EM)),

     ?_assertEqual(0, hrr_exist_mask:get_center_frequency(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_maximum_rcs(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_doppler_of_origin(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_extent_pixels(EM)),

     ?_assertEqual(100, hrr:get_revisit_index(HRRDecoded)),
     ?_assertEqual(20000, hrr:get_dwell_index(HRRDecoded)),
     ?_assertEqual(additional_dwells, hrr:get_last_dwell_of_revisit(HRRDecoded)),
     ?_assertEqual(2, hrr:get_num_of_range_samples(HRRDecoded)),
     ?_assertEqual(1, hrr:get_num_of_doppler_samples(HRRDecoded)),
     ?_assertEqual(100, hrr:get_detection_threshold(HRRDecoded)),
     ?_assert(almost_equal(45.16, hrr:get_range_resolution(HRRDecoded), 0.01)),
     ?_assert(almost_equal(99.99, hrr:get_range_bin_spacing(HRRDecoded), 0.01)),
     ?_assert(almost_equal(1000.50, hrr:get_doppler_resolution(HRRDecoded), 0.001)),
     ?_assert(almost_equal(123.456, hrr:get_doppler_bin_spacing(HRRDecoded), 0.001)),
     ?_assertEqual(threshold_decomposition_x10, hrr:get_compression_flag(HRRDecoded)),
     ?_assertEqual(other, hrr:get_range_weighting_type(HRRDecoded)),
     ?_assertEqual(no_statement, hrr:get_doppler_weighting_type(HRRDecoded)),
     ?_assert(almost_equal(-100.0, hrr:get_maximum_pixel_power(HRRDecoded), 0.01)),
     ?_assertEqual(oversized_hrr_chip, hrr:get_type_of_hrr(HRRDecoded)),
     ?_assertEqual(PM, hrr:get_processing_mask(HRRDecoded)),
     ?_assertEqual(1, hrr:get_num_bytes_magnitude(HRRDecoded)),
     ?_assertEqual(2, hrr:get_num_bytes_phase(HRRDecoded)),
     ?_assertEqual([{hrr_scatterer_record,34,133,10320,10432},
                    {hrr_scatterer_record,34,133,10320,10432}],
                    hrr:get_hrr_scatter_records(HRRDecoded))].

%% Create a sample HRR Segment with only the mandatory fields set.
minimal_hrr() ->
    % Create a list of fields for the existence mask.
    F = [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit,
         num_of_target_scatterers, num_of_doppler_samples, detection_threshold,
         range_resolution,
         range_bin_spacing, doppler_resolution, doppler_bin_spacing,
         compression_flag, range_weighting_type, doppler_weighting_type,
         maximum_pixel_power, type_of_hrr, processing_mask, num_bytes_magnitude, num_bytes_phase],

    % Create an existence mask.
    EM = hrr_exist_mask:new(F),

    % Create a processing mask.
    PM = hrr:new_processing_mask(0,0,0),

    % Set the fields of the HRR segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000},
         {last_dwell_of_revisit, no_additional_dwells},
         {num_of_target_scatterers, 0}, {num_of_doppler_samples, 0},
         {detection_threshold, 100}, {range_resolution, 45.16},
         {range_bin_spacing, 99.99}, {doppler_resolution, 1000.50},
         {doppler_bin_spacing, 123.456}, {compression_flag, no_compression},
         {range_weighting_type, no_statement},
         {doppler_weighting_type, taylor_weighting},
         {maximum_pixel_power, -100.0}, {type_of_hrr, other},
         {processing_mask, PM},
         {num_bytes_magnitude, 1}, {num_bytes_phase, 0}],

    % Use the parameters to construct a new HRR segment.
    {hrr:new(P), PM}.

sample_hrr1() ->
    % Create a list of fields for the existence mask.
    F = [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit,
         num_of_target_scatterers, num_of_range_samples, num_of_doppler_samples, detection_threshold,
         range_resolution,
         range_bin_spacing, doppler_resolution, doppler_bin_spacing,
         compression_flag, range_weighting_type, doppler_weighting_type,
         maximum_pixel_power, type_of_hrr, processing_mask, num_bytes_magnitude, num_bytes_phase,
         scatterer_magnitude, scatterer_phase, range_index, doppler_index],

    % Create an existence mask.
    EM = hrr_exist_mask:new(F),

    % Create a scatterer record.
    {_, SM, SP, SR} = sample_scatter_record(),

    % Create a processing mask.
    PM = hrr:new_processing_mask(0,0,0),

    % Set the fields of the HRR segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000},
         {last_dwell_of_revisit, no_additional_dwells},
         {num_of_target_scatterers, 0}, {num_of_range_samples, 1},
         {num_of_doppler_samples, 0},
         {detection_threshold, 100}, {range_resolution, 45.16},
         {range_bin_spacing, 99.99}, {doppler_resolution, 1000.50},
         {doppler_bin_spacing, 123.456}, {compression_flag, threshold_decomposition_x10},
         {range_weighting_type, taylor_weighting},
         {doppler_weighting_type, other},
         {maximum_pixel_power, -100.0}, {type_of_hrr, sparse_hrr_chip},
         {processing_mask, PM},
         {num_bytes_magnitude, SM}, {num_bytes_phase, SP},
         {hrr_scatter_records, [SR]}],

    % Use the parameters to construct a new HRR segment.
    {hrr:new(P), PM}.

sample_hrr2() ->
    % Create a list of fields for the existence mask.
    F = [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit,
         num_of_range_samples, num_of_doppler_samples, detection_threshold,
         range_resolution,
         range_bin_spacing, doppler_resolution, doppler_bin_spacing,
         compression_flag, range_weighting_type, doppler_weighting_type,
         maximum_pixel_power, type_of_hrr, processing_mask, num_bytes_magnitude, num_bytes_phase,
         scatterer_magnitude, scatterer_phase, range_index, doppler_index],

    % Create an existence mask.
    EM = hrr_exist_mask:new(F),

    % Create a scatterer record.
    {_, SM, SP, SR} = sample_scatter_record(),

    % Create a processing mask.
    PM = hrr:new_processing_mask(0,0,0),

    % Set the fields of the HRR segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000},
         {last_dwell_of_revisit, additional_dwells},
         {num_of_range_samples, 2},
         {num_of_doppler_samples, 1},
         {detection_threshold, 100}, {range_resolution, 45.16},
         {range_bin_spacing, 99.99}, {doppler_resolution, 1000.50},
         {doppler_bin_spacing, 123.456}, {compression_flag, threshold_decomposition_x10},
         {range_weighting_type, other},
         {doppler_weighting_type, no_statement},
         {maximum_pixel_power, -100.0}, {type_of_hrr, oversized_hrr_chip},
         {processing_mask, PM},
         {num_bytes_magnitude, SM}, {num_bytes_phase, SP},
         {hrr_scatter_records, [SR,SR]}],

    % Use the parameters to construct a new HRR segment.
    {hrr:new(P), PM}.

%% Create a sample target report. Sets all fields to a value.
sample_scatter_record() ->
    Params = [{scatterer_magnitude, 34}, {scatterer_phase, 133},
              {range_index, 10320}, {doppler_index, 10432}],

    % Return a suitable existence mask as well as the actual record.
    EM = {1,1,1,1},
    SM = 1,
    SP = 2,
    {EM, SM, SP, scatterer_rec:new(Params)}.

%% Utility function to compare whether floating point values are within a
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
