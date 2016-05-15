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
-module(hrr_exist_mask).

-export([
    decode/1,
    encode/1,
    new/1,
    display/1,
    get_revisit_index/1,
    get_dwell_index/1,
    get_last_dwell_of_revisit/1,
    get_mti_report_index/1,
    get_num_of_target_scatterers/1,
    get_num_of_range_samples/1,
    get_num_of_doppler_samples/1,
    get_mean_clutter_power/1,
    get_detection_threshold/1,
    get_range_resolution/1,
    get_range_bin_spacing/1,
    get_doppler_resolution/1,
    get_doppler_bin_spacing/1,
    get_center_frequency/1,
    get_compression_flag/1,
    get_range_weighting_type/1,
    get_doppler_weighting_type/1,
    get_maximum_pixel_power/1,
    get_maximum_rcs/1,
    get_range_of_origin/1,
    get_doppler_of_origin/1,
    get_type_of_hrr/1,
    get_processing_mask/1,
    get_num_bytes_magnitude/1,
    get_num_bytes_phase/1,
    get_range_extent_pixels/1,
    get_range_to_nearest_edge/1,
    get_index_of_zero_velocity/1,
    get_target_radial_electrical_length/1,
    get_electrical_length_uncertainty/1,
    get_scatterer_magnitude/1,
    get_scatterer_phase/1,
    get_range_index/1,
    get_doppler_index/1]).

-record(hrr_exist_mask, {
    revisit_index,
    dwell_index,
    last_dwell_of_revisit,
    mti_report_index,
    num_of_target_scatterers,
    num_of_range_samples,
    num_of_doppler_samples,
    mean_clutter_power,
    detection_threshold,
    range_resolution,
    range_bin_spacing,
    doppler_resolution,
    doppler_bin_spacing,
    center_frequency,
    compression_flag,
    range_weighting_type,
    doppler_weighting_type,
    maximum_pixel_power,
    maximum_rcs,
    range_of_origin,
    doppler_of_origin,
    type_of_hrr,
    processing_mask,
    num_bytes_magnitude,
    num_bytes_phase,
    range_extent_pixels,
    range_to_nearest_edge,
    index_of_zero_velocity,
    target_radial_electrical_length,
    electrical_length_uncertainty,
    scatterer_magnitude,
    scatterer_phase,
    range_index,
    doppler_index}).

%% Function to decode the existance mask. Will crash caller if the mask
%% does not have the mandatory bits set.
decode(<<16#7:3,
    H5:1,H6:1,H7:1,1:1,H9:1,16#1F:5,H15:1,16#F:4,
    H20:1,H21:1,H22:1,16#F:4,H27:1,H28:1,H29:1,H30:1,
    H31:1,1:1,H32_2:1,H32_3:1,H32_4:1,
    _Spare:6>>) ->

    #hrr_exist_mask{
        revisit_index = 1,
        dwell_index = 1,
        last_dwell_of_revisit = 1,
        mti_report_index = H5,
        num_of_target_scatterers = H6,
        num_of_range_samples = H7,
        num_of_doppler_samples = 1,
        mean_clutter_power = H9,
        detection_threshold = 1,
        range_resolution = 1,
        range_bin_spacing = 1,
        doppler_resolution = 1,
        doppler_bin_spacing = 1,
        center_frequency = H15,
        compression_flag = 1,
        range_weighting_type = 1,
        doppler_weighting_type = 1,
        maximum_pixel_power = 1,
        maximum_rcs = H20,
        range_of_origin = H21,
        doppler_of_origin = H22,
        type_of_hrr = 1,
        processing_mask = 1,
        num_bytes_magnitude = 1,
        num_bytes_phase = 1,
        range_extent_pixels = H27,
        range_to_nearest_edge = H28,
        index_of_zero_velocity = H29,
        target_radial_electrical_length = H30,
        electrical_length_uncertainty = H31,
        scatterer_magnitude =1,
        scatterer_phase = H32_2,
        range_index = H32_3,
        doppler_index = H32_4}.

%% Function to take an existence mask record and return a binary encoding of
%% the fields.
encode(#hrr_exist_mask{
    revisit_index = 1,
    dwell_index = 1,
    last_dwell_of_revisit = 1,
    mti_report_index = H5,
    num_of_target_scatterers = H6,
    num_of_range_samples = H7,
    num_of_doppler_samples = 1,
    mean_clutter_power = H9,
    detection_threshold = 1,
    range_resolution = 1,
    range_bin_spacing = 1,
    doppler_resolution = 1,
    doppler_bin_spacing = 1,
    center_frequency = H15,
    compression_flag = 1,
    range_weighting_type = 1,
    doppler_weighting_type = 1,
    maximum_pixel_power = 1,
    maximum_rcs = H20,
    range_of_origin = H21,
    doppler_of_origin = H22,
    type_of_hrr = 1,
    processing_mask = 1,
    num_bytes_magnitude = 1,
    num_bytes_phase = 1,
    range_extent_pixels = H27,
    range_to_nearest_edge = H28,
    index_of_zero_velocity = H29,
    target_radial_electrical_length = H30,
    electrical_length_uncertainty = H31,
    scatterer_magnitude =1,
    scatterer_phase = H32_2,
    range_index = H32_3,
    doppler_index = H32_4}) ->

    % Encode all of the fields as a binary.
    <<16#7:3,
        H5:1,H6:1,H7:1,1:1,H9:1,16#1F:5,H15:1,16#F:4,
        H20:1,H21:1,H22:1,16#F:4,H27:1,H28:1,H29:1,H30:1,
        H31:1,1:1,H32_2:1,H32_3:1,H32_4:1,0:6>>.

%% Function to allow the caller to construct an existence mask by supplying
%% a list of atoms specifiying the optional and conditional fields to set.
new(Fields) when is_list(Fields) ->
    % Local function to check if a parameter exists in the list supplied
    % and return 1 if it is and 0 if not.
    F = fun(X) ->
            bool_to_int(lists:member(X, Fields))
        end,

    #hrr_exist_mask{
        revisit_index = 1,
        dwell_index = 1,
        last_dwell_of_revisit = 1,
        mti_report_index = F(mti_report_index),
        num_of_target_scatterers = F(num_of_target_scatterers),
        num_of_range_samples = F(num_of_range_samples),
        num_of_doppler_samples = 1,
        mean_clutter_power = F(mean_clutter_power),
        detection_threshold = 1,
        range_resolution = 1,
        range_bin_spacing = 1,
        doppler_resolution = 1,
        doppler_bin_spacing = 1,
        center_frequency = F(center_frequency),
        compression_flag = 1,
        range_weighting_type = 1,
        doppler_weighting_type = 1,
        maximum_pixel_power = 1,
        maximum_rcs = F(maximum_rcs),
        range_of_origin = F(range_of_origin),
        doppler_of_origin = F(doppler_of_origin),
        type_of_hrr = 1,
        processing_mask = 1,
        num_bytes_magnitude = 1,
        num_bytes_phase = 1,
        range_extent_pixels = F(range_extent_pixels),
        range_to_nearest_edge = F(range_to_nearest_edge),
        index_of_zero_velocity = F(index_of_zero_velocity),
        target_radial_electrical_length = F(target_radial_electrical_length),
        electrical_length_uncertainty = F(electrical_length_uncertainty),
        scatterer_magnitude =1,
        scatterer_phase = F(scatterer_phase),
        range_index = F(range_index),
        doppler_index = F(doppler_index)}.

bool_to_int(true) -> 1;
bool_to_int(false) -> 0.

display(EM) ->
    io:format("****************************************~n"),
    io:format("** @hrr existence_mask~n"),
    io:format("Revisit index: ~p~n", [get_revisit_index(EM)]),
    io:format("Dwell index: ~p~n", [get_dwell_index(EM)]),
    io:format("Last dwell of revisit: ~p~n", [get_last_dwell_of_revisit(EM)]),
    io:format("MTI Report Index: ~p~n", [get_mti_report_index(EM)]),
    io:format("Number of Target Scatterers: ~p~n", [get_num_of_target_scatterers(EM)]),
    io:format("Number of Range Samples/Total Scatterers: ~p~n", [get_num_of_range_samples(EM)]),
    io:format("Number of Doppler Samples: ~p~n", [get_num_of_doppler_samples(EM)]),
    io:format("Mean Clutter Power: ~p~n", [get_mean_clutter_power(EM)]),
    io:format("Detection Threshold: ~p~n", [get_detection_threshold(EM)]),
    io:format("Range Resolution: ~p~n", [get_range_resolution(EM)]),
    io:format("Range Bin Spacing: ~p~n", [get_range_bin_spacing(EM)]),
    io:format("Doppler Resolution: ~p~n", [get_doppler_resolution(EM)]),
    io:format("Doppler Bin Spacing / PRF: ~p~n", [get_doppler_bin_spacing(EM)]),
    io:format("Center Frequency: ~p~n", [get_center_frequency(EM)]),
    io:format("Compression Flag: ~p~n", [get_compression_flag(EM)]),
    io:format("Range Weighting Function Type: ~p~n", [get_range_weighting_type(EM)]),
    io:format("Doppler Weighting Function Type: ~p~n", [get_doppler_weighting_type(EM)]),
    io:format("Maximum Pixel Power: ~p~n", [get_maximum_pixel_power(EM)]),
    io:format("Maximum RCS: ~p~n", [get_maximum_rcs(EM)]),
    io:format("Range of Origin: ~p~n", [get_range_of_origin(EM)]),
    io:format("Doppler of Origin: ~p~n", [get_doppler_of_origin(EM)]),
    io:format("Type of HRR/RDM: ~p~n", [get_type_of_hrr(EM)]),
    io:format("Processing Mask: ~p~n", [get_processing_mask(EM)]),
    io:format("Number Bytes Magnitude: ~p~n", [get_num_bytes_magnitude(EM)]),
    io:format("Number Bytes Phase: ~p~n", [get_num_bytes_phase(EM)]),
    io:format("Range Extent In Pixels: ~p~n", [get_range_extent_pixels(EM)]),
    io:format("Range To Nearest Edge In Chip: ~p~n", [get_range_to_nearest_edge(EM)]),
    io:format("Index Of Zero Velocity Bin: ~p~n", [get_index_of_zero_velocity(EM)]),
    io:format("Target Radial Electrical Length: ~p~n", [get_target_radial_electrical_length(EM)]),
    io:format("Electrical Length Uncertainty: ~p~n", [get_electrical_length_uncertainty(EM)]),
    io:format("Scatterer Magnitude: ~p~n", [get_scatterer_magnitude(EM)]),
    io:format("Scatterer Phase: ~p~n", [get_scatterer_phase(EM)]),
    io:format("Range Index: ~p~n", [get_range_index(EM)]),
    io:format("Doppler Index: ~p~n", [get_doppler_index(EM)]).

%% Functions to allow external modules to gain access to fields of the
%% existence mask without knowledge of the internal representation.
get_revisit_index(EM) -> EM#hrr_exist_mask.revisit_index.
get_dwell_index(EM) -> EM#hrr_exist_mask.dwell_index.
get_last_dwell_of_revisit(EM) -> EM#hrr_exist_mask.last_dwell_of_revisit.
get_mti_report_index(EM) -> EM#hrr_exist_mask.mti_report_index.
get_num_of_target_scatterers(EM) -> EM#hrr_exist_mask.num_of_target_scatterers.
get_num_of_range_samples(EM) -> EM#hrr_exist_mask.num_of_range_samples.
get_num_of_doppler_samples(EM) -> EM#hrr_exist_mask.num_of_doppler_samples.
get_mean_clutter_power(EM) -> EM#hrr_exist_mask.mean_clutter_power.
get_detection_threshold(EM) -> EM#hrr_exist_mask.detection_threshold.
get_range_resolution(EM) -> EM#hrr_exist_mask.range_resolution.
get_range_bin_spacing(EM) -> EM#hrr_exist_mask.range_bin_spacing.
get_doppler_resolution(EM) -> EM#hrr_exist_mask.doppler_resolution.
get_doppler_bin_spacing(EM) -> EM#hrr_exist_mask.doppler_bin_spacing.
get_center_frequency(EM) -> EM#hrr_exist_mask.center_frequency.
get_compression_flag(EM) -> EM#hrr_exist_mask.compression_flag.
get_range_weighting_type(EM) -> EM#hrr_exist_mask.range_weighting_type.
get_doppler_weighting_type(EM) -> EM#hrr_exist_mask.doppler_weighting_type.
get_maximum_pixel_power(EM) -> EM#hrr_exist_mask.maximum_pixel_power.
get_maximum_rcs(EM) -> EM#hrr_exist_mask.maximum_rcs.
get_range_of_origin(EM) -> EM#hrr_exist_mask.range_of_origin.
get_doppler_of_origin(EM) -> EM#hrr_exist_mask.doppler_of_origin.
get_type_of_hrr(EM) -> EM#hrr_exist_mask.type_of_hrr.
get_processing_mask(EM) -> EM#hrr_exist_mask.processing_mask.
get_num_bytes_magnitude(EM) -> EM#hrr_exist_mask.num_bytes_magnitude.
get_num_bytes_phase(EM) -> EM#hrr_exist_mask.num_bytes_phase.
get_range_extent_pixels(EM) -> EM#hrr_exist_mask.range_extent_pixels.
get_range_to_nearest_edge(EM) -> EM#hrr_exist_mask.range_to_nearest_edge.
get_index_of_zero_velocity(EM) -> EM#hrr_exist_mask.index_of_zero_velocity.
get_target_radial_electrical_length(EM) -> EM#hrr_exist_mask.target_radial_electrical_length.
get_electrical_length_uncertainty(EM) -> EM#hrr_exist_mask.electrical_length_uncertainty.
get_scatterer_magnitude(EM) -> EM#hrr_exist_mask.scatterer_magnitude.
get_scatterer_phase(EM) -> EM#hrr_exist_mask.scatterer_phase.
get_range_index(EM) -> EM#hrr_exist_mask.range_index.
get_doppler_index(EM) -> EM#hrr_exist_mask.doppler_index.
