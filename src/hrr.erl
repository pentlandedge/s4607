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
-module(hrr).

-record(hrr_segment, {
    existence_mask,
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
    hrr_scatter_records}).
