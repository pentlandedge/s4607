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

-export([
    decode/1,
    new/1,
    get_existence_mask/1,
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
    get_hrr_scatter_records/1]).

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

-record(processing_mask, {
    clutter_cancellation,
    single_ambiguity_keystoning,
    multi_ambiguity_keystoning}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HRR segment decoding functions.

decode(<<EM:5/binary, RI:16/integer-unsigned-big,
    DI:16/integer-unsigned-big, LD, Rest/binary>>) ->

    % Fixed part of the HRR segement is pattern matched above, remainder
    % depends on the existence mask.
    <<16#7:3,
    H5:1,H6:1,H7:1,1:1,H9:1,16#1F:5,H15:1,16#F:4,
    H20:1,H21:1,H22:1,16#F:4,H27:1,H28:1,H29:1,H30:1,
    H31:1,1:1,_H32_2:1,_H32_3:1,_H32_4:1,
    _Spare:6>> = EM,

    {MtiReportIndex, Bin1} = sutils:conditional_extract(
        Rest,
        H5,
        2,
        fun stanag_types:i16_to_integer/1,
        0),

    {NumOfTargetScatterers, Bin2} = sutils:conditional_extract(
        Bin1,
        H6,
        2,
        fun stanag_types:i16_to_integer/1,
        0),

    {NumOfRangeSamples, Bin3} = sutils:conditional_extract(
        Bin2,
        H7,
        2,
        fun stanag_types:i16_to_integer/1,
        0),

    {ok, NumOfDopplerSamples, Bin4} = sutils:extract_conv_data(
        Bin3, 2, fun stanag_types:i16_to_integer/1),

    {MeanClutterPower, Bin5} = sutils:conditional_extract(
        Bin4,
        H9,
        1,
        fun stanag_types:i8_to_integer/1,
        0),

    {ok, DetectionThreshold, Bin6} = sutils:extract_conv_data(
        Bin5, 1, fun stanag_types:i8_to_integer/1),

    {ok, RangeResolution, Bin7} = sutils:extract_conv_data(
        Bin6, 2, fun stanag_types:b16_to_float/1),

    {ok, RangeBinSpacing, Bin8} = sutils:extract_conv_data(
        Bin7, 2, fun stanag_types:b16_to_float/1),

    {ok, DopplerResolution, Bin9} = sutils:extract_conv_data(
        Bin8, 4, fun stanag_types:h32_to_float/1),

    {ok, DopplerBinSpacing, Bin10} = sutils:extract_conv_data(
        Bin9, 4, fun stanag_types:h32_to_float/1),

    {CenterFrequency, Bin11} = sutils:conditional_extract(
        Bin10,
        H15,
        4,
        fun stanag_types:b32_to_float/1,
        1.0),

    {ok, CompressionFlag, Bin12} = sutils:extract_data(Bin11, 1),
    CompressionFlagDecoded = decode_compression_flag(CompressionFlag),

    {ok, RangeWeightingType, Bin13} = sutils:extract_data(Bin12, 1),
    RangeWeightingTypeDecoded = decode_range_weighting_type(RangeWeightingType),

    {ok, DopplerWeightingType, Bin14} = sutils:extract_data(Bin13, 1),
    DopplerWeightingTypeDecoded = decode_doppler_weighting_type(DopplerWeightingType),

    {ok, MaximumPixelPower, Bin15} = sutils:extract_conv_data(
        Bin14, 2, fun stanag_types:b16_to_float/1),

    {MaximumRcs, Bin16} = sutils:conditional_extract(
        Bin15,
        H20,
        1,
        fun stanag_types:s8_to_integer/1,
        0),

    {RangeOfOrigin, Bin17} = sutils:conditional_extract(
        Bin16,
        H21,
        2,
        fun stanag_types:s16_to_integer/1,
        0),

    {DopplerOfOrigin, Bin18} = sutils:conditional_extract(
        Bin17,
        H22,
        4,
        fun stanag_types:h32_to_float/1,
        0.0),

    {ok, TypeOfHrr, Bin19} = sutils:extract_data(Bin18, 1),
    TypeOfHrrDecoded = decode_type_of_hrr(TypeOfHrr),

    {ok, ProcessingMask, Bin20} = sutils:extract_conv_data(
        Bin19, 1, fun stanag_types:i8_to_integer/1),

    {ok, NumBytesMagnitude, Bin21} = sutils:extract_conv_data(
        Bin20, 1, fun stanag_types:i8_to_integer/1),

    {ok, NumBytesPhase, Bin22} = sutils:extract_conv_data(
        Bin21, 1, fun stanag_types:i8_to_integer/1),

    {RangeExtentPixels, Bin23} = sutils:conditional_extract(
        Bin22,
        H27,
        1,
        fun stanag_types:i8_to_integer/1,
        0),

    {RangeToNearestEdge, Bin24} = sutils:conditional_extract(
        Bin23,
        H28,
        1,
        fun stanag_types:i8_to_integer/1,
        0),

    {IndexOfZeroVelocity, Bin25} = sutils:conditional_extract(
        Bin24,
        H29,
        1,
        fun stanag_types:i8_to_integer/1,
        0),

    {TargetRadialElectricalLength, Bin26} = sutils:conditional_extract(
        Bin25,
        H30,
        4,
        fun stanag_types:b32_to_float/1,
        0.0),

    {ElectricalLengthUncertainty, Bin27} = sutils:conditional_extract(
        Bin26,
        H31,
        4,
        fun stanag_types:b32_to_float/1,
        0.0),

    HrrScatterRecords = Bin27,

    {ok, #hrr_segment{
        existence_mask = EM,
        revisit_index = RI,
        dwell_index = DI,
        last_dwell_of_revisit = decode_last_dwell_of_revisit(LD),
        mti_report_index = MtiReportIndex,
        num_of_target_scatterers = NumOfTargetScatterers,
        num_of_range_samples = NumOfRangeSamples,
        num_of_doppler_samples = NumOfDopplerSamples,
        mean_clutter_power = MeanClutterPower,
        detection_threshold = DetectionThreshold,
        range_resolution = RangeResolution,
        range_bin_spacing = RangeBinSpacing,
        doppler_resolution = DopplerResolution,
        doppler_bin_spacing = DopplerBinSpacing,
        center_frequency = CenterFrequency,
        compression_flag = CompressionFlagDecoded,
        range_weighting_type = RangeWeightingTypeDecoded,
        doppler_weighting_type = DopplerWeightingTypeDecoded,
        maximum_pixel_power = MaximumPixelPower,
        maximum_rcs = MaximumRcs,
        range_of_origin = RangeOfOrigin,
        doppler_of_origin = DopplerOfOrigin,
        type_of_hrr = TypeOfHrrDecoded,
        processing_mask = ProcessingMask,
        num_bytes_magnitude = NumBytesMagnitude,
        num_bytes_phase = NumBytesPhase,
        range_extent_pixels = RangeExtentPixels,
        range_to_nearest_edge = RangeToNearestEdge,
        index_of_zero_velocity = IndexOfZeroVelocity,
        target_radial_electrical_length = TargetRadialElectricalLength,
        electrical_length_uncertainty = ElectricalLengthUncertainty,
        hrr_scatter_records = HrrScatterRecords}}.

%% Function to create a new HRR segment structure from the specified fields.
new(Fields) ->
    % Local function to pull the parameter from the list or supply a default
    % value.
    F = fun(P, L, Default) ->
            case lists:keyfind(P, 1, L) of
                {P, V} -> V;
                false  -> Default
            end
        end,

    #hrr_segment{
        existence_mask = F(existence_mask, Fields, 0),
        revisit_index = F(revisit_index, Fields, 0),
        dwell_index = F(dwell_index, Fields, 0),
        last_dwell_of_revisit = F(last_dwell_of_revisit, Fields, 0),
        mti_report_index = F(mti_report_index, Fields, 0),
        num_of_target_scatterers = F(num_of_target_scatterers, Fields, 0),
        num_of_range_samples = F(num_of_range_samples, Fields, 0),
        num_of_doppler_samples = F(num_of_doppler_samples, Fields, 0),
        mean_clutter_power = F(mean_clutter_power, Fields, 0),
        detection_threshold = F(detection_threshold, Fields, 0),
        range_resolution = F(range_resolution, Fields, 0),
        range_bin_spacing = F(range_bin_spacing, Fields, 0),
        doppler_resolution = F(doppler_resolution, Fields, 0),
        doppler_bin_spacing = F(doppler_bin_spacing, Fields, 0),
        center_frequency = F(center_frequency, Fields, 0),
        compression_flag = F(compression_flag, Fields, 0),
        range_weighting_type = F(range_weighting_type, Fields, 0),
        doppler_weighting_type = F(doppler_weighting_type, Fields, 0),
        maximum_pixel_power = F(maximum_pixel_power, Fields, 0),
        maximum_rcs = F(maximum_rcs, Fields, 0),
        range_of_origin = F(range_of_origin, Fields, 0),
        doppler_of_origin = F(doppler_of_origin, Fields, 0),
        type_of_hrr = F(type_of_hrr, Fields, 0),
        processing_mask = F(processing_mask, Fields, 0),
        num_bytes_magnitude = F(num_bytes_magnitude, Fields, 0),
        num_bytes_phase = F(num_bytes_phase, Fields, 0),
        range_extent_pixels = F(range_extent_pixels, Fields, 0),
        range_to_nearest_edge = F(range_to_nearest_edge, Fields, 0),
        index_of_zero_velocity = F(index_of_zero_velocity, Fields, 0),
        target_radial_electrical_length = F(target_radial_electrical_length, Fields, 0),
        electrical_length_uncertainty = F(electrical_length_uncertainty, Fields, 0),
        hrr_scatter_records = F(hrr_scatter_records, Fields, [])}.

decode_last_dwell_of_revisit(0) -> additional_dwells;
decode_last_dwell_of_revisit(1) -> no_additional_dwells.

decode_compression_flag(<<X>>) -> decode_compression_flag(X);
decode_compression_flag(0) -> no_compression;
decode_compression_flag(1) -> threshold_decomposition_x10.

decode_range_weighting_type(<<X>>) -> decode_range_weighting_type(X);
decode_range_weighting_type(0) -> no_statement;
decode_range_weighting_type(1) -> taylor_weighting;
decode_range_weighting_type(2) -> other.

decode_doppler_weighting_type(<<X>>) -> decode_doppler_weighting_type(X);
decode_doppler_weighting_type(0) -> no_statement;
decode_doppler_weighting_type(1) -> taylor_weighting;
decode_doppler_weighting_type(2) -> other.

decode_type_of_hrr(<<X>>) -> decode_type_of_hrr(X);
decode_type_of_hrr(0) -> other;
decode_type_of_hrr(1) -> one_d_hrr_chip;
decode_type_of_hrr(2) -> two_d_hrr_chip;
decode_type_of_hrr(3) -> sparse_hrr_chip;
decode_type_of_hrr(4) -> oversized_hrr_chip;
decode_type_of_hrr(5) -> full_rdm;
decode_type_of_hrr(6) -> partial_rdm;
decode_type_of_hrr(7) -> full_range_pulse_data.

decode_processing_mask(<<PM>>) ->
    <<ClutterCancellation:1, SingleAmbiguityKeystoning:1,
    MultiAmbiguityKeystoning:1,
    _Spare:5>> = PM,

    #processing_mask{
    clutter_cancellation = ClutterCancellation,
    single_ambiguity_keystoning = SingleAmbiguityKeystoning,
    multi_ambiguity_keystoning = MultiAmbiguityKeystoning}.

%% Accessor functions to allow access to the record fields without creating
%% client dependencies on the actual structure.
get_existence_mask(#hrr_segment{existence_mask = X}) -> X.
get_revisit_index(#hrr_segment{revisit_index = X}) -> X.
get_dwell_index(#hrr_segment{dwell_index = X}) -> X.
get_last_dwell_of_revisit(#hrr_segment{last_dwell_of_revisit = X}) -> X.
get_mti_report_index(#hrr_segment{mti_report_index = X}) -> X.
get_num_of_target_scatterers(#hrr_segment{num_of_target_scatterers = X}) -> X.
get_num_of_range_samples(#hrr_segment{num_of_range_samples = X}) -> X.
get_num_of_doppler_samples(#hrr_segment{num_of_doppler_samples = X}) -> X.
get_mean_clutter_power(#hrr_segment{mean_clutter_power = X}) -> X.
get_detection_threshold(#hrr_segment{detection_threshold = X}) -> X.
get_range_resolution(#hrr_segment{range_resolution = X}) -> X.
get_range_bin_spacing(#hrr_segment{range_bin_spacing = X}) -> X.
get_doppler_resolution(#hrr_segment{doppler_resolution = X}) -> X.
get_doppler_bin_spacing(#hrr_segment{doppler_bin_spacing = X}) -> X.
get_center_frequency(#hrr_segment{center_frequency = X}) -> X.
get_compression_flag(#hrr_segment{compression_flag = X}) -> X.
get_range_weighting_type(#hrr_segment{range_weighting_type = X}) -> X.
get_doppler_weighting_type(#hrr_segment{doppler_weighting_type = X}) -> X.
get_maximum_pixel_power(#hrr_segment{maximum_pixel_power = X}) -> X.
get_maximum_rcs(#hrr_segment{maximum_rcs = X}) -> X.
get_range_of_origin(#hrr_segment{range_of_origin = X}) -> X.
get_doppler_of_origin(#hrr_segment{doppler_of_origin = X}) -> X.
get_type_of_hrr(#hrr_segment{type_of_hrr = X}) -> X.
get_processing_mask(#hrr_segment{processing_mask = X}) -> X.
get_num_bytes_magnitude(#hrr_segment{num_bytes_magnitude = X}) -> X.
get_num_bytes_phase(#hrr_segment{num_bytes_phase = X}) -> X.
get_range_extent_pixels(#hrr_segment{range_extent_pixels = X}) -> X.
get_range_to_nearest_edge(#hrr_segment{range_to_nearest_edge = X}) -> X.
get_index_of_zero_velocity(#hrr_segment{index_of_zero_velocity = X}) -> X.
get_target_radial_electrical_length(#hrr_segment{target_radial_electrical_length = X}) -> X.
get_electrical_length_uncertainty(#hrr_segment{electrical_length_uncertainty = X}) -> X.
get_hrr_scatter_records(#hrr_segment{hrr_scatter_records = X}) -> X.
