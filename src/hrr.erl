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
    decode/1]).

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
