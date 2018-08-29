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
    encode/1,
    new/1,
    new_processing_mask/3,
    payload_size/1,
    display/1,
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
    EMrec = hrr_exist_mask:decode(EM),

    {MtiReportIndex, Bin1} = sutils:conditional_extract(
        Rest,
        hrr_exist_mask:get_mti_report_index(EMrec),
        2,
        fun stanag_types:i16_to_integer/1,
        0),

    {NumOfTargetScatterers, Bin2} = sutils:conditional_extract(
        Bin1,
        hrr_exist_mask:get_num_of_target_scatterers(EMrec),
        2,
        fun stanag_types:i16_to_integer/1,
        0),

    {NumOfRangeSamples, Bin3} = sutils:conditional_extract(
        Bin2,
        hrr_exist_mask:get_num_of_range_samples(EMrec),
        2,
        fun stanag_types:i16_to_integer/1,
        0),

    {ok, NumOfDopplerSamples, Bin4} = sutils:extract_conv_data(
        Bin3, 2, fun stanag_types:i16_to_integer/1),

    {MeanClutterPower, Bin5} = sutils:conditional_extract(
        Bin4,
        hrr_exist_mask:get_mean_clutter_power(EMrec),
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
        hrr_exist_mask:get_center_frequency(EMrec),
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
        hrr_exist_mask:get_maximum_rcs(EMrec),
        1,
        fun stanag_types:s8_to_integer/1,
        0),

    {RangeOfOrigin, Bin17} = sutils:conditional_extract(
        Bin16,
        hrr_exist_mask:get_range_of_origin(EMrec),
        2,
        fun stanag_types:s16_to_integer/1,
        0),

    {DopplerOfOrigin, Bin18} = sutils:conditional_extract(
        Bin17,
        hrr_exist_mask:get_doppler_of_origin(EMrec),
        4,
        fun stanag_types:h32_to_float/1,
        0.0),

    {ok, TypeOfHrr, Bin19} = sutils:extract_data(Bin18, 1),
    TypeOfHrrDecoded = decode_type_of_hrr(TypeOfHrr),

    {ok, ProcessingMask, Bin20} = sutils:extract_conv_data(
        Bin19, 1, fun decode_processing_mask/1),

    {ok, NumBytesMagnitude, Bin21} = sutils:extract_conv_data(
        Bin20, 1, fun stanag_types:i8_to_integer/1),

    {ok, NumBytesPhase, Bin22} = sutils:extract_conv_data(
        Bin21, 1, fun stanag_types:i8_to_integer/1),

    {RangeExtentPixels, Bin23} = sutils:conditional_extract(
        Bin22,
        hrr_exist_mask:get_range_extent_pixels(EMrec),
        1,
        fun stanag_types:i8_to_integer/1,
        0),

    {RangeToNearestEdge, Bin24} = sutils:conditional_extract(
        Bin23,
        hrr_exist_mask:get_range_to_nearest_edge(EMrec),
        1,
        fun stanag_types:i8_to_integer/1,
        0),

    {IndexOfZeroVelocity, Bin25} = sutils:conditional_extract(
        Bin24,
        hrr_exist_mask:get_index_of_zero_velocity(EMrec),
        1,
        fun stanag_types:i8_to_integer/1,
        0),

    {TargetRadialElectricalLength, Bin26} = sutils:conditional_extract(
        Bin25,
        hrr_exist_mask:get_target_radial_electrical_length(EMrec),
        4,
        fun stanag_types:b32_to_float/1,
        0.0),

    {ElectricalLengthUncertainty, Bin27} = sutils:conditional_extract(
        Bin26,
        hrr_exist_mask:get_electrical_length_uncertainty(EMrec),
        4,
        fun stanag_types:b32_to_float/1,
        0.0),

    NumOfRecords = get_number_of_scatterer_records(TypeOfHrrDecoded,
        NumOfTargetScatterers, NumOfRangeSamples, NumOfDopplerSamples),

    HrrScatterRecords = decode_scatterer_rec_list(Bin27,
        {1,
        hrr_exist_mask:get_scatterer_phase(EMrec),
        hrr_exist_mask:get_range_index(EMrec),
        hrr_exist_mask:get_doppler_index(EMrec)},
        NumOfRecords,
        NumBytesMagnitude,
        NumBytesPhase),

    {ok, #hrr_segment{
        existence_mask = EMrec,
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
        hrr_scatter_records = HrrScatterRecords}};
decode(_) ->
    {error, hrr_mismatch}.

encode(HRR) ->
    % Extract the existence mask from the incoming HRR segment.
    EM = get_existence_mask(HRR),

    % Create a local function to wrap the check of the existence mask and
    % the parameter encoding/appending.
    % Exploits the trick that the function to access the HRR segment
    % fields is the same as that to access the corresponding existence mask
    % field.
    F = fun({EvalFun, EncFun}, Acc) ->
            case hrr_exist_mask:EvalFun(EM) of
                1 ->
                    Param = hrr:EvalFun(HRR),
                    PB = EncFun(Param),
                    <<Acc/binary,PB/binary>>;
                0 ->
                    Acc
            end
        end,

    ParamTable = [
        {get_revisit_index, fun stanag_types:integer_to_i16/1},
        {get_dwell_index, fun stanag_types:integer_to_i16/1},
        {get_last_dwell_of_revisit, fun encode_last_dwell_of_revisit/1},
        {get_mti_report_index, fun stanag_types:integer_to_i16/1},
        {get_num_of_target_scatterers, fun stanag_types:integer_to_i16/1},
        {get_num_of_range_samples, fun stanag_types:integer_to_i16/1},
        {get_num_of_doppler_samples, fun stanag_types:integer_to_i16/1},
        {get_mean_clutter_power, fun stanag_types:integer_to_i8/1},
        {get_detection_threshold, fun stanag_types:integer_to_i8/1},
        {get_range_resolution, fun stanag_types:float_to_b16/1},
        {get_range_bin_spacing, fun stanag_types:float_to_b16/1},
        {get_doppler_resolution, fun stanag_types:float_to_h32/1},
        {get_doppler_bin_spacing, fun stanag_types:float_to_h32/1},
        {get_center_frequency, fun stanag_types:float_to_ba32/1},
        {get_compression_flag, fun encode_compression_flag/1},
        {get_range_weighting_type, fun encode_range_weighting_type/1},
        {get_doppler_weighting_type, fun encode_doppler_weighting_type/1},
        {get_maximum_pixel_power, fun stanag_types:float_to_b16/1},
        {get_maximum_rcs, fun stanag_types:integer_to_s8/1},
        {get_range_of_origin, fun stanag_types:integer_to_s16/1},
        {get_doppler_of_origin, fun stanag_types:float_to_h32/1},
        {get_type_of_hrr, fun encode_type_of_hrr/1},
        {get_processing_mask, fun encode_processing_mask/1},
        {get_num_bytes_magnitude, fun stanag_types:integer_to_i8/1},
        {get_num_bytes_phase, fun stanag_types:integer_to_i8/1},
        {get_range_extent_pixels, fun stanag_types:integer_to_i8/1},
        {get_range_to_nearest_edge, fun stanag_types:integer_to_i32/1},
        {get_index_of_zero_velocity, fun stanag_types:integer_to_i8/1},
        {get_target_radial_electrical_length, fun stanag_types:float_to_b32/1},
        {get_electrical_length_uncertainty, fun stanag_types:float_to_b32/1}],


    EMenc = hrr_exist_mask:encode(EM),
    % Produce a binary hrr segment, missing only the scatterer records.
    Bin1 = lists:foldl(F, EMenc, ParamTable),
    % Append any scatterer records.
    encode_scatterer_records(
        get_number_of_scatterer_records(
            get_type_of_hrr(HRR),
            get_num_of_target_scatterers(HRR),
            get_num_of_range_samples(HRR),
            get_num_of_doppler_samples(HRR)),
        get_hrr_scatter_records(HRR),
        {
            hrr_exist_mask:get_scatterer_magnitude(EM),
            hrr_exist_mask:get_scatterer_phase(EM),
            hrr_exist_mask:get_range_index(EM),
            hrr_exist_mask:get_doppler_index(EM)
        },
        get_num_bytes_magnitude(HRR),
        get_num_bytes_phase(HRR),
        Bin1).

%% Helper function for the hrr encode: encodes all of the scatterer records.
%% InitBin can be set to the HRR segment prior to adding the records
%% so that the scatterer records are automatically added to the end of the
%% HRR segment.
encode_scatterer_records(0, _RepList, _EM, _MagBytes, _PhaseBytes, InitBin) ->
    InitBin;
encode_scatterer_records(RepCount, RepList, EM, MagBytes, PhaseBytes, InitBin)
    when RepCount =:= length(RepList) ->

    F = fun(R, Acc) ->
            Bin = scatterer_rec:encode(R, EM, MagBytes, PhaseBytes),
            <<Acc/binary,Bin/binary>>
        end,
    lists:foldl(F, InitBin, RepList).


%% Create a new HRR segment structure from the specified fields.
new(Fields) ->
    % Shorthand.
    F = fun sutils:extract_param_or_default/3,

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

encode_last_dwell_of_revisit(additional_dwells) -> <<0>>;
encode_last_dwell_of_revisit(no_additional_dwells) -> <<1>>.

decode_compression_flag(<<X>>) -> decode_compression_flag(X);
decode_compression_flag(0) -> no_compression;
decode_compression_flag(1) -> threshold_decomposition_x10.

encode_compression_flag(no_compression) -> <<0>>;
encode_compression_flag(threshold_decomposition_x10) -> <<1>>.

decode_range_weighting_type(<<X>>) -> decode_range_weighting_type(X);
decode_range_weighting_type(0) -> no_statement;
decode_range_weighting_type(1) -> taylor_weighting;
decode_range_weighting_type(2) -> other.

encode_range_weighting_type(no_statement) -> <<0>>;
encode_range_weighting_type(taylor_weighting) -> <<1>>;
encode_range_weighting_type(other) -> <<2>>.

decode_doppler_weighting_type(<<X>>) -> decode_doppler_weighting_type(X);
decode_doppler_weighting_type(0) -> no_statement;
decode_doppler_weighting_type(1) -> taylor_weighting;
decode_doppler_weighting_type(2) -> other.

encode_doppler_weighting_type(no_statement) -> <<0>>;
encode_doppler_weighting_type(taylor_weighting) -> <<1>>;
encode_doppler_weighting_type(other) -> <<2>>.

decode_type_of_hrr(<<X>>) -> decode_type_of_hrr(X);
decode_type_of_hrr(0) -> other;
decode_type_of_hrr(1) -> one_d_hrr_chip;
decode_type_of_hrr(2) -> two_d_hrr_chip;
decode_type_of_hrr(3) -> sparse_hrr_chip;
decode_type_of_hrr(4) -> oversized_hrr_chip;
decode_type_of_hrr(5) -> full_rdm;
decode_type_of_hrr(6) -> partial_rdm;
decode_type_of_hrr(7) -> full_range_pulse_data.

encode_type_of_hrr(other) -> <<0>>;
encode_type_of_hrr(one_d_hrr_chip) -> <<1>>;
encode_type_of_hrr(two_d_hrr_chip) -> <<2>>;
encode_type_of_hrr(sparse_hrr_chip) -> <<3>>;
encode_type_of_hrr(oversized_hrr_chip) -> <<4>>;
encode_type_of_hrr(full_rdm) -> <<5>>;
encode_type_of_hrr(partial_rdm) -> <<6>>;
encode_type_of_hrr(full_range_pulse_data) -> <<7>>.

new_processing_mask(ClutterCancellation,
                    SingleAmbiguityKeystoning,
                    MultiAmbiguityKeystoning) ->
    #processing_mask{
        clutter_cancellation = ClutterCancellation,
        single_ambiguity_keystoning = SingleAmbiguityKeystoning,
        multi_ambiguity_keystoning = MultiAmbiguityKeystoning}.

decode_processing_mask(<<ClutterCancellation:1, SingleAmbiguityKeystoning:1,
    MultiAmbiguityKeystoning:1, _Spare:5>>) ->

    #processing_mask{
        clutter_cancellation = ClutterCancellation,
        single_ambiguity_keystoning = SingleAmbiguityKeystoning,
        multi_ambiguity_keystoning = MultiAmbiguityKeystoning}.

encode_processing_mask(#processing_mask{
        clutter_cancellation = ClutterCancellation,
        single_ambiguity_keystoning = SingleAmbiguityKeystoning,
        multi_ambiguity_keystoning = MultiAmbiguityKeystoning}) ->

    <<ClutterCancellation:1, SingleAmbiguityKeystoning:1,
    MultiAmbiguityKeystoning:1, 0:5>>.

decode_scatterer_rec_list(Bin, EM, RecordCount, MagnitudeByteSize, PhaseByteSize) ->
    decode_scatterer_rec_list(Bin, EM, RecordCount, MagnitudeByteSize, PhaseByteSize, []).
decode_scatterer_rec_list(_Bin, _EM, 0, _MagnitudeByteSize, _PhaseByteSize, AccRecords) ->
    lists:reverse(AccRecords);
decode_scatterer_rec_list(Bin, EM, RecordCount, MagnitudeByteSize, PhaseByteSize, AccRecords) when RecordCount > 0 ->
    {ok, SR, Rem} = scatterer_rec:decode(Bin, EM, MagnitudeByteSize, PhaseByteSize),
    decode_scatterer_rec_list(Rem, EM, RecordCount-1, MagnitudeByteSize, PhaseByteSize, [SR|AccRecords]).

%% Calculate the expected size of a HRR segment once encoded.
payload_size(#hrr_segment{existence_mask = EM, type_of_hrr = TypeOfHrr,
            num_of_target_scatterers = NumOfTargetScatterers,
            num_of_range_samples = NumOfRangeSamples,
            num_of_doppler_samples = NumOfDopplerSamples,
            num_bytes_magnitude = MagnitudeByteSize,
            num_bytes_phase = PhaseByteSize}) ->
    SRCount = get_number_of_scatterer_records(TypeOfHrr,
        NumOfTargetScatterers, NumOfRangeSamples, NumOfDopplerSamples),
    payload_size(EM, SRCount, MagnitudeByteSize, PhaseByteSize).

payload_size(EM, SRCount, MagnitudeByteSize, PhaseByteSize) ->
    SizeList = [
        {get_revisit_index, 2},
        {get_dwell_index, 2},
        {get_last_dwell_of_revisit, 1},
        {get_mti_report_index, 2},
        {get_num_of_target_scatterers, 2},
        {get_num_of_range_samples, 2},
        {get_num_of_doppler_samples, 2},
        {get_mean_clutter_power, 1},
        {get_detection_threshold, 1},
        {get_range_resolution, 2},
        {get_range_bin_spacing, 2},
        {get_doppler_resolution, 4},
        {get_doppler_bin_spacing, 4},
        {get_center_frequency, 4},
        {get_compression_flag, 1},
        {get_range_weighting_type, 1},
        {get_doppler_weighting_type, 1},
        {get_maximum_pixel_power, 2},
        {get_maximum_rcs, 1},
        {get_range_of_origin, 2},
        {get_doppler_of_origin, 4},
        {get_type_of_hrr, 1},
        {get_processing_mask, 1},
        {get_num_bytes_magnitude, 1},
        {get_num_bytes_phase, 1},
        {get_range_extent_pixels, 1},
        {get_range_to_nearest_edge, 4},
        {get_index_of_zero_velocity, 1},
        {get_target_radial_electrical_length, 4},
        {get_electrical_length_uncertainty, 4}],

    % Define a function to accumulate the size.
    F = fun({GetF, Size}, Acc) ->
            case hrr_exist_mask:GetF(EM) of
                1 -> Acc + Size;
                0 -> Acc
            end
        end,
    HRREM = {hrr_exist_mask:get_scatterer_magnitude(EM),
             hrr_exist_mask:get_scatterer_phase(EM),
             hrr_exist_mask:get_range_index(EM),
             hrr_exist_mask:get_doppler_index(EM)},
    % Accumulate the total size for all the included parameters (excluding
    % the scatterer records). Initial size of 5 is to allow for the existence
    % mask itself.
    HRRSize = lists:foldl(F, 5, SizeList),

    % Calculate the size for the scatterer records.
    SRSize = SRCount * scatterer_rec:payload_size(HRREM, MagnitudeByteSize, PhaseByteSize),

    % Return the combined total of the hrr and the scatterer records.
    HRRSize + SRSize.



display(HRR) ->
    io:format("****************************************~n"),
    io:format("** @hrr~n"),
    EM = HRR#hrr_segment.existence_mask,
    hrr_exist_mask:display(EM),
    io:format("Revisit index: ~p~n", [get_revisit_index(HRR)]),
    io:format("Dwell index: ~p~n", [get_dwell_index(HRR)]),
    io:format("Last dwell of revisit: ~p~n", [get_last_dwell_of_revisit(HRR)]),
    sutils:conditional_display("MTI Report Index: ~p~n",
        [get_mti_report_index(HRR)],
        hrr_exist_mask:get_mti_report_index(EM)),
    sutils:conditional_display("Number of Target Scatterers: ~p~n",
        [get_num_of_target_scatterers(HRR)],
        hrr_exist_mask:get_num_of_target_scatterers(EM)),
    sutils:conditional_display("Number of Range Samples/Total Scatterers: ~p~n",
        [get_num_of_range_samples(HRR)],
        hrr_exist_mask:get_num_of_range_samples(EM)),
    io:format("Number of Doppler Samples: ~p~n", [get_num_of_doppler_samples(HRR)]),
    sutils:conditional_display("Mean Clutter Power: ~p~n",
        [get_mean_clutter_power(HRR)],
        hrr_exist_mask:get_mean_clutter_power(EM)),
    io:format("Detection Threshold: ~p~n", [get_detection_threshold(HRR)]),
    io:format("Range Resolution: ~p~n", [get_range_resolution(HRR)]),
    io:format("Range Bin Spacing: ~p~n", [get_range_bin_spacing(HRR)]),
    io:format("Doppler Resolution: ~p~n", [get_doppler_resolution(HRR)]),
    io:format("Doppler Bin Spacing / PRF: ~p~n", [get_doppler_bin_spacing(HRR)]),
    sutils:conditional_display("Center Frequency: ~p~n",
        [get_center_frequency(HRR)],
        hrr_exist_mask:get_center_frequency(EM)),
    io:format("Compression Flag: ~p~n", [get_compression_flag(HRR)]),
    io:format("Range Weighting Function Type: ~p~n", [get_range_weighting_type(HRR)]),
    io:format("Doppler Weighting Function Type: ~p~n", [get_doppler_weighting_type(HRR)]),
    io:format("Maximum Pixel Power: ~p~n", [get_maximum_pixel_power(HRR)]),
    sutils:conditional_display("Maximum RCS: ~p~n",
        [get_maximum_rcs(HRR)],
        hrr_exist_mask:get_maximum_rcs(EM)),
    sutils:conditional_display("Range of Origin: ~p~n",
        [get_range_of_origin(HRR)],
        hrr_exist_mask:get_range_of_origin(EM)),
    sutils:conditional_display("Doppler of Origin: ~p~n",
        [get_doppler_of_origin(HRR)],
        hrr_exist_mask:get_doppler_of_origin(EM)),
    io:format("Type of HRR/RDM: ~p~n", [get_type_of_hrr(HRR)]),
    io:format("Processing Mask: ~p~n", [get_processing_mask(HRR)]),
    io:format("Number Bytes Magnitude: ~p~n", [get_num_bytes_magnitude(HRR)]),
    io:format("Number Bytes Phase: ~p~n", [get_num_bytes_phase(HRR)]),
    sutils:conditional_display("Range Extent In Pixels: ~p~n",
        [get_range_extent_pixels(HRR)],
        hrr_exist_mask:get_range_extent_pixels(EM)),
    sutils:conditional_display("Range To Nearest Edge In Chip: ~p~n",
        [get_range_to_nearest_edge(HRR)],
        hrr_exist_mask:get_range_to_nearest_edge(EM)),
    sutils:conditional_display("Index Of Zero Velocity Bin: ~p~n",
        [get_index_of_zero_velocity(HRR)],
        hrr_exist_mask:get_index_of_zero_velocity(EM)),
    sutils:conditional_display("Target Radial Electrical Length: ~p~n",
        [get_target_radial_electrical_length(HRR)],
        hrr_exist_mask:get_target_radial_electrical_length(EM)),
    sutils:conditional_display("Electrical Length Uncertainty: ~p~n",
        [get_electrical_length_uncertainty(HRR)],
        hrr_exist_mask:get_electrical_length_uncertainty(EM)).


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

get_number_of_scatterer_records(sparse_hrr_chip, _, NumOfRangeSamples, _) ->
    NumOfRangeSamples;
get_number_of_scatterer_records(_, NumOfTargetScatterers, 0, _) ->
    NumOfTargetScatterers;
get_number_of_scatterer_records(_, _, NumOfRangeSamples, NumOfDopplerSamples) ->
    NumOfRangeSamples * NumOfDopplerSamples.

