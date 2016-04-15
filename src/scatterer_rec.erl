-module(scatterer_rec).

-export([
    decode/4]).

-record(hrr_scatter_record, {
    scatterer_magnitude,
    scatterer_phase,
    range_index,
    doppler_index
    }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Scatterer record decoding functions.

decode(<<Record/binary>>, EM, MagnitudeByteSize, PhaseByteSize) ->

    <<1:1, H32_2:1, H32_3:1, H32_4:1>> = EM,

    {ok, ScattererMagnitude, Bin1} = decode_scatterer_magnitude(
        Record,
        MagnitudeByteSize),

    {ScattererPhase, Bin2} = decode_scatterer_phase(
        Bin1,
        H32_2,
        PhaseByteSize),

    {RangeIndex, Bin3} = sutils:conditional_extract(
        Bin2,
        H32_3,
        2,
        fun stanag_types:i16_to_integer/1,
        0),

    {DopplerIndex, _} = sutils:conditional_extract(
        Bin3,
        H32_4,
        2,
        fun stanag_types:i16_to_integer/1,
        0),

    {ok, #hrr_scatter_record{
        scatterer_magnitude = ScattererMagnitude,
        scatterer_phase = ScattererPhase,
        range_index = RangeIndex,
        doppler_index = DopplerIndex
    }}.

decode_scatterer_magnitude(<<Record/binary>>, MagnitudeByteSize) ->
    case MagnitudeByteSize of
        1 ->
            sutils:extract_conv_data(
                Record, 1, fun stanag_types:i8_to_integer/1);
        2 ->
            sutils:extract_conv_data(
                Record, 2, fun stanag_types:i16_to_integer/1)
    end.

decode_scatterer_phase(<<Record/binary>>, H32_2, PhaseByteSize) ->
    case PhaseByteSize of
        0 ->
            {PhaseByteSize, Record};
        1 ->
            sutils:conditional_extract(
                Record,
                H32_2,
                1,
                fun stanag_types:i8_to_integer/1,
                0);
        2 ->
            sutils:conditional_extract(
                Record,
                H32_2,
                2,
                fun stanag_types:i16_to_integer/1,
                0)
    end.
