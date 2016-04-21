-module(scatterer_rec).

-export([
    decode/4,
    new/1,
    payload_size/3,
    get_scatterer_magnitude/1,
    get_scatterer_phase/1,
    get_range_index/1,
    get_doppler_index/1
    ]).

-record(hrr_scatterer_record, {
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

    {ok, #hrr_scatterer_record{
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

%% Create a new HRR Scatterer Record with a set of parameters
%% provided as a list of [{param_name, value}] tuples.
new(RepParams) ->
    % Local function to pull the parameter from the list or supply a default
    % value.
    F = fun(P, L) ->
            case lists:keyfind(P, 1, L) of
                {P, V} -> V;
                false  -> 0
            end
        end,

    #hrr_scatterer_record{
        %% The scatterer_magnitude is mandatory
        scatterer_magnitude = F(scatterer_magnitude, RepParams),
        scatterer_phase = F(scatterer_phase, RepParams),
        range_index = F(range_index, RepParams),
        doppler_index = F(doppler_index, RepParams)}.

%% Calculate the size in bytes of a scatterer record, depending upon
%% which fields have been set in the existence mask and the size of
%% scatterer_magnitude and scatterer_phase.
payload_size(EM, MagnitudeByteSize, PhaseByteSize) ->

    {H32_1, H32_2, H32_3, H32_4} = EM,

    SizeList = [
        {H32_1, MagnitudeByteSize},
        {H32_2, PhaseByteSize},
        {H32_3, 2},
        {H32_4, 2}],

    F = fun({M, Size}, Acc) ->
            case M of
                1 -> Acc + Size;
                0 -> Acc
            end
        end,

    % Accumulate the total size for all the included parameters.
    lists:foldl(F, 0, SizeList).

%% Accessor functions to allow clients to read the individual record fields.
get_scatterer_magnitude(#hrr_scatterer_record{scatterer_magnitude = X}) -> X.
get_scatterer_phase(#hrr_scatterer_record{scatterer_phase = X}) -> X.
get_range_index(#hrr_scatterer_record{range_index = X}) -> X.
get_doppler_index(#hrr_scatterer_record{doppler_index = X}) -> X.
