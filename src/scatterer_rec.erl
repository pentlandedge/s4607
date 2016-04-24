-module(scatterer_rec).

-export([
    decode/4,
    encode/4,
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

decode(<<Record/binary>>, {1, H32_2, H32_3, H32_4}, MagnitudeByteSize, PhaseByteSize) ->

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

%% Encode a HRR Scatterer Record binary from the specified record.
encode(#hrr_scatterer_record{scatterer_magnitude = ScattererMagnitude,
        scatterer_phase = ScattererPhase, range_index = RangeIndex,
        doppler_index = DopplerIndex},
        {1, H32_2, H32_3, H32_4},
        MagnitudeByteSize, PhaseByteSize) ->

    F = fun({Param, ExistMask, EncFun}, Acc) ->
            case ExistMask of
                1 ->
                     PB = EncFun(Param),
                    <<Acc/binary,PB/binary>>;
                0 ->
                    Acc
            end
        end,

    ParamTable = [
        {{ScattererMagnitude, MagnitudeByteSize}, 1,
            fun encode_scatterer_magnitude/1},
        {{ScattererPhase, PhaseByteSize}, H32_2, fun encode_scatterer_phase/1},
        {RangeIndex, H32_3, fun stanag_types:integer_to_i16/1},
        {DopplerIndex, H32_4, fun stanag_types:integer_to_i16/1}],

    lists:foldl(F, <<>>, ParamTable).

encode_scatterer_magnitude({ScattererMagnitude, MagnitudeByteSize}) ->
    case MagnitudeByteSize of
        1 ->
            stanag_types:integer_to_i8(ScattererMagnitude);
        2 ->
            stanag_types:integer_to_i16(ScattererMagnitude)
    end.

encode_scatterer_phase({ScattererPhase, PhaseByteSize}) ->
    case PhaseByteSize of
        0 ->
            <<>>;
        1 ->
            stanag_types:integer_to_i8(ScattererPhase);
        2 ->
            stanag_types:integer_to_i16(ScattererPhase)
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
