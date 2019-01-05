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
-module(pheader).

-export([
    decode/1, 
    encode/1,
    new/1,
    header_size/0,
    display/1, 
    to_csv_iolist/1,
    decode_classification/1,
    decode_us_packet_code/1,
    decode_exercise_indicator/1,
    update_size/2,
    version_id_to_float_str/1]).

-export([
    get_version_id/1,
    get_packet_size/1,
    get_nationality/1,
    get_classification/1,
    get_class_system/1,
    get_packet_code/1,
    get_exercise_indicator/1,
    get_platform_id/1,
    get_mission_id/1,
    get_job_id/1]).

%% Export of functions for unit testing.
-ifdef(TEST).
-export([
    enc_class/1, 
    encode_us_packet_code/1,
    encode_exercise_indicator/1,
    decode_class_system/1,
    encode_class_system/1]).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Record definitiions. 

-record(pheader, {
    version, 
    packet_size,
    nationality, 
    classification, 
    class_system, 
    packet_code, 
    exercise_ind, 
    platform_id, 
    mission_id, 
    job_id}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications. 

-opaque pheader() :: #pheader{}.

-export_type([pheader/0]).

-type pheader_key() :: version | packet_size | nationality | classification | 
    class_system | packet_code | exercise_ind | platform_id | mission_id | 
    job_id.

-export_type([pheader_key/0]).

-type version() :: {non_neg_integer(), non_neg_integer()}.
-export_type([version/0]).

-type classification() :: top_secret | secret | confidential | restricted | 
        unclassified | no_classification.

-export_type([classification/0]).

-type class_system() :: none | string().

-export_type([class_system/0]).

-type us_packet_code() :: none | nocontract | orcon | propin | wnintel | 
    national_only | limdis | fouo | efto | lim_off_use | noncompartment | 
    special_control | special_intel | warning_notice | rel_nato | rel_4_eyes | 
    rel_9_eyes.

-export_type([us_packet_code/0]).

-type exercise_indicator() :: operation_real | operation_simulated | 
    operation_synthesized | exercise_real | exercise_simulated | 
    exercise_synthesized.

-export_type([exercise_indicator/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Packet header decoding functions.

%% @doc Decode a Stanag 4607 packet header. 
-spec decode(binary()) -> {ok, pheader()}. 
decode(<<P1:2/binary, PktSize:32/integer-unsigned-big, 
    P3:2/binary, P4, P5:2/binary, P6:16/integer-unsigned-big, P7, 
    P8:10/binary, P9:4/binary, P10:4/binary>>) ->

    Ver = decode_version(P1),
    Nat = decode_nationality(P3),

    % Don't crash if we don't recognise the classification.
    Class = case decode_classification(P4) of
                {ok, X} -> X;
                {unknown_classification, _} -> unknown_classification
            end,

    {ok, Sys} = decode_class_system(P5), 
    {ok, Code} = decode_us_packet_code(P6),
    {ok, Ex} = decode_exercise_indicator(P7),
    PlatId = decode_platform_id(P8),
    MissId = stanag_types:i32_to_integer(P9),
    JobId = stanag_types:i32_to_integer(P10),
    
    {ok, #pheader{version = Ver, packet_size = PktSize, nationality = Nat, 
        classification = Class, class_system = Sys, packet_code = Code, 
        exercise_ind = Ex, platform_id = PlatId, mission_id = MissId, 
        job_id = JobId}}.

%% @doc Encode a packet header in its binary form.
-spec encode(PH::pheader()) -> binary().
encode(PH) ->

    % Function to encode each parameter in the list and append to an 
    % accumulated binary.
    F = fun({GetFun, EncFun}, Acc) ->
            P = GetFun(PH),
            Bin = EncFun(P),
            <<Acc/binary,Bin/binary>>
        end,

    % List of parameters and a suitable encoding function.
    ParamList = 
        [{fun get_version_id/1, fun encode_version/1},
         {fun get_packet_size/1, fun stanag_types:integer_to_i32/1},
         {fun get_nationality/1, fun encode_nationality/1},
         {fun get_classification/1, fun encode_classification/1},
         {fun get_class_system/1, fun encode_class_system/1},
         {fun get_packet_code/1, fun encode_us_packet_code/1},
         {fun get_exercise_indicator/1, fun encode_exercise_indicator/1},
         {fun get_platform_id/1, fun encode_platform_id/1},
         {fun get_mission_id/1, fun stanag_types:integer_to_i32/1},
         {fun get_job_id/1, fun stanag_types:integer_to_i32/1}],

    % Encode all of the parameters
    lists:foldl(F, <<>>, ParamList).

%% @doc Create a new packet header from the list of parameters supplied as 
%% {Key, Value} pairs.
-spec new(ParamList::[{Key,Value}]) -> pheader() when 
    Key :: pheader_key(), Value::any().
new(ParamList) ->
    % Local function to pull the parameter from the list or use a default
    % value.
    F = fun(P, L, D) ->
            case lists:keyfind(P, 1, L) of
                {P, V} -> V;
                false  -> D 
            end
        end,

    #pheader{
        version = F(version, ParamList, {3,0}),
        packet_size = F(packet_size, ParamList, 32), 
        nationality = F(nationality, ParamList, "XN"), 
        classification = F(classification, ParamList, unclassified), 
        class_system = F(class_system, ParamList, "  "), 
        packet_code = F(packet_code, ParamList, none), 
        exercise_ind = F(exercise_ind, ParamList, exercise_real), 
        platform_id = F(platform_id, ParamList, ""), 
        mission_id = F(mission_id, ParamList, 0), 
        job_id = F(job_id, ParamList, 0)}. 

%% @doc return the size of the packet header.
-spec header_size() -> non_neg_integer().
header_size() -> 32.

%% @doc Decode the version string.
-spec decode_version(binary()) -> version().
decode_version(<<M,N>>) ->
    {M - $0, N - $0}.

encode_version({M,N}) ->
    V1 = M + $0,
    V2 = N + $0,
    <<V1,V2>>.

%% @doc Convert the binary representation of nationality to a string.
-spec decode_nationality(Bin::binary()) -> string().
decode_nationality(<<X:2/binary>>) ->
    binary_to_list(X).

%% @doc Encode the two character nationality string as a binary element.
-spec encode_nationality(Nat::string()) -> binary().
encode_nationality(Nat) when length(Nat) =:= 2 ->
    list_to_binary(Nat).

%% @doc Decode the classification field
-spec decode_classification(X::non_neg_integer()) -> Res when
    Res :: {ok, classification()} | 
           {unknown_classification, non_neg_integer()}.
decode_classification(1) -> {ok, top_secret};
decode_classification(2) -> {ok, secret};
decode_classification(3) -> {ok, confidential};
decode_classification(4) -> {ok, restricted};
decode_classification(5) -> {ok, unclassified};
decode_classification(6) -> {ok, no_classification};
decode_classification(X) when X > 0 -> {unknown_classification, X}.

%% @doc Encode the classification field in its binary form.
-spec encode_classification(Class::classification()) -> binary().
encode_classification(Class) ->
    Val = enc_class(Class),
    <<Val>>.

%% @doc Map the classification value from the symbolic type to its numeric 
%% value.
-spec enc_class(Class::classification()) -> pos_integer().
enc_class(top_secret) -> 1;
enc_class(secret) -> 2;
enc_class(confidential) -> 3;
enc_class(restricted) -> 4;
enc_class(unclassified) -> 5;
% This version is not in edition 3, but often seen in older files.
enc_class(no_classification) -> 6.

%% @doc Decode the class system string (two character codes).
-spec decode_class_system(Bin::binary()) -> Res when
    Res :: {ok, class_system()}.
decode_class_system(<<"  ">>) ->
    {ok, none};
decode_class_system(<<X:2/binary>>) ->
    {ok, binary_to_list(X)}.

%% @doc Encode the class system in its binary representation.
-spec encode_class_system(CS :: none | string()) -> binary().
encode_class_system(CS) ->
    Val = enc_class_sys(CS),
    list_to_binary(Val).

%% @doc Convert the classification system value to a string. Strings must be
%% two byte values.
-spec enc_class_sys(CS :: none | string()) -> string().
enc_class_sys(none) -> "  ";
enc_class_sys(L) when length(L) =:= 2 -> L.

%% @doc Decode the US packet code.
-spec decode_us_packet_code(non_neg_integer()) -> Ret when
    Ret :: {ok, us_packet_code()} | {error, unknown_packet_code}.
decode_us_packet_code(16#0000) -> {ok, none};
decode_us_packet_code(16#0001) -> {ok, nocontract};
decode_us_packet_code(16#0002) -> {ok, orcon};
decode_us_packet_code(16#0004) -> {ok, propin};
decode_us_packet_code(16#0008) -> {ok, wnintel};
decode_us_packet_code(16#0010) -> {ok, national_only};
decode_us_packet_code(16#0020) -> {ok, limdis};
decode_us_packet_code(16#0040) -> {ok, fouo};
decode_us_packet_code(16#0080) -> {ok, efto};
decode_us_packet_code(16#0100) -> {ok, lim_off_use};
decode_us_packet_code(16#0200) -> {ok, noncompartment};
decode_us_packet_code(16#0400) -> {ok, special_control};
decode_us_packet_code(16#0800) -> {ok, special_intel};
decode_us_packet_code(16#1000) -> {ok, warning_notice};
decode_us_packet_code(16#2000) -> {ok, rel_nato};
decode_us_packet_code(16#4000) -> {ok, rel_4_eyes};
decode_us_packet_code(16#8000) -> {ok, rel_9_eyes};
decode_us_packet_code(_) -> {error, unknown_packet_code}.

%% @doc Encode the US packet code in its binary form.
-spec encode_us_packet_code(PC::us_packet_code()) -> <<_:16>>.
encode_us_packet_code(none) -> 
    <<16#0000:16>>;
encode_us_packet_code(nocontract) -> 
    <<16#0001:16>>;
encode_us_packet_code(orcon) -> 
    <<16#0002:16>>;
encode_us_packet_code(propin) -> 
    <<16#0004:16>>;
encode_us_packet_code(wnintel) -> 
    <<16#0008:16>>;
encode_us_packet_code(national_only) -> 
    <<16#0010:16>>;
encode_us_packet_code(limdis) -> 
    <<16#0020:16>>;
encode_us_packet_code(fouo) -> 
    <<16#0040:16>>;
encode_us_packet_code(efto) -> 
    <<16#0080:16>>;
encode_us_packet_code(lim_off_use) -> 
    <<16#0100:16>>;
encode_us_packet_code(noncompartment) -> 
    <<16#0200:16>>;
encode_us_packet_code(special_control) -> 
    <<16#0400:16>>;
encode_us_packet_code(special_intel) -> 
    <<16#0800:16>>;
encode_us_packet_code(warning_notice) -> 
    <<16#1000:16>>;
encode_us_packet_code(rel_nato) -> 
    <<16#2000:16>>;
encode_us_packet_code(rel_4_eyes) -> 
    <<16#4000:16>>;
encode_us_packet_code(rel_9_eyes) -> 
    <<16#8000:16>>.

%% @doc Decode the exercise indicator field. 
-spec decode_exercise_indicator(non_neg_integer()) -> Ret when
    Ret :: {ok, exercise_indicator()} | {error, reserved}.
decode_exercise_indicator(0) -> {ok, operation_real};
decode_exercise_indicator(1) -> {ok, operation_simulated};
decode_exercise_indicator(2) -> {ok, operation_synthesized};
decode_exercise_indicator(128) -> {ok, exercise_real};
decode_exercise_indicator(129) -> {ok, exercise_simulated};
decode_exercise_indicator(130) -> {ok, exercise_synthesized};
decode_exercise_indicator(_) -> {error, reserved}.

%% @doc Encode the exercise indicator field.
-spec encode_exercise_indicator(Ind::exercise_indicator()) -> <<_:8>>.
encode_exercise_indicator(operation_real) -> <<0>>;
encode_exercise_indicator(operation_simulated) -> <<1>>;
encode_exercise_indicator(operation_synthesized) -> <<2>>;
encode_exercise_indicator(exercise_real) -> <<128>>;
encode_exercise_indicator(exercise_simulated) -> <<129>>;
encode_exercise_indicator(exercise_synthesized) -> <<130>>.

%% @doc Decode the platform ID field. This simply strips trailing spaces from
%% the binary and returns a string.
-spec decode_platform_id(binary()) -> string().
decode_platform_id(<<X:10/binary>>) ->
    sutils:trim_trailing_spaces(binary_to_list(X)).

%% @doc Encode the platform ID field. This pads the string with spaces and
%% returns a binary of the correct length.
-spec encode_platform_id(PlatID::string()) -> binary().
encode_platform_id(PlatID) when is_list(PlatID), length(PlatID) =< 10 ->
    Pad = sutils:add_trailing_spaces(PlatID, 10),
    list_to_binary(Pad).

%% @doc Display the contents of a decoded packet header.
-spec display(PktHdr::pheader()) -> ok.
display(PktHdr) ->
    io:format("****************************************~n"),
    io:format("** @pheader~n"),
    io:format("Version: ~s~n", [version_id_to_float_str(get_version_id(PktHdr))]),
    io:format("Packet size: ~p~n", [get_packet_size(PktHdr)]), 
    io:format("Nationality: ~p~n", [get_nationality(PktHdr)]),
    io:format("Classification: ~p~n", [get_classification(PktHdr)]),
    io:format("Classification System: ~p~n", [get_class_system(PktHdr)]),
    io:format("Packet code: ~p~n", [get_packet_code(PktHdr)]),
    io:format("Exercise Indication: ~p~n", [get_exercise_indicator(PktHdr)]),
    io:format("Platform ID: ~p~n", [get_platform_id(PktHdr)]),
    io:format("Mission ID: ~p~n", [get_mission_id(PktHdr)]),
    io:format("Job ID: ~p~n", [get_job_id(PktHdr)]).

%% @doc Convert to CSV form as an iolist for output to file.
-spec to_csv_iolist(PktHdr::pheader()) -> iolist().
to_csv_iolist(PktHdr) ->
    Args = [version_id_to_float_str(get_version_id(PktHdr)),
            get_packet_size(PktHdr),
            get_nationality(PktHdr),
            get_classification(PktHdr),
            get_class_system(PktHdr),
            get_packet_code(PktHdr),
            get_exercise_indicator(PktHdr),
            get_platform_id(PktHdr),
            get_mission_id(PktHdr),
            get_job_id(PktHdr)],
    io_lib:format("PH,~s,~p,~s,~p,~s,~p,~p,~s,~p,~p~n", Args). 

%% @doc Update the size field in a packet header.
-spec update_size(Hdr::pheader(), NewSize::non_neg_integer()) -> pheader().
update_size(#pheader{} = Hdr, NewSize) when NewSize >= 0 ->
    Hdr#pheader{packet_size = NewSize}.

%% @doc Convert the version ID tuple to a string showing a float.
-spec version_id_to_float_str({Maj,Min}) -> string() when
    Maj :: non_neg_integer(), Min :: non_neg_integer().
version_id_to_float_str({Maj,Min}) -> 
    io_lib:format("~p.~p", [Maj, Min]).

%% @doc Get the version ID from a packet header
-spec get_version_id(PH::pheader()) -> version().
get_version_id(#pheader{version = V}) -> V.

%% @doc Get the packet size from the header. 
-spec get_packet_size(PH::pheader()) -> non_neg_integer().
get_packet_size(#pheader{packet_size = S}) -> S.

%% @doc Get the nationality from a header structure.
-spec get_nationality(PH::pheader()) -> string().
get_nationality(#pheader{nationality = N}) -> N.

%% @doc Get the classification level
-spec get_classification(PH::pheader()) -> classification().
get_classification(#pheader{classification = C}) -> C.

%% @doc Get the classification system from the header. 
-spec get_class_system(PH::pheader()) -> class_system().
get_class_system(#pheader{class_system = X}) -> X.

%% @doc Get the packet security code from the header.
get_packet_code(#pheader{packet_code = X}) -> X.

%% @doc Get the exercise indicator from the header structure.
get_exercise_indicator(#pheader{exercise_ind = X}) -> X.

%% @doc Get the platform ID from the header structure.
get_platform_id(#pheader{platform_id = X}) -> X.

%% @doc Get the mission ID from the header structure.
get_mission_id(#pheader{mission_id = X}) -> X.

%% @doc Get the job ID from the header structure.
get_job_id(#pheader{job_id = X}) -> X.


