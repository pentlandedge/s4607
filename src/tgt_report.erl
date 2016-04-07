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
-module(tgt_report).

-export([
    decode/2, 
    encode/2,
    new/1,
    payload_size/1,
    to_dict/2,
    display/2,
    get_mti_report_index/1,
    get_target_hr_lat/1,
    get_target_hr_lon/1,
    get_target_delta_lat/1,
    get_target_delta_lon/1,
    get_geodetic_height/1,
    get_target_vel_los/1,
    get_target_wrap_velocity/1,
    get_target_snr/1,
    get_target_classification/1,
    get_target_class_prob/1,
    get_target_slant_range_unc/1,
    get_target_cross_range_unc/1,
    get_target_height_unc/1,
    get_target_rad_vel_unc/1,
    get_truth_tag_app/1,
    get_truth_tag_entity/1,
    get_target_rcs/1]).

%% Export of functions for unit testing.
-ifdef(TEST).
-export([
    decode_target_classification/1,
    encode_target_classification/1]).
-endif.


-record(tgt_report, {
    mti_report_index,
    target_hr_lat,
    target_hr_lon,
    target_delta_lat,
    target_delta_lon,
    geodetic_height,
    target_vel_los,
    target_wrap_velocity,
    target_snr,
    target_classification,
    target_class_prob,
    target_slant_range_unc,
    target_cross_range_unc,
    target_height_unc,
    target_rad_vel_unc,
    truth_tag_app,
    truth_tag_entity,
    target_rcs}).

decode(TrBin, EM) ->
    
    {MRI, Rem1} = sutils:conditional_extract(
        TrBin, 
        exist_mask:get_mti_report_index(EM), 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtHiResLat, Rem2} = sutils:conditional_extract(
        Rem1, 
        exist_mask:get_target_hr_lat(EM), 
        4, 
        fun stanag_types:sa32_to_float/1, 
        0.0),

    {TgtHiResLon, Rem3} = sutils:conditional_extract(
        Rem2, 
        exist_mask:get_target_hr_lon(EM), 
        4, 
        fun stanag_types:ba32_to_float/1, 
        0.0),

    {TgtDeltaLat, Rem4} = sutils:conditional_extract(
        Rem3, 
        exist_mask:get_target_delta_lat(EM), 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {TgtDeltaLon, Rem5} = sutils:conditional_extract(
        Rem4, 
        exist_mask:get_target_delta_lon(EM), 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {GeodHeight, Rem6} = sutils:conditional_extract(
        Rem5, 
        exist_mask:get_geodetic_height(EM), 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {TgtVelLos, Rem7} = sutils:conditional_extract(
        Rem6, 
        exist_mask:get_target_vel_los(EM), 
        2, 
        fun stanag_types:s16_to_integer/1, 
        0),

    {TgtWrapVel, Rem8} = sutils:conditional_extract(
        Rem7, 
        exist_mask:get_target_wrap_velocity(EM), 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtSnr, Rem9} = sutils:conditional_extract(
        Rem8, 
        exist_mask:get_target_snr(EM), 
        1, 
        fun stanag_types:s8_to_integer/1, 
        0),

    {TgtClassification, Rem10} = sutils:conditional_extract(
        Rem9, 
        exist_mask:get_target_classification(EM), 
        1, 
        fun decode_target_classification/1, 
        no_information_live_target),

    {TgtClassProb, Rem11} = sutils:conditional_extract(
        Rem10, 
        exist_mask:get_target_class_prob(EM), 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {TgtSlantRgeUnc, Rem12} = sutils:conditional_extract(
        Rem11, 
        exist_mask:get_target_slant_range_unc(EM), 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtCrossRgeUnc, Rem13} = sutils:conditional_extract(
        Rem12, 
        exist_mask:get_target_cross_range_unc(EM), 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TgtHeightUnc, Rem14} = sutils:conditional_extract(
        Rem13, 
        exist_mask:get_target_height_unc(EM), 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {TgtRadVelUnc, Rem15} = sutils:conditional_extract(
        Rem14, 
        exist_mask:get_target_rad_vel_unc(EM), 
        2, 
        fun stanag_types:i16_to_integer/1, 
        0),

    {TruthTagApp, Rem16} = sutils:conditional_extract(
        Rem15, 
        exist_mask:get_truth_tag_app(EM), 
        1, 
        fun stanag_types:i8_to_integer/1, 
        0),

    {TruthTagEnt, Rem17} = sutils:conditional_extract(
        Rem16, 
        exist_mask:get_truth_tag_entity(EM), 
        4, 
        fun stanag_types:i32_to_integer/1, 
        0),

    {TgtRcs, Rem18} = sutils:conditional_extract(
        Rem17, 
        exist_mask:get_target_rcs(EM), 
        1, 
        fun stanag_types:s8_to_integer/1, 
        0),
   
    TR = #tgt_report{
        mti_report_index = MRI,
        target_hr_lat = TgtHiResLat,
        target_hr_lon = TgtHiResLon,
        target_delta_lat = TgtDeltaLat,
        target_delta_lon = TgtDeltaLon,
        geodetic_height = GeodHeight,
        target_vel_los = TgtVelLos,
        target_wrap_velocity = TgtWrapVel,
        target_snr = TgtSnr,
        target_classification = TgtClassification,
        target_class_prob = TgtClassProb,
        target_slant_range_unc = TgtSlantRgeUnc,
        target_cross_range_unc = TgtCrossRgeUnc,
        target_height_unc = TgtHeightUnc,
        target_rad_vel_unc = TgtRadVelUnc,
        truth_tag_app = TruthTagApp,
        truth_tag_entity = TruthTagEnt,
        target_rcs = TgtRcs},
        
    {ok, TR, Rem18}.

%% Function to encode a target report binary from the specified record. 
%% Uses the existence mask to decide which fields to use.
encode(#tgt_report{
    mti_report_index = MRI,
    target_hr_lat = TgtHiResLat,
    target_hr_lon = TgtHiResLon,
    target_delta_lat = TgtDeltaLat,
    target_delta_lon = TgtDeltaLon,
    geodetic_height = GeodHeight,
    target_vel_los = TgtVelLos,
    target_wrap_velocity = TgtWrapVel,
    target_snr = TgtSnr,
    target_classification = TgtClassification,
    target_class_prob = TgtClassProb,
    target_slant_range_unc = TgtSlantRgeUnc,
    target_cross_range_unc = TgtCrossRgeUnc,
    target_height_unc = TgtHeightUnc,
    target_rad_vel_unc = TgtRadVelUnc,
    truth_tag_app = TruthTagApp,
    truth_tag_entity = TruthTagEnt,
    target_rcs = TgtRcs} = TR, EM) ->
   
    % Create a local function to wrap the check of the existence mask and 
    % the parameter encoding/appending.
    F = fun({Param, EvalFun, EncFun}, Acc) ->
            case exist_mask:EvalFun(EM) of 
                1 -> 
                    PB = EncFun(Param),
                    <<Acc/binary,PB/binary>>;
                0 ->
                    Acc 
            end
        end,

    ParamTable = [
        {MRI, get_mti_report_index, fun stanag_types:integer_to_i16/1},
        {TgtHiResLat, get_target_hr_lat, fun stanag_types:float_to_sa32/1},
        {TgtHiResLon, get_target_hr_lon, fun stanag_types:float_to_ba32/1}, 
        {TgtDeltaLat, get_target_delta_lat, fun stanag_types:integer_to_s16/1}, 
        {TgtDeltaLon, get_target_delta_lon, fun stanag_types:integer_to_s16/1}, 
        {GeodHeight, get_geodetic_height, fun stanag_types:integer_to_s16/1}, 
        {TgtVelLos, get_target_vel_los, fun stanag_types:integer_to_s16/1}, 
        {TgtWrapVel, get_target_wrap_velocity, fun stanag_types:integer_to_i16/1}, 
        {TgtSnr, get_target_snr, fun stanag_types:integer_to_s8/1}, 
        {TgtClassification, get_target_classification, fun encode_target_classification/1}, 
        {TgtClassProb, get_target_class_prob, fun stanag_types:integer_to_i8/1}, 
        {TgtSlantRgeUnc, get_target_slant_range_unc, fun stanag_types:integer_to_i16/1}, 
        {TgtCrossRgeUnc, get_target_cross_range_unc, fun stanag_types:integer_to_i16/1}, 
        {TgtHeightUnc, get_target_height_unc, fun stanag_types:integer_to_i8/1}, 
        {TgtRadVelUnc, get_target_rad_vel_unc, fun stanag_types:integer_to_i16/1}, 
        {TruthTagApp, get_truth_tag_app, fun stanag_types:integer_to_i8/1}, 
        {TruthTagEnt, get_truth_tag_entity, fun stanag_types:integer_to_i32/1}, 
        {TgtRcs, get_target_rcs, fun stanag_types:integer_to_s8/1}], 

    lists:foldl(F, <<>>, ParamTable).

%% Function to allow the creation of a new target report with a set of 
%% parameters provided as a list of [{param_name, value}] tuples.
new(RepParams) ->
    % Local function to pull the parameter from the list or supply a default
    % value.
    F = fun(P, L) ->
            case lists:keyfind(P, 1, L) of
                {P, V} -> V;
                false  -> 0
            end
        end,

    #tgt_report{
        mti_report_index = F(mti_report_index, RepParams),
        target_hr_lat = F(target_hr_lat, RepParams),
        target_hr_lon = F(target_hr_lon, RepParams),
        target_delta_lat = F(target_delta_lat, RepParams),
        target_delta_lon = F(target_delta_lon, RepParams),
        geodetic_height = F(geodetic_height, RepParams),
        target_vel_los = F(target_vel_los, RepParams),
        target_wrap_velocity = F(target_wrap_velocity, RepParams),
        target_snr = F(target_snr, RepParams),
        target_classification = F(target_classification, RepParams),
        target_class_prob = F(target_class_prob, RepParams),
        target_slant_range_unc = F(target_slant_range_unc, RepParams),
        target_cross_range_unc = F(target_cross_range_unc, RepParams),
        target_height_unc = F(target_height_unc, RepParams),
        target_rad_vel_unc = F(target_rad_vel_unc, RepParams),
        truth_tag_app = F(truth_tag_app, RepParams),
        truth_tag_entity = F(truth_tag_entity, RepParams),
        target_rcs = F(target_rcs, RepParams)}.

%% Function to calculate the size in bytes of a target report, depending upon
%% which fields have been set in the existence mask.
payload_size(EM) ->
    
    SizeList = [
        {fun exist_mask:get_mti_report_index/1, 2},
        {fun exist_mask:get_target_hr_lat/1, 4},
        {fun exist_mask:get_target_hr_lon/1, 4},
        {fun exist_mask:get_target_delta_lat/1, 2},
        {fun exist_mask:get_target_delta_lon/1, 2},
        {fun exist_mask:get_geodetic_height/1, 2},
        {fun exist_mask:get_target_vel_los/1, 2},
        {fun exist_mask:get_target_wrap_velocity/1, 2},
        {fun exist_mask:get_target_snr/1, 1},
        {fun exist_mask:get_target_classification/1, 1},
        {fun exist_mask:get_target_class_prob/1, 1},
        {fun exist_mask:get_target_slant_range_unc/1, 2},
        {fun exist_mask:get_target_cross_range_unc/1, 2},
        {fun exist_mask:get_target_height_unc/1, 1},
        {fun exist_mask:get_target_rad_vel_unc/1, 2},
        {fun exist_mask:get_truth_tag_app/1, 1},
        {fun exist_mask:get_truth_tag_entity/1, 4},
        {fun exist_mask:get_target_rcs/1, 1}],
 
    % Define a function to accumulate the size.
    F = fun({GetF, Size}, Acc) ->
            case GetF(EM) of
                1 -> Acc + Size;
                0 -> Acc
            end
        end,

    % Accumulate the total size for all the included parameters.
    lists:foldl(F, 0, SizeList).

%% Function to convert a target report to a dict. Uses the existence mask to
%% add only the valid parameters.
to_dict(#tgt_report{
    mti_report_index = MRI,
    target_hr_lat = TgtHiResLat,
    target_hr_lon = TgtHiResLon,
    target_delta_lat = TgtDeltaLat,
    target_delta_lon = TgtDeltaLon,
    geodetic_height = GeodHeight,
    target_vel_los = TgtVelLos,
    target_wrap_velocity = TgtWrapVel,
    target_snr = TgtSnr,
    target_classification = TgtClassification,
    target_class_prob = TgtClassProb,
    target_slant_range_unc = TgtSlantRgeUnc,
    target_cross_range_unc = TgtCrossRgeUnc,
    target_height_unc = TgtHeightUnc,
    target_rad_vel_unc = TgtRadVelUnc,
    truth_tag_app = TruthTagApp,
    truth_tag_entity = TruthTagEnt,
    target_rcs = TgtRcs} = TR, EM) ->

    % Define a table of the form [{A, C, V}] where A is an atom field name,
    % C is an existence mask checker function, and V is the value of the 
    % field.
    ParamTable = [
        {mti_report_index, fun exist_mask:get_mti_report_index/1, MRI},
        {target_hr_lat, fun exist_mask:get_target_hr_lat/1, TgtHiResLat},
        {target_hr_lon, fun exist_mask:get_target_hr_lon/1, TgtHiResLon},
        {target_delta_lat, fun exist_mask:get_target_delta_lat/1, TgtDeltaLat},
        {target_delta_lon, fun exist_mask:get_target_delta_lon/1, TgtDeltaLon},
        {geodetic_height, fun exist_mask:get_geodetic_height/1, GeodHeight},
        {target_vel_los, fun exist_mask:get_target_vel_los/1, TgtVelLos},
        {target_wrap_velocity, fun exist_mask:get_target_wrap_velocity/1, TgtWrapVel},
        {target_snr, fun exist_mask:get_target_snr/1, TgtSnr},
        {target_classification, fun exist_mask:get_target_classification/1, TgtClassification},
        {target_class_prob, fun exist_mask:get_target_class_prob/1, TgtClassProb},
        {target_slant_range_unc, fun exist_mask:get_target_slant_range_unc/1, TgtSlantRgeUnc},
        {target_cross_range_unc, fun exist_mask:get_target_cross_range_unc/1, TgtCrossRgeUnc},
        {target_height_unc, fun exist_mask:get_target_height_unc/1, TgtHeightUnc},
        {target_rad_vel_unc, fun exist_mask:get_target_rad_vel_unc/1, TgtRadVelUnc},
        {truth_tag_app, fun exist_mask:get_truth_tag_app/1, TruthTagApp},
        {truth_tag_entity, fun exist_mask:get_truth_tag_entity/1, TruthTagEnt},
        {target_rcs, fun exist_mask:get_target_rcs/1, TgtRcs}],

    % Define a local function to convert all of the available parameters to 
    % a dictionary.

    F = fun({At, GetF, Val}, Acc) ->
            case GetF(EM) of
                1 -> dict:store(At, Val, Acc);
                0 -> Acc
            end
        end,

    lists:foldl(F, dict:new(), ParamTable).

decode_target_classification(<<Val:8>>) ->
    decode_target_classification(Val);
    
decode_target_classification(0) -> no_information_live_target;
decode_target_classification(1) -> tracked_vehicle_live_target;
decode_target_classification(2) -> wheeled_vehicle_live_target;
decode_target_classification(3) -> rotary_wing_aircraft_live_target;
decode_target_classification(4) -> fixed_wing_aircraft_live_target;
decode_target_classification(5) -> stationary_rotator_live_target;
decode_target_classification(6) -> maritime_live_target;
decode_target_classification(7) -> beacon_live_target;
decode_target_classification(8) -> amphibious_live_target;
decode_target_classification(9) -> person_live_target;
decode_target_classification(10) -> vehicle_live_target;
decode_target_classification(11) -> animal_live_target;
decode_target_classification(12) -> large_multiple_return_live_land_target;
decode_target_classification(13) -> large_multiple_return_live_maritime_target;
decode_target_classification(126) -> other_live_target;
decode_target_classification(127) -> unknown_live_target;
decode_target_classification(128) -> no_information_simulated_target;
decode_target_classification(129) -> tracked_vehicle_simulated_target;
decode_target_classification(130) -> wheeled_vehicle_simulated_target;
decode_target_classification(131) -> rotary_wing_aircraft_simulated_target;
decode_target_classification(132) -> fixed_wing_aircraft_simulated_target;
decode_target_classification(133) -> stationary_rotator_simulated_target;
decode_target_classification(134) -> maritime_simulated_target;
decode_target_classification(135) -> beacon_simulated_target;
decode_target_classification(136) -> amphibious_simulated_target;
decode_target_classification(137) -> person_simulated_target;
decode_target_classification(138) -> vehicle_simulated_target;
decode_target_classification(139) -> animal_simulated_target;
decode_target_classification(140) -> large_multiple_return_simulated_land_target;
decode_target_classification(141) -> large_multiple_return_simulated_maritime_target;
decode_target_classification(142) -> tagging_device;
decode_target_classification(254) -> other_simulated_target;
decode_target_classification(255) -> unknown_simulated_target;
decode_target_classification(_) -> reserved.

%% Function to convert a target classification to the binary representation.
encode_target_classification(X) when is_atom(X) ->
    Val = etc(X),
    <<Val>>.

%% Helper functions with the encode mapping.
etc(no_information_live_target) -> 0;
etc(tracked_vehicle_live_target) -> 1;
etc(wheeled_vehicle_live_target) -> 2;
etc(rotary_wing_aircraft_live_target) -> 3;
etc(fixed_wing_aircraft_live_target) -> 4;
etc(stationary_rotator_live_target) -> 5;
etc(maritime_live_target) -> 6;
etc(beacon_live_target) -> 7;
etc(amphibious_live_target) -> 8;
etc(person_live_target) -> 9;
etc(vehicle_live_target) -> 10;
etc(animal_live_target) -> 11;
etc(large_multiple_return_live_land_target) -> 12;
etc(large_multiple_return_live_maritime_target) -> 13;
etc(other_live_target) -> 126;
etc(unknown_live_target) -> 127;
etc(no_information_simulated_target) -> 128;
etc(tracked_vehicle_simulated_target) -> 129;
etc(wheeled_vehicle_simulated_target) -> 130;
etc(rotary_wing_aircraft_simulated_target) -> 131;
etc(fixed_wing_aircraft_simulated_target) -> 132;
etc(stationary_rotator_simulated_target) -> 133;
etc(maritime_simulated_target) -> 134;
etc(beacon_simulated_target) -> 135;
etc(amphibious_simulated_target) -> 136;
etc(person_simulated_target) -> 137;
etc(vehicle_simulated_target) -> 138;
etc(animal_simulated_target) -> 139;
etc(large_multiple_return_simulated_land_target) -> 140;
etc(large_multiple_return_simulated_maritime_target) -> 141;
etc(tagging_device) -> 142;
etc(other_simulated_target) -> 254;
etc(unknown_simulated_target) -> 255.
 
display(TR, EM) ->
    io:format("****************************************~n"),
    io:format("** @tgt_report~n"),
    sutils:conditional_display("MTI report index: ~p~n", 
        [get_mti_report_index(TR)], 
        exist_mask:get_mti_report_index(EM)),
    sutils:conditional_display("Target HR Lat: ~p~n", 
        [get_target_hr_lat(TR)], 
        exist_mask:get_target_hr_lat(EM)),
    sutils:conditional_display("Target HR Lon: ~p~n", 
        [get_target_hr_lon(TR)], 
        exist_mask:get_target_hr_lon(EM)),
    sutils:conditional_display("Target Delta Lat: ~p~n", 
        [get_target_delta_lat(TR)], 
        exist_mask:get_target_delta_lat(EM)),
    sutils:conditional_display("Target Delta Lon: ~p~n", 
        [get_target_delta_lon(TR)], 
        exist_mask:get_target_delta_lon(EM)),
    sutils:conditional_display("Geodetic Height: ~p~n", 
        [get_geodetic_height(TR)], 
        exist_mask:get_geodetic_height(EM)),
    sutils:conditional_display("Target vel. LOS.: ~p~n", 
        [get_target_vel_los(TR)], 
        exist_mask:get_target_vel_los(EM)),
    sutils:conditional_display("Target wrap vel.: ~p~n", 
        [get_target_wrap_velocity(TR)], 
        exist_mask:get_target_wrap_velocity(EM)),
    sutils:conditional_display("Target SNR: ~p~n", 
        [get_target_snr(TR)], 
        exist_mask:get_target_snr(EM)),
    sutils:conditional_display("Target classification: ~p~n", 
        [get_target_classification(TR)], 
        exist_mask:get_target_classification(EM)),
    sutils:conditional_display("Target classification probability: ~p~n", 
        [get_target_class_prob(TR)], 
        exist_mask:get_target_class_prob(EM)),
    sutils:conditional_display("Target slant range unc.: ~p~n", 
        [get_target_slant_range_unc(TR)], 
        exist_mask:get_target_slant_range_unc(EM)),
    sutils:conditional_display("Target cross range unc.: ~p~n", 
        [get_target_cross_range_unc(TR)], 
        exist_mask:get_target_cross_range_unc(EM)),
    sutils:conditional_display("Target height unc.: ~p~n", 
        [get_target_height_unc(TR)], 
        exist_mask:get_target_height_unc(EM)),
    sutils:conditional_display("Target rad. vel. unc.: ~p~n", 
        [get_target_rad_vel_unc(TR)], 
        exist_mask:get_target_rad_vel_unc(EM)),
    sutils:conditional_display("Truth tag application: ~p~n", 
        [get_truth_tag_app(TR)], 
        exist_mask:get_truth_tag_app(EM)),
    sutils:conditional_display("Truth tag entity: ~p~n", 
        [get_truth_tag_entity(TR)], 
        exist_mask:get_truth_tag_entity(EM)),
    sutils:conditional_display("Target RCS: ~p~n", 
        [get_target_rcs(TR)], 
        exist_mask:get_target_rcs(EM)).

%% Accessor functions to allow clients to read the individual record fields.
get_mti_report_index(#tgt_report{mti_report_index = X}) -> X.
get_target_hr_lat(#tgt_report{target_hr_lat = X}) -> X.
get_target_hr_lon(#tgt_report{target_hr_lon = X}) -> X.
get_target_delta_lat(#tgt_report{target_delta_lat = X}) -> X.
get_target_delta_lon(#tgt_report{target_delta_lon = X}) -> X.
get_geodetic_height(#tgt_report{geodetic_height = X}) -> X.
get_target_vel_los(#tgt_report{target_vel_los = X}) -> X.
get_target_wrap_velocity(#tgt_report{target_wrap_velocity = X}) -> X.
get_target_snr(#tgt_report{target_snr = X}) -> X.
get_target_classification(#tgt_report{target_classification = X}) -> X.
get_target_class_prob(#tgt_report{target_class_prob = X}) -> X.
get_target_slant_range_unc(#tgt_report{target_slant_range_unc = X}) -> X.
get_target_cross_range_unc(#tgt_report{target_cross_range_unc = X}) -> X.
get_target_height_unc(#tgt_report{target_height_unc = X}) -> X.
get_target_rad_vel_unc(#tgt_report{target_rad_vel_unc = X}) -> X.
get_truth_tag_app(#tgt_report{truth_tag_app = X}) -> X.
get_truth_tag_entity(#tgt_report{truth_tag_entity = X}) -> X.
get_target_rcs(#tgt_report{target_rcs = X}) -> X.


