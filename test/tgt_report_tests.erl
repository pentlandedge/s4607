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

-module(tgt_report_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for target reports. 
tgt_report_test_() ->
    [creation_checks1(), encode_decode_checks(), payload_size_check1(),
     payload_size_check2(), decode_classification_checks(), 
     encode_classification_checks(), dict_conversion_checks()].

creation_checks1() ->
    % Create a target report and check all the fields.
    {_, R1} = sample_report(),

    [?_assertEqual(34, tgt_report:get_mti_report_index(R1)),
     ?_assert(almost_equal(-33.3, tgt_report:get_target_hr_lat(R1), 0.0000001)),
     ?_assert(almost_equal(357.57, tgt_report:get_target_hr_lon(R1), 0.0000001)),
     ?_assertEqual(46, tgt_report:get_target_delta_lon(R1)),
     ?_assertEqual(5000, tgt_report:get_geodetic_height(R1)),
     ?_assertEqual(32000, tgt_report:get_target_vel_los(R1)),
     ?_assertEqual(40000, tgt_report:get_target_wrap_velocity(R1)),
     ?_assertEqual(-128, tgt_report:get_target_snr(R1)),
     ?_assertEqual(vehicle_live_target, tgt_report:get_target_classification(R1)),
     ?_assertEqual(90, tgt_report:get_target_class_prob(R1)),
     ?_assertEqual(1000, tgt_report:get_target_slant_range_unc(R1)),
     ?_assertEqual(2000, tgt_report:get_target_cross_range_unc(R1)),
     ?_assertEqual(200, tgt_report:get_target_height_unc(R1)),
     ?_assertEqual(5000, tgt_report:get_target_rad_vel_unc(R1)),
     ?_assertEqual(200, tgt_report:get_truth_tag_app(R1)),
     ?_assertEqual(10000, tgt_report:get_truth_tag_entity(R1)),
     ?_assertEqual(10, tgt_report:get_target_rcs(R1))].

%% Checks of the encode/decode functions.
encode_decode_checks() ->
    % Create a sample report. 
    {EM, R1} = sample_report(),

    % Encode it
    Bin = tgt_report:encode(R1, EM),

    % Decode it again 
    {ok, TR, Rem} = tgt_report:decode(Bin, EM),  

    [?_assertEqual(34, tgt_report:get_mti_report_index(TR)),
     ?_assert(almost_equal(-33.3, tgt_report:get_target_hr_lat(TR), 0.0000001)),
     ?_assert(almost_equal(357.57, tgt_report:get_target_hr_lon(TR), 0.0000001)),
     ?_assertEqual(46, tgt_report:get_target_delta_lon(TR)),
     ?_assertEqual(5000, tgt_report:get_geodetic_height(TR)),
     ?_assertEqual(32000, tgt_report:get_target_vel_los(TR)),
     ?_assertEqual(40000, tgt_report:get_target_wrap_velocity(TR)),
     ?_assertEqual(-128, tgt_report:get_target_snr(TR)),
     ?_assertEqual(vehicle_live_target, tgt_report:get_target_classification(TR)),
     ?_assertEqual(90, tgt_report:get_target_class_prob(TR)),
     ?_assertEqual(1000, tgt_report:get_target_slant_range_unc(TR)),
     ?_assertEqual(2000, tgt_report:get_target_cross_range_unc(TR)),
     ?_assertEqual(200, tgt_report:get_target_height_unc(TR)),
     ?_assertEqual(5000, tgt_report:get_target_rad_vel_unc(TR)),
     ?_assertEqual(200, tgt_report:get_truth_tag_app(TR)),
     ?_assertEqual(10000, tgt_report:get_truth_tag_entity(TR)),
     ?_assertEqual(10, tgt_report:get_target_rcs(TR)),
     ?_assertEqual(<<>>, Rem)].

%% Checks of the payload size calculation.
payload_size_check1() ->
    % Create a sample report (all parameters). 
    {EM, R1} = sample_report(),

    % Compute the payload size expected after encoding.
    PaySize = tgt_report:payload_size(EM),

    % Encode it
    Bin = tgt_report:encode(R1, EM),

    [?_assertEqual(PaySize, byte_size(Bin))].

%% Payload size check for a target report with some parameters 
%% excluded.
payload_size_check2() ->
    FieldList = [mti_report_index, target_hr_lat, target_hr_lon, 
        geodetic_height, target_vel_los, target_wrap_velocity, target_snr,
        target_rcs],

    EM = exist_mask:new(FieldList), 
    PaySize = tgt_report:payload_size(EM),
    [?_assertEqual(PaySize, 18)].

decode_classification_checks() ->
    ClassList = target_classification_table(),
    % Add a value in the reserved range.
    ClassList2 = [{200, reserved}|ClassList],

    F = fun({K, V}) ->
            ?_assertEqual(V, tgt_report:decode_target_classification(K))
        end,
    lists:map(F, ClassList2).

encode_classification_checks() -> 
    ClassList = target_classification_table(),
    F = fun({K, V}) ->
            ?_assertEqual(<<K>>, tgt_report:encode_target_classification(V))
        end,
    lists:map(F, ClassList).

dict_conversion_checks() ->
    % Create a sample report (all parameters). 
    {EM, R1} = sample_report(),
   
    % Convert to a dictionary.
    D1 = tgt_report:to_dict(R1, EM),

    % Check a couple of parameters
    [?_assertEqual(5000, dict:fetch(geodetic_height, D1)),
     ?_assertEqual(-128, dict:fetch(target_snr, D1))].
    
%% Function to create a sample target report. Sets all fields to a value.

sample_report() ->
    Params = [{mti_report_index, 34}, {target_hr_lat, -33.3}, 
              {target_hr_lon, 357.57}, {target_delta_lat, -45},
              {target_delta_lon, 46}, {geodetic_height, 5000},
              {target_vel_los, 32000}, {target_wrap_velocity, 40000},
              {target_snr, -128}, {target_classification, vehicle_live_target},
              {target_class_prob, 90}, {target_slant_range_unc, 1000},
              {target_cross_range_unc, 2000}, {target_height_unc, 200},
              {target_rad_vel_unc, 5000}, {truth_tag_app, 200},
              {truth_tag_entity, 10000}, {target_rcs, 10}],

    % Return a suitable existence mask as well as the actual target report.
    FieldList = [K || {K, _V} <- Params],
    EM = exist_mask:new(FieldList), 
    {EM, tgt_report:new(Params)}.

target_classification_table() ->
    [{0, no_information_live_target}, 
     {1, tracked_vehicle_live_target},
     {2, wheeled_vehicle_live_target}, 
     {3, rotary_wing_aircraft_live_target},
     {4, fixed_wing_aircraft_live_target},
     {5, stationary_rotator_live_target},
     {6, maritime_live_target},
     {7, beacon_live_target},
     {8, amphibious_live_target},
     {9, person_live_target},
     {10, vehicle_live_target},
     {11, animal_live_target},
     {12, large_multiple_return_live_land_target},
     {13, large_multiple_return_live_maritime_target},
     {14, clutter_live_target},
     {15, phantom_live},
     {16, ground_rotator_live},
     {126, other_live_target},
     {127, unknown_live_target},
     {128, no_information_simulated_target},
     {129, tracked_vehicle_simulated_target},
     {130, wheeled_vehicle_simulated_target},
     {131, rotary_wing_aircraft_simulated_target},
     {132, fixed_wing_aircraft_simulated_target},
     {133, stationary_rotator_simulated_target},
     {134, maritime_simulated_target},
     {135, beacon_simulated_target},
     {136, amphibious_simulated_target},
     {137, person_simulated_target},
     {138, vehicle_simulated_target},
     {139, animal_simulated_target},
     {140, large_multiple_return_simulated_land_target},
     {141, large_multiple_return_simulated_maritime_target},
     {142, tagging_device},
     {144, clutter_simulated_target},
     {145, phantom_simulated},
     {146, ground_rotator_simulated},
     {254, other_simulated_target},
     {255, unknown_simulated_target}].

%% Utility function to compare whether floating point values are within a 
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
    
