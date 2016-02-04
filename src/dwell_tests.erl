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

-module(dwell_tests).

-include_lib("eunit/include/eunit.hrl").

-export([minimal_dwell/0, one_target_dwell/0, three_targets_dwell/0]).

%% Define a test generator for the dwell segment. 
dwell_test_() ->
    [creation_checks1(), encode_decode_check1(), encode_decode_check2(), 
     encode_decode_check3()].

creation_checks1() ->
    DS = minimal_dwell(),
    EM = dwell:get_existence_mask(DS),
    [?_assertEqual(1, exist_mask:get_revisit_index(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_index(EM)),
     ?_assertEqual(1, exist_mask:get_last_dwell_of_revisit(EM)),
     ?_assertEqual(1, exist_mask:get_target_report_count(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_time(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_lat(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_lon(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_alt(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_range_half_extent(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_angle_half_extent(EM)),
     
     ?_assertEqual(0, exist_mask:get_spu_along_track(EM)),
     ?_assertEqual(0, exist_mask:get_spu_alt(EM)),
     ?_assertEqual(0, exist_mask:get_mti_report_index(EM)),
     ?_assertEqual(0, exist_mask:get_target_hr_lat(EM)),
     ?_assertEqual(0, exist_mask:get_target_hr_lon(EM)),
     ?_assertEqual(0, exist_mask:get_mdv(EM)),

     ?_assertEqual(100, dwell:get_revisit_index(DS)),
     ?_assertEqual(20000, dwell:get_dwell_index(DS)),
     ?_assertEqual(no_additional_dwells, dwell:get_last_dwell_of_revisit(DS)),
     ?_assertEqual(0, dwell:get_target_report_count(DS)),
     ?_assertEqual(1000000, dwell:get_dwell_time(DS)),
     ?_assert(almost_equal(-45.0, dwell:get_sensor_lat(DS), 0.0000001)),
     ?_assert(almost_equal(350.0, dwell:get_sensor_lon(DS), 0.0000001)),
     ?_assertEqual(-10000, dwell:get_sensor_alt(DS)),
     ?_assert(almost_equal(255.0, dwell:get_dwell_range_half_extent(DS), 0.0000001)),
     ?_assertEqual(350, dwell:get_dwell_angle_half_extent(DS))].

encode_decode_check1() ->
    MD = minimal_dwell(),
    EM = dwell:get_existence_mask(MD),
    Bin = dwell:encode(MD),
    {ok, DS} = dwell:decode(Bin),
    [?_assertEqual(1, exist_mask:get_revisit_index(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_index(EM)),
     ?_assertEqual(1, exist_mask:get_last_dwell_of_revisit(EM)),
     ?_assertEqual(1, exist_mask:get_target_report_count(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_time(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_lat(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_lon(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_alt(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_range_half_extent(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_angle_half_extent(EM)),
     
     ?_assertEqual(0, exist_mask:get_spu_along_track(EM)),
     ?_assertEqual(0, exist_mask:get_spu_alt(EM)),
     ?_assertEqual(0, exist_mask:get_mti_report_index(EM)),
     ?_assertEqual(0, exist_mask:get_target_hr_lat(EM)),
     ?_assertEqual(0, exist_mask:get_target_hr_lon(EM)),
     ?_assertEqual(0, exist_mask:get_mdv(EM)),

     ?_assertEqual(100, dwell:get_revisit_index(DS)),
     ?_assertEqual(20000, dwell:get_dwell_index(DS)),
     ?_assertEqual(no_additional_dwells, dwell:get_last_dwell_of_revisit(DS)),
     ?_assertEqual(0, dwell:get_target_report_count(DS)),
     ?_assertEqual(1000000, dwell:get_dwell_time(DS)),
     ?_assert(almost_equal(-45.0, dwell:get_sensor_lat(DS), 0.0000001)),
     ?_assert(almost_equal(350.0, dwell:get_sensor_lon(DS), 0.0000001)),
     ?_assertEqual(-10000, dwell:get_sensor_alt(DS)),
     ?_assert(almost_equal(255.0, dwell:get_dwell_range_half_extent(DS), 0.0000001)),
     ?_assert(almost_equal(350.0, dwell:get_dwell_angle_half_extent(DS), 0.1))].

%% Second encode/decode check, this time with a target report included in 
%% the dwell segment.
encode_decode_check2() ->
    TD = one_target_dwell(),
    EM = dwell:get_existence_mask(TD),
    Bin = dwell:encode(TD),
    {ok, DS} = dwell:decode(Bin),
    [?_assertEqual(1, exist_mask:get_revisit_index(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_index(EM)),
     ?_assertEqual(1, exist_mask:get_last_dwell_of_revisit(EM)),
     ?_assertEqual(1, exist_mask:get_target_report_count(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_time(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_lat(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_lon(EM)),
     ?_assertEqual(1, exist_mask:get_sensor_alt(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_range_half_extent(EM)),
     ?_assertEqual(1, exist_mask:get_dwell_angle_half_extent(EM)),
     
     ?_assertEqual(0, exist_mask:get_spu_along_track(EM)),
     ?_assertEqual(0, exist_mask:get_spu_alt(EM)),
     ?_assertEqual(1, exist_mask:get_mti_report_index(EM)),
     ?_assertEqual(1, exist_mask:get_target_hr_lat(EM)),
     ?_assertEqual(1, exist_mask:get_target_hr_lon(EM)),
     ?_assertEqual(0, exist_mask:get_mdv(EM)),

     ?_assertEqual(100, dwell:get_revisit_index(DS)),
     ?_assertEqual(20000, dwell:get_dwell_index(DS)),
     ?_assertEqual(no_additional_dwells, dwell:get_last_dwell_of_revisit(DS)),
     ?_assertEqual(1, dwell:get_target_report_count(DS)),
     ?_assertEqual(1000000, dwell:get_dwell_time(DS)),
     ?_assert(almost_equal(-45.0, dwell:get_sensor_lat(DS), 0.0000001)),
     ?_assert(almost_equal(350.0, dwell:get_sensor_lon(DS), 0.0000001)),
     ?_assertEqual(-10000, dwell:get_sensor_alt(DS)),
     ?_assert(almost_equal(255.0, dwell:get_dwell_range_half_extent(DS), 0.0000001)),
     ?_assert(almost_equal(350.0, dwell:get_dwell_angle_half_extent(DS), 0.1))].

%% Third encode/decode check, this time with three target reports included in 
%% the dwell segment.
encode_decode_check3() ->
    TD = three_targets_dwell(),
    Bin = dwell:encode(TD),
    {ok, DS} = dwell:decode(Bin),
    [T1,T2,T3] = dwell:get_targets(DS),
    [?_assertEqual(3, dwell:get_target_report_count(DS)),
     ?_assert(almost_equal(-33.3, tgt_report:get_target_hr_lat(T1), 0.0000001)),
     ?_assert(almost_equal(-33.4, tgt_report:get_target_hr_lat(T2), 0.0000001)),
     ?_assert(almost_equal(-33.5, tgt_report:get_target_hr_lat(T3), 0.0000001)),
     ?_assertEqual(3000, tgt_report:get_geodetic_height(T1)),
     ?_assertEqual(4000, tgt_report:get_geodetic_height(T2)),
     ?_assertEqual(5000, tgt_report:get_geodetic_height(T3))].

%% Function to create a sample dwell segment with only the mandatory fields
% set.
minimal_dwell() ->
    % Create a list of fields for the existence mask.
    F = [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit,
         target_report_count, dwell_time, sensor_lat, sensor_lon, 
         sensor_alt, dwell_center_lat, dwell_center_lon, 
         dwell_range_half_extent, dwell_angle_half_extent],

    % Create an existence mask.
    EM = exist_mask:new(F), 
   
    % Set the fields of the dwell segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000}, 
         {last_dwell_of_revisit, no_additional_dwells}, {target_report_count, 0}, 
         {dwell_time, 1000000}, {sensor_lat, -45.0}, {sensor_lon, 350},
         {sensor_alt, -10000}, {dwell_center_lat, -45.2}, 
         {dwell_center_lon, 350.2}, {dwell_range_half_extent, 255.0}, 
         {dwell_angle_half_extent, 350}],

    % Use the parameters to construct a new dwell segment.
    dwell:new(P).

%% Function to create a sample dwell with a single target report in it.
one_target_dwell() ->
    % Create a list of fields for the existence mask (excluding the target
    % report).
    F = [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit,
         target_report_count, dwell_time, sensor_lat, sensor_lon, 
         sensor_alt, dwell_center_lat, dwell_center_lon, 
         dwell_range_half_extent, dwell_angle_half_extent, targets],
  
    % The fields of the target report.
    Params = [{mti_report_index, 34}, {target_hr_lat, -33.3}, 
              {target_hr_lon, 357.57}, {target_delta_lat, -45},
              {target_delta_lon, 46}, {geodetic_height, 5000},
              {target_vel_los, 32000}, {target_wrap_velocity, 40000},
              {target_snr, -128}, {target_classification, vehicle_live_target},
              {target_class_prob, 90}, {target_slant_range_unc, 1000},
              {target_cross_range_unc, 2000}, {target_height_unc, 200},
              {target_rad_vel_unc, 5000}, {truth_tag_app, 200},
              {truth_tag_entity, 10000}, {target_rcs, 10}],

    % Extract the list of fields in the target report.
    FieldList = [K || {K, _V} <- Params],

    % Splice together all the fields that make up the existence mask.
    Efields = F ++ FieldList,

    % Create the existence mask.
    EM = exist_mask:new(Efields), 
   
    % Create the target report.
    TgtRep = tgt_report:new(Params),

    % Set the fields of the dwell segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000}, 
         {last_dwell_of_revisit, no_additional_dwells}, {target_report_count, 1}, 
         {dwell_time, 1000000}, {sensor_lat, -45.0}, {sensor_lon, 350},
         {sensor_alt, -10000}, {dwell_center_lat, -45.2}, 
         {dwell_center_lon, 350.2}, {dwell_range_half_extent, 255.0}, 
         {dwell_angle_half_extent, 350}, {targets, [TgtRep]}],

    % Create and return the dwell segment.
    dwell:new(P).

%% Function to create a sample dwell with three target reports in it.
three_targets_dwell() ->
    % Create a list of fields for the existence mask (excluding the target
    % report).
    F = [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit,
         target_report_count, dwell_time, sensor_lat, sensor_lon, 
         sensor_alt, dwell_center_lat, dwell_center_lon, 
         dwell_range_half_extent, dwell_angle_half_extent, targets],
  
    % The fields of the target report 1.
    Params1 = [{mti_report_index, 34}, {target_hr_lat, -33.3}, 
               {target_hr_lon, 357.57}, {target_delta_lat, -45},
               {target_delta_lon, 46}, {geodetic_height, 3000},
               {target_vel_los, 32000}, {target_wrap_velocity, 40000},
               {target_snr, -128}, {target_classification, vehicle_live_target},
               {target_class_prob, 90}, {target_slant_range_unc, 1000},
               {target_cross_range_unc, 2000}, {target_height_unc, 200},
               {target_rad_vel_unc, 5000}, {truth_tag_app, 200},
               {truth_tag_entity, 10000}, {target_rcs, 10}],

    % The fields of the target report 2.
    Params2 = [{mti_report_index, 34}, {target_hr_lat, -33.4}, 
               {target_hr_lon, 357.67}, {target_delta_lat, -45},
               {target_delta_lon, 46}, {geodetic_height, 4000},
               {target_vel_los, 32000}, {target_wrap_velocity, 40000},
               {target_snr, -128}, {target_classification, vehicle_live_target},
               {target_class_prob, 90}, {target_slant_range_unc, 1000},
               {target_cross_range_unc, 2000}, {target_height_unc, 200},
               {target_rad_vel_unc, 5000}, {truth_tag_app, 200},
               {truth_tag_entity, 10000}, {target_rcs, 15}],

    % The fields of the target report 3.
    Params3 = [{mti_report_index, 34}, {target_hr_lat, -33.5}, 
               {target_hr_lon, 357.77}, {target_delta_lat, -45},
               {target_delta_lon, 46}, {geodetic_height, 5000},
               {target_vel_los, 32000}, {target_wrap_velocity, 40000},
               {target_snr, -128}, {target_classification, vehicle_live_target},
               {target_class_prob, 90}, {target_slant_range_unc, 1000},
               {target_cross_range_unc, 2000}, {target_height_unc, 200},
               {target_rad_vel_unc, 5000}, {truth_tag_app, 200},
               {truth_tag_entity, 10000}, {target_rcs, 20}],

    % Extract the list of fields in the target report.
    FieldList = [K || {K, _V} <- Params1],

    % Splice together all the fields that make up the existence mask.
    Efields = F ++ FieldList,

    % Create the existence mask.
    EM = exist_mask:new(Efields), 
   
    % Create the target reports.
    TgtRep1 = tgt_report:new(Params1),
    TgtRep2 = tgt_report:new(Params2),
    TgtRep3 = tgt_report:new(Params3),

    % Set the fields of the dwell segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000}, 
         {last_dwell_of_revisit, no_additional_dwells}, {target_report_count, 3}, 
         {dwell_time, 1000000}, {sensor_lat, -45.0}, {sensor_lon, 350},
         {sensor_alt, -10000}, {dwell_center_lat, -45.2}, 
         {dwell_center_lon, 350.2}, {dwell_range_half_extent, 255.0}, 
         {dwell_angle_half_extent, 350}, 
         {targets, [TgtRep1, TgtRep2, TgtRep3]}],

    % Create and return the dwell segment.
    dwell:new(P).

%% Utility function to compare whether floating point values are within a 
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
