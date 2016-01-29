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
    [tgt_checks1()].

tgt_checks1() ->
    Params = [{mti_report_index, 34}, {target_hr_lat, -33.3}, 
              {target_hr_lon, -3.57}, {target_delta_lat, -45},
              {target_delta_lon, 46}, {geodetic_height, 5000},
              {target_vel_los, 32000}, {target_wrap_velocity, 40000},
              {target_snr, -128}, {target_classification, vehicle_live_target},
              {target_class_prob, 90}, {target_slant_range_unc, 1000},
              {target_cross_range_unc, 2000}, {target_height_unc, 200},
              {target_rad_vel_unc, 5000}, {truth_tag_app, 200},
              {truth_tag_entity, 10000}, {target_rcs, 10}],

    R1 = tgt_report:new(Params),

    [?_assertEqual(34, tgt_report:get_mti_report_index(R1)),
     ?_assert(almost_equal(-33.3, tgt_report:get_target_hr_lat(R1), 0.0000001)),
     ?_assert(almost_equal(-3.57, tgt_report:get_target_hr_lon(R1), 0.0000001)),
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


%% Utility function to compare whether floating point values are within a 
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
    