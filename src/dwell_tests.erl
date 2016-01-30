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

%% Define a test generator for the dwell segment. 
dwell_test_() ->
    [creation_checks1()].

creation_checks1() ->
    DS = minimal_dwell(),
    [?_assertEqual(100, dwell:get_revisit_index(DS))].

%% Function to create a sample dwell segment with only the mandatory fields
%% set.
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
         {last_dwell_of_revisit, 1}, {target_report_count, 0}, 
         {dwell_time, 1000000}, {sensor_lat, -45.0}, {sensor_lon, 350},
         {sensor_alt, -10000}, {dwell_center_lat, -45.2}, 
         {dwell_center_lon, 350.2}, {dwell_range_half_extent, 255.0}, 
         {dwell_angle_half_extent, 350}],

    % Use the parameters to construct a new dwell segment.
    dwell:new(P).

