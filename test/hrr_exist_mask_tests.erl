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

-module(hrr_exist_mask_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the existence mask.
hrr_exist_mask_test_() ->
    [hrr_exist_mask_check1(), hrr_exist_mask_check2()].

hrr_exist_mask_check1() ->
    EM = hrr_exist_mask:new([num_of_target_scatterers]),
    % Call the display function to avoid it dragging down code coverage.
    hrr_exist_mask:display(EM),
    [?_assertEqual(1, hrr_exist_mask:get_num_of_target_scatterers(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_num_of_range_samples(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_center_frequency(EM))].

hrr_exist_mask_check2() ->
    EM = hrr_exist_mask:new([num_of_target_scatterers, mean_clutter_power,
        mti_report_index, maximum_rcs, range_extent_pixels, range_index]),
    [?_assertEqual(0, hrr_exist_mask:get_num_of_range_samples(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_num_of_target_scatterers(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_center_frequency(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_mean_clutter_power(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_mti_report_index(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_maximum_rcs(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_extent_pixels(EM)),
     ?_assertEqual(1, hrr_exist_mask:get_range_index(EM)),
     ?_assertEqual(0, hrr_exist_mask:get_range_to_nearest_edge(EM))].


