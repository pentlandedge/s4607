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

-module(platform_loc_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the decoding of the platform location segment.
platform_loc_test_() ->
    [location_time_checks(), lat_checks(), lon_checks(), alt_checks(),
    platform_track_checks(), platform_speed_checks(),
    platform_vertical_velocity_checks()].

location_time_checks() ->
    {ok, PLS1} = platform_loc:decode(sample_platform_location_seg1()),
    {ok, PLS2} = platform_loc:decode(sample_platform_location_seg2()),
    [?_assertEqual(123456789, platform_loc:get_location_time(PLS1)),
     ?_assertEqual(987654321, platform_loc:get_location_time(PLS2))].

lat_checks() ->
    {ok, PLS1} = platform_loc:decode(sample_platform_location_seg1()),
    {ok, PLS2} = platform_loc:decode(sample_platform_location_seg2()),
    [?_assert(almost_equal(-20.2, platform_loc:get_lat(PLS1), 0.000001)),
     ?_assert(almost_equal(20.2, platform_loc:get_lat(PLS2), 0.000001))].

lon_checks() ->
    {ok, PLS1} = platform_loc:decode(sample_platform_location_seg1()),
    {ok, PLS2} = platform_loc:decode(sample_platform_location_seg2()),
    [?_assert(almost_equal(256.7, platform_loc:get_lon(PLS1), 0.000001)),
     ?_assert(almost_equal(39.0, platform_loc:get_lon(PLS2), 0.000001))].

alt_checks() ->
    {ok, PLS1} = platform_loc:decode(sample_platform_location_seg1()),
    {ok, PLS2} = platform_loc:decode(sample_platform_location_seg2()),
    [?_assertEqual(120000, platform_loc:get_alt(PLS1)),
     ?_assertEqual(-20000, platform_loc:get_alt(PLS2))].

platform_track_checks() ->
    {ok, PLS1} = platform_loc:decode(sample_platform_location_seg1()),
    {ok, PLS2} = platform_loc:decode(sample_platform_location_seg2()),
    [?_assert(almost_equal(90.5, platform_loc:get_platform_track(PLS1), 0.01)),
     ?_assert(almost_equal(294.2, platform_loc:get_platform_track(PLS2), 0.01))].

platform_speed_checks() ->
    {ok, PLS1} = platform_loc:decode(sample_platform_location_seg1()),
    {ok, PLS2} = platform_loc:decode(sample_platform_location_seg2()),
    [?_assertEqual(52100, platform_loc:get_platform_speed(PLS1)),
     ?_assertEqual(100, platform_loc:get_platform_speed(PLS2))].

platform_vertical_velocity_checks() ->
    {ok, PLS1} = platform_loc:decode(sample_platform_location_seg1()),
    {ok, PLS2} = platform_loc:decode(sample_platform_location_seg2()),
    [?_assertEqual(127, platform_loc:get_platform_vertical_velocity(PLS1)),
     ?_assertEqual(-46, platform_loc:get_platform_vertical_velocity(PLS2))].

sample_platform_location_seg1() ->
    %%[123456789, -20.2, 256.7, 120000, 90.5, 52100, 127].
    <<7,91,205,21, 227,69,103,138, 182,138,207,19, 0,1,212,192, 64,91,
        0,0,203,132, 127>>.

sample_platform_location_seg2() ->
    %%[987654321, 20.2, 39.0, -20000, 294.2, 100, -46].
    <<58,222,104,177, 28,186,152,118, 27,187,187,188, 255,255,177,224,
        209,53, 0,0,0,100, 210>>.

%% Utility function to compare whether floating point values are within a
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
