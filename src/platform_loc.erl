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
-module(platform_loc).

-export([
    get_location_time/1,
    get_lat/1,
    get_lon/1,
    get_altitude/1,
    get_platform_track/1,
    get_platform_speed/1,
    get_platform_vertical_velocity/1,
    get_platform_position/1]).

-record(platform_loc_segment, {
    location_time,
    lat,
    lon,
    altitude,
    platform_track,
    platform_speed,
    platform_vertical_velocity}).

%% Accessor function for the location time field.
get_location_time(#platform_loc_segment{location_time = Time}) -> Time.

%% Accessor function for the flight plan field.
get_lat(#platform_loc_segment{lat = Lat}) -> Lat.

%% Accessor function for the platform type field.
get_lon(#platform_loc_segment{lon = Lon}) -> Lon.

%% Accessor function for the platform config field.
get_altitude(#platform_loc_segment{altitude = Altitude}) -> Altitude.

%% Accessor function for the year field.
get_platform_track(#platform_loc_segment{platform_track = Track}) -> Track.

%% Accessor function for the month field.
get_platform_speed(#platform_loc_segment{platform_speed = Speed}) -> Speed.

%% Accessor function for the day field.
get_platform_vertical_velocity(
    #platform_loc_segment{platform_vertical_velocity = VerticalVelocity})
        -> VerticalVelocity.

%% Convenience function to allow a caller to extract all of the position
%% fields as a single tuple.
get_platform_position(#platform_loc_segment{lat = Lat, lon = Lon,
    altitude = Altitude}) -> {Lat, Lon, Altitude}.

