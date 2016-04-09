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
    decode/1,
    encode/1,
    new/7,
    payload_size/1,
    display/1,
    get_location_time/1,
    get_lat/1,
    get_lon/1,
    get_alt/1,
    get_platform_track/1,
    get_platform_speed/1,
    get_platform_vertical_velocity/1,
    get_platform_position/1]).

-record(platform_loc_segment, {
    location_time,
    lat,
    lon,
    alt,
    platform_track,
    platform_speed,
    platform_vertical_velocity}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Platform Location segment decoding/encoding functions.

decode(<<L1:32/integer-unsigned-big, L2:4/binary, L3:4/binary, L4:4/binary,
    L5:2/binary, L6:32/integer-unsigned-big, L7:8/integer-signed-big>>) ->

    {ok, #platform_loc_segment{
        location_time = L1,
        lat = stanag_types:sa32_to_float(L2),
        lon = stanag_types:ba32_to_float(L3),
        alt = stanag_types:s32_to_integer(L4),
        platform_track = stanag_types:ba16_to_float(L5),
        platform_speed = L6,
        platform_vertical_velocity = L7}}.

%% Function takes a platform location segment and returns an encoded binary form.
encode(#platform_loc_segment{location_time = LocationTime, lat = Lat,
    lon = Lon, alt = Alt, platform_track = Track, platform_speed = Speed,
    platform_vertical_velocity = VerticalVelocity}) ->

    B1 = stanag_types:integer_to_i32(LocationTime),
    B2 = stanag_types:float_to_sa32(Lat),
    B3 = stanag_types:float_to_ba32(Lon),
    B4 = stanag_types:integer_to_s32(Alt),
    B5 = stanag_types:float_to_ba16(Track),
    B6 = stanag_types:integer_to_i32(Speed),
    B7 = stanag_types:integer_to_s8(VerticalVelocity),

    <<B1,B2,B3,B4,B5,B6,B7>>.


%% Simple function to create a mission segment from the supplied parameters.
new(LocationTime, Lat, Lon, Alt, Track, Speed, VerticalVelocity) ->
    #platform_loc_segment{location_time = LocationTime, lat = Lat,
        lon = Lon, alt = Alt, platform_track = Track,
        platform_speed = Speed, platform_vertical_velocity = VerticalVelocity}.

%% Function to return the size of the platform location segment payload.
payload_size(_) -> 23.

%% Function to display the platform location segment in readable format
display(PLSeg) ->
    io:format("****************************************~n"),
    io:format("** @platform_location~n"),
    io:format("Location Time: ~p~n", [PLSeg#platform_loc_segment.location_time]),
    io:format("Plat. Pos. Latitude: ~p~n", [PLSeg#platform_loc_segment.lat]),
    io:format("Plat. Pos. Longitude: ~p~n", [PLSeg#platform_loc_segment.lon]),
    io:format("Plat. Pos. Altitude: ~p~n", [PLSeg#platform_loc_segment.lat]),
    io:format("Plat. Track: ~p~n", [PLSeg#platform_loc_segment.platform_track]),
    io:format("Plat. Speed: ~p~n", [PLSeg#platform_loc_segment.platform_speed]),
    io:format("Plat. Vertical Velocity: ~p~n", [PLSeg#platform_loc_segment.platform_vertical_velocity]).

%% Accessor function for the location time field.
get_location_time(#platform_loc_segment{location_time = Time}) -> Time.

%% Accessor function for the latitude field.
get_lat(#platform_loc_segment{lat = Lat}) -> Lat.

%% Accessor function for the longitude field.
get_lon(#platform_loc_segment{lon = Lon}) -> Lon.

%% Accessor function for the altitude field.
get_alt(#platform_loc_segment{alt = Alt}) -> Alt.

%% Accessor function for the platform track field.
get_platform_track(#platform_loc_segment{platform_track = Track}) -> Track.

%% Accessor function for the platform speed field.
get_platform_speed(#platform_loc_segment{platform_speed = Speed}) -> Speed.

%% Accessor function for the platform vertical velocity field.
get_platform_vertical_velocity(
    #platform_loc_segment{platform_vertical_velocity = VerticalVelocity})
        -> VerticalVelocity.

%% Convenience function to allow a caller to extract all of the position
%% fields as a single tuple.
get_platform_position(#platform_loc_segment{lat = Lat, lon = Lon,
    alt = Alt}) -> {Lat, Lon, Alt}.

