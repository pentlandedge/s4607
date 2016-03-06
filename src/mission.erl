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
-module(mission).

-export([
    decode/1, 
    encode/1, 
    new/7,
    payload_size/0,
    display/1, 
    decode_platform_type/1,
    encode_platform_type/1,
    get_mission_plan/1,
    get_flight_plan/1,
    get_platform_type/1,
    get_platform_config/1,
    get_year/1,
    get_month/1,
    get_day/1,
    get_time/1]).

-record(mission_segment, {
    mission_plan, 
    flight_plan, 
    platform_type,
    platform_config,
    year,
    month,
    day}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mission segment decoding/encoding functions.

decode(<<M1:12/binary, M2:12/binary, M3, M4:10/binary, 
    Year:16/integer-unsigned-big, Month, Day>>) ->

    Mission = sutils:trim_trailing_spaces(binary_to_list(M1)),
    Flight = sutils:trim_trailing_spaces(binary_to_list(M2)),
    Type = decode_platform_type(M3),
    Config = sutils:trim_trailing_spaces(binary_to_list(M4)),

    {ok, #mission_segment{mission_plan = Mission, flight_plan = Flight, 
        platform_type = Type, platform_config = Config, year = Year,
        month = Month, day = Day}}.

%% Function takes a mission segment and returns an ecoded binary form.
encode(#mission_segment{mission_plan = MP, flight_plan = FP, 
    platform_type = Type, platform_config = Config,
    year = Year, month = Month, day = Day}) ->

    % Declare a local function to pad the strings and convert. 
    F = fun(Str, N) ->
            Padded = sutils:add_trailing_spaces(Str, N),
            list_to_binary(Padded)
        end,
    
    B1 = F(MP, 12),
    B2 = F(FP, 12),
    B3 = encode_platform_type(Type),
    B4 = F(Config, 10),
    B5 = stanag_types:integer_to_i16(Year),

    <<B1/binary,B2/binary,B3/binary,B4/binary,B5/binary,Month,Day>>.

%% Simple function to create a mission segment from the supplied parameters.
new(Mission, Flight, Type, Config, Year, Month, Day) ->
    #mission_segment{mission_plan = Mission, flight_plan = Flight, 
        platform_type = Type, platform_config = Config, year = Year,
        month = Month, day = Day}.

%% Function to return the size of the mission segment payload.
payload_size() -> 39.

decode_platform_type(0) -> unidentified;
decode_platform_type(1) -> acs;
decode_platform_type(2) -> arl_m;
decode_platform_type(3) -> sentinel;
decode_platform_type(4) -> rotary_wing_radar;
decode_platform_type(5) -> global_hawk_navy;
decode_platform_type(6) -> horizon;
decode_platform_type(7) -> e_8;
decode_platform_type(8) -> p_3c;
decode_platform_type(9) -> predator;
decode_platform_type(10) -> radarsat2;
decode_platform_type(11) -> u_2;
decode_platform_type(12) -> e_10;
decode_platform_type(13) -> ugs_single;
decode_platform_type(14) -> ugs_cluster;
decode_platform_type(15) -> ground_based;
decode_platform_type(16) -> uav_army;
decode_platform_type(17) -> uav_marines;
decode_platform_type(18) -> uav_navy;
decode_platform_type(19) -> uav_air_force;
decode_platform_type(20) -> global_hawk_air_force;
decode_platform_type(21) -> global_hawk_australia;
decode_platform_type(22) -> global_hawk_germany;
decode_platform_type(23) -> paul_revere;
decode_platform_type(24) -> mariner_uav;
decode_platform_type(25) -> bac_111;
decode_platform_type(26) -> coyote;
decode_platform_type(27) -> king_air;
decode_platform_type(28) -> limit;
decode_platform_type(29) -> nrl_np_3b;
decode_platform_type(30) -> sostar_x;
decode_platform_type(31) -> watchkeeper;
decode_platform_type(32) -> alliance_ground_surveillance;
decode_platform_type(33) -> stryker;
decode_platform_type(34) -> ags_hale_uav;
decode_platform_type(35) -> sidm;
decode_platform_type(36) -> reaper;
decode_platform_type(37) -> warrior_a;
decode_platform_type(38) -> warrior;
decode_platform_type(39) -> twin_otter;
decode_platform_type(255) -> other;
decode_platform_type(_) -> future_use.

%% Function to convert a platform type to a binary representation.
encode_platform_type(X) when is_atom(X) ->
    Val = ept(X),
    <<Val>>.

%% Helper function with the atom -> integer mappings for the platform type.
ept(unidentified) -> 0;
ept(acs) -> 1;
ept(arl_m) -> 2;
ept(sentinel) -> 3;
ept(rotary_wing_radar) -> 4;
ept(global_hawk_navy) -> 5;
ept(horizon) -> 6;
ept(e_8) -> 7;
ept(p_3c) -> 8;
ept(predator) -> 9;
ept(radarsat2) -> 10;
ept(u_2) -> 11;
ept(e_10) -> 12;
ept(ugs_single) -> 13;
ept(ugs_cluster) -> 14;
ept(ground_based) -> 15;
ept(uav_army) -> 16;
ept(uav_marines) -> 17;
ept(uav_navy) -> 18;
ept(uav_air_force) -> 19;
ept(global_hawk_air_force) -> 20;
ept(global_hawk_australia) -> 21;
ept(global_hawk_germany) -> 22;
ept(paul_revere) -> 23;
ept(mariner_uav) -> 24;
ept(bac_111) -> 25;
ept(coyote) -> 26;
ept(king_air) -> 27;
ept(limit) -> 28;
ept(nrl_np_3b) -> 29;
ept(sostar_x) -> 30;
ept(watchkeeper) -> 31;
ept(alliance_ground_surveillance) -> 32;
ept(stryker) -> 33;
ept(ags_hale_uav) -> 34;
ept(sidm) -> 35;
ept(reaper) -> 36;
ept(warrior_a) -> 37;
ept(warrior) -> 38;
ept(twin_otter) -> 39;
ept(other) -> 255;
ept(future_use) -> 254.

display(MSeg) ->
    io:format("****************************************~n"),
    io:format("** @mission~n"),
    io:format("Mission Plan: ~p~n", [MSeg#mission_segment.mission_plan]),
    io:format("Flight Plan: ~p~n", [MSeg#mission_segment.flight_plan]),
    io:format("Plat. Type: ~p~n", [MSeg#mission_segment.platform_type]),
    io:format("Plat. Config: ~p~n", [MSeg#mission_segment.platform_config]),
    io:format("Year: ~p~n", [MSeg#mission_segment.year]),
    io:format("Month: ~p~n", [MSeg#mission_segment.month]),
    io:format("Day: ~p~n", [MSeg#mission_segment.day]).

%% Accessor function for the mission plan field.
get_mission_plan(#mission_segment{mission_plan = M}) -> M.

%% Accessor function for the flight plan field.
get_flight_plan(#mission_segment{flight_plan = F}) -> F.

%% Accessor function for the platform type field.
get_platform_type(#mission_segment{platform_type = T}) -> T.

%% Accessor function for the platform config field.
get_platform_config(#mission_segment{platform_config = C}) -> C.

%% Accessor function for the year field.
get_year(#mission_segment{year = Y}) -> Y.

%% Accessor function for the month field.
get_month(#mission_segment{month = M}) -> M.

%% Accessor function for the day field.
get_day(#mission_segment{day = D}) -> D.

%% Convenience function to allow a caller to extract all of the time fields
%% as a single tuple.
get_time(#mission_segment{year = Y, month = M, day = D}) -> {Y, M, D}.

