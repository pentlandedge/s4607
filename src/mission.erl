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
%% @doc Functions for manipulating mission segments defined in the 
%%      Stanag 4607 standard.

-module(mission).

-export([
    decode/1, 
    encode/1, 
    new/7,
    payload_size/1,
    display/1, 
    to_csv_iolist/1,
    decode_platform_type/1,
    encode_platform_type/1,
    set_date/2,
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
%% Type specifications.

-opaque mission_segment() :: #mission_segment{}.

-type mission_segment_bin() :: <<_:312>>.

-export_type([mission_segment/0, mission_segment_bin/0]).

-type platform_type() :: unidentified | acs | arl_m | sentinel | 
    rotary_wing_radar | global_hawk_navy | horizon | e_8 | p_3c | predator | 
    radarsat2 | u_2 | e_10 | ugs_single | ugs_cluster | ground_based | 
    uav_army | uav_marines | uav_navy | uav_air_force | 
    global_hawk_air_force | global_hawk_australia | global_hawk_germany | 
    paul_revere | mariner_uav | bac_111 | coyote | king_air | limit | 
    nrl_np_3b | sostar_x | watchkeeper | alliance_ground_surveillance | 
    stryker | ags_hale_uav | sidm | reaper | warrior_a | warrior | 
    twin_otter | lemv | p8a_poseidon | a160 | mq_1c_gray_eagle | rq_7c_shadow | 
    pgss | pdts | lras_3 | raid_tower | heron | scan_eagle | fire_scout | 
    f35_joint_strike_fighter | s_61_sea_king | lynx_wildcat | merlin | other.

-export_type([platform_type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mission segment decoding/encoding functions.

%% @doc Decode a binary mission segment.
-spec decode(Bin::mission_segment_bin())-> {ok, mission_segment()}.
decode(<<M1:12/binary, M2:12/binary, M3, M4:10/binary, 
    Year:16/integer-unsigned-big, Month, Day>>) ->

    Mission = sutils:trim_trailing_spaces(binary_to_list(M1)),
    Flight = sutils:trim_trailing_spaces(binary_to_list(M2)),
    Type = decode_platform_type(M3),
    Config = sutils:trim_trailing_spaces(binary_to_list(M4)),

    {ok, #mission_segment{mission_plan = Mission, flight_plan = Flight, 
        platform_type = Type, platform_config = Config, year = Year,
        month = Month, day = Day}}.

%% @doc Convert a mission segment to an encoded binary form.
-spec encode(MS::mission_segment()) -> mission_segment_bin().
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

%% @doc Create a mission segment structure from the supplied parameters.
-spec new(Mission, Flight, Type, Config, Year, Month, Day) -> MS 
    when Mission::string(), 
        Flight::string(), 
        Type::platform_type(),
        Config::string(), 
        Year::stanag_types:i16_int(),
        Month::stanag_types:i8_int(),
        Day::stanag_types:i8_int(),
        MS::mission_segment().
new(Mission, Flight, Type, Config, Year, Month, Day) ->
    #mission_segment{mission_plan = Mission, flight_plan = Flight, 
        platform_type = Type, platform_config = Config, year = Year,
        month = Month, day = Day}.

%% @doc Return the size of an encoded mission segment payload.
-spec payload_size(any()) -> non_neg_integer().
payload_size(_) -> 39.

%% @doc Decode the platform type parameter.
-spec decode_platform_type(PT::byte()) -> future_use | platform_type(). 
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
decode_platform_type(36) -> mq_9_reaper;
decode_platform_type(37) -> warrior_a;
decode_platform_type(38) -> warrior;
decode_platform_type(39) -> twin_otter;
decode_platform_type(40) -> lemv;
decode_platform_type(41) -> p8a_poseidon;
decode_platform_type(42) -> a160;
decode_platform_type(43) -> mq_1c_gray_eagle;
decode_platform_type(44) -> rq_7c_shadow;
decode_platform_type(45) -> pgss;
decode_platform_type(46) -> pdts;
decode_platform_type(47) -> lras_3;
decode_platform_type(48) -> raid_tower;
decode_platform_type(49) -> heron;
decode_platform_type(50) -> scan_eagle;
decode_platform_type(51) -> fire_scout;
decode_platform_type(52) -> f35_joint_strike_fighter;
decode_platform_type(53) -> s_61_sea_king;
decode_platform_type(54) -> lynx_wildcat;
decode_platform_type(55) -> merlin;
decode_platform_type(56) -> sdt;
decode_platform_type(255) -> other;
decode_platform_type(_) -> future_use.

%% @doc Convert a platform type parameter to a binary representation.
-spec encode_platform_type(Type::platform_type()) -> <<_:8>>.
encode_platform_type(Type) when is_atom(Type) ->
    Val = ept(Type),
    <<Val>>.

%% @doc Helper function with the atom -> integer mappings for the platform 
%% type.
-spec ept(Type::platform_type()) -> byte().
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
ept(mq_9_reaper) -> 36;
ept(warrior_a) -> 37;
ept(warrior) -> 38;
ept(twin_otter) -> 39;
ept(lemv) -> 40;
ept(p8a_poseidon) -> 41;
ept(a160) -> 42;
ept(mq_1c_gray_eagle) -> 43;
ept(rq_7c_shadow) -> 44;
ept(pgss) -> 45;
ept(pdts) -> 46;
ept(lras_3) -> 47;
ept(raid_tower) -> 48;
ept(heron) -> 49;
ept(scan_eagle) -> 50;
ept(fire_scout) -> 51;
ept(f35_joint_strike_fighter) -> 52;
ept(s_61_sea_king) -> 53;
ept(lynx_wildcat) -> 54;
ept(merlin) -> 55;
ept(sdt) -> 56;
ept(other) -> 255.

%% @doc Patch the date field in a mission segment.
set_date(MSeg, {Y, M, D}) ->
    MSeg#mission_segment{year = Y, month = M, day = D}.

%% @doc Display the contents of a decoded mission segment structure.
-spec display(MSeg::mission_segment()) -> ok.
display(MSeg) ->
    io:format("****************************************~n"),
    io:format("** @mission~n"),
    io:format("Mission Plan: ~p~n", [get_mission_plan(MSeg)]),
    io:format("Flight Plan: ~p~n", [get_flight_plan(MSeg)]),
    io:format("Plat. Type: ~p~n", [get_platform_type(MSeg)]),
    io:format("Plat. Config: ~p~n", [get_platform_config(MSeg)]),
    io:format("Year: ~p~n", [get_year(MSeg)]),
    io:format("Month: ~p~n", [get_month(MSeg)]),
    io:format("Day: ~p~n", [get_day(MSeg)]).

%% @doc Convert the segment data into a CSV string.
-spec to_csv_iolist(MS::mission_segment()) -> iolist().
to_csv_iolist(MSeg) ->
    Args = [get_mission_plan(MSeg),
            get_flight_plan(MSeg),
            get_platform_type(MSeg),
            get_platform_config(MSeg),
            get_year(MSeg),
            get_month(MSeg),
            get_day(MSeg)],
    io_lib:format("MS,~s,~s,~p,~s,~p,~p,~p~n", Args). 

%% @doc Accessor function for the mission plan field.
-spec get_mission_plan(MS::mission_segment()) -> string().
get_mission_plan(#mission_segment{mission_plan = M}) -> M.

%% @doc Accessor function for the flight plan field.
-spec get_flight_plan(MS::mission_segment()) -> string().
get_flight_plan(#mission_segment{flight_plan = F}) -> F.

%% @doc Accessor function for the platform type field.
-spec get_platform_type(MS::mission_segment()) -> platform_type().
get_platform_type(#mission_segment{platform_type = T}) -> T.

%% @doc Accessor function for the platform config field.
-spec get_platform_config(MS::mission_segment()) -> string().
get_platform_config(#mission_segment{platform_config = C}) -> C.

%% @doc Accessor function for the year field.
-spec get_year(MS::mission_segment()) -> stanag_types:i16_int().
get_year(#mission_segment{year = Y}) -> Y.

%% @doc Accessor function for the month field.
-spec get_month(MS::mission_segment()) -> stanag_types:i8_int().
get_month(#mission_segment{month = M}) -> M.

%% @doc Accessor function for the day field.
-spec get_day(MS::mission_segment()) -> stanag_types:i8_int().
get_day(#mission_segment{day = D}) -> D.

%% @doc Convenience function to allow a caller to extract all of the 
%% reference time fields (which is really a date) as a single tuple.
-spec get_time(MS::mission_segment()) -> Date
    when Date :: {Year, Month, Day},
        Year :: stanag_types:i16_int(),
        Month :: stanag_types:i8_int(),
        Day :: stanag_types:i8_int().
get_time(#mission_segment{year = Y, month = M, day = D}) -> {Y, M, D}.

