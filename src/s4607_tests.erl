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

-module(s4607_tests).

-include_lib("eunit/include/eunit.hrl").

s4607_test_() ->
    [encode_mission_segment_check(), encode_job_def_segment_check(),
     encode_dwell_segment_check1(), encode_dwell_segment_check2()].
   
encode_mission_segment_check() ->
    % Create a mission segment.
    MS = mission:new("Drifter 1", "A1234", other, "Build 1", 2016, 2, 5),

    % Create a segment header.
    SegSize = seg_header:header_size() + mission:payload_size(),
    SH = seg_header:new(mission, SegSize),

    % Create a complete segment with the header and payload.
    Seg = s4607:new_segment(SH, MS),

    % Encode the segment.
    {ok, ES} = s4607:segment_encode(Seg),

    % Decode the segment again.
    [{_, _, DS}] = s4607:decode_segments(ES, []),

    % Check the fields in the decoded segment match what we expect.
    [?_assertEqual("Drifter 1", mission:get_mission_plan(DS)),
     ?_assertEqual("A1234", mission:get_flight_plan(DS)),
     ?_assertEqual(other, mission:get_platform_type(DS)),
     ?_assertEqual("Build 1", mission:get_platform_config(DS)),
     ?_assertEqual(2016, mission:get_year(DS)),
     ?_assertEqual(2, mission:get_month(DS)),
     ?_assertEqual(5, mission:get_day(DS))].

encode_job_def_segment_check() ->
    % Create a job definition segment.
    JD = job_def_tests:sample_job_def(),

    % Create a segment header.
    SegSize = seg_header:header_size() + job_def:payload_size(),
    SH = seg_header:new(job_definition, SegSize),

    % Create the complete segment record.
    Seg = s4607:new_segment(SH, JD),
    
    % Encode the segment.
    {ok, ES} = s4607:segment_encode(Seg),

    % Decode the segment again.
    [{_, _, DS}] = s4607:decode_segments(ES, []),

    % Check a couple of the fields. 
    [?_assertEqual(rotary_wing_radar, job_def:get_sensor_id_type(DS)),
     ?_assertEqual(dgm50, job_def:get_terr_elev_model(DS))].

encode_dwell_segment_check1() ->
    % Create a dwell segment.
    Dwell = dwell_tests:one_target_dwell(),
    
    % Create a segment header.
    SegSize = seg_header:header_size() + dwell:payload_size(Dwell),
    SH = seg_header:new(dwell, SegSize),

    % Create the complete segment record.
    Seg = s4607:new_segment(SH, Dwell),
    
    % Encode the segment.
    {ok, ES} = s4607:segment_encode(Seg),

    % Decode the segment again.
    [{_, _, DS}] = s4607:decode_segments(ES, []),

    % Fetch the target report.
    [TR] = dwell:get_targets(DS),

    % Check a couple of the fields. 
    [?_assertEqual(34, tgt_report:get_mti_report_index(TR)),
     ?_assertEqual(vehicle_live_target, tgt_report:get_target_classification(TR))].

encode_dwell_segment_check2() ->
    % Create a dwell segment.
    Dwell = dwell_tests:minimal_dwell(),
    
    % Create a segment header.
    SegSize = seg_header:header_size() + dwell:payload_size(Dwell),
    SH = seg_header:new(dwell, SegSize),

    % Create the complete segment record.
    Seg = s4607:new_segment(SH, Dwell),
    
    % Encode the segment.
    {ok, ES} = s4607:segment_encode(Seg),

    % Decode the segment again.
    [{_, _, DS}] = s4607:decode_segments(ES, []),

    % Check a couple of the fields. 
    [?_assertEqual(100, dwell:get_revisit_index(DS)),
     ?_assertEqual(-10000, dwell:get_sensor_alt(DS))].
