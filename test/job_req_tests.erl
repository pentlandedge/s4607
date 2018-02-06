%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2018 Pentland Edge Ltd.
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

-module(job_req_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the Job Request segment functions. 
job_req_test_() ->
    [valid_checks()].

valid_checks() ->
    Bin = sample_job_request(),
    {ok, JR} = job_req:decode(Bin),
    job_req:display(JR),
    ReqID = job_req:get_requestor_id(JR),
    TaskID = job_req:get_requestor_task_id(JR),
    Pri = job_req:get_requestor_priority(JR),
    [?_assertEqual("Job Req ID", ReqID),
     ?_assertEqual("JReqTaskID", TaskID),
     ?_assertEqual(default_priority, Pri),
     ?_assert(almost_equal(45.0, job_req:get_bounding_a_lat(JR), 0.00001)),
     ?_assert(almost_equal(345.0, job_req:get_bounding_a_lon(JR), 0.00001))
    ].

sample_job_request() ->
    <<"Job Req ID","JReqTaskID",0,
      64,0,0,0, 245,85,85,85, 64,0,0,0, 245,85,85,85, 64,0,0,0,
      245,85,85,85, 64,0,0,0, 245,85,85,85>>.

%% Utility function to compare whether floating point values are within a
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
