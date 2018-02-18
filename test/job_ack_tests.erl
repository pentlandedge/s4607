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

-module(job_ack_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the Job Acknowledge segment functions. 
job_ack_test_() ->
    Bin = sample_job_ack(),
    {ok, JA} = job_ack:decode(Bin),
    [
     ?_assertEqual(16#12345678, job_ack:get_job_id(JA)),
     ?_assertEqual("Job Ack ID", job_ack:get_requestor_id(JA))
    ].

%% Return a binary job acknowledge segment to use as test data.
sample_job_ack() ->
    <<16#12345678:32,
      "Job Ack ID",
      "JAckTaskID",
      27,
      "Darth ",
      99,
      64,0,0,0, 245,85,85,85, 64,0,0,0, 245,85,85,85, 
      64,0,0,0, 245,85,85,85, 64,0,0,0, 245,85,85,85, 
      21, 
      16#123:16, 
      16#1234:16,
      2,
      2018:16,5,23,10,11,50,
      "XN">>.

