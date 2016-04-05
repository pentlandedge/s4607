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

-module(free_text_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the free text segment. 
free_text_test_() ->
    [decode_checks(), new_checks(), encode_checks()].

decode_checks() ->
    Bin = sample_free_text(),
    FT = free_text:decode(Bin),
    [?_assertEqual("ABCDEFGHIJ", free_text:get_originator(FT)),
     ?_assertEqual("1234567890", free_text:get_recipient(FT)),
     ?_assertEqual("Some free text", free_text:get_text(FT))].

new_checks() ->
    {ok, FT} = free_text:new("ABC", "DEF", "Some important message"),
    [?_assertEqual("ABC", free_text:get_originator(FT)),
     ?_assertEqual("DEF", free_text:get_recipient(FT)),
     ?_assertEqual("Some important message", free_text:get_text(FT))].

encode_checks() ->
    {ok, FT} = free_text:new("ABC", "DEF", "Some important message"),
    Bin = free_text:encode(FT),
    FT2 = free_text:decode(Bin),
    [?_assertEqual("ABC       ", free_text:get_originator(FT2)),
     ?_assertEqual("DEF       ", free_text:get_recipient(FT2)),
     ?_assertEqual("Some important message", free_text:get_text(FT2))].

sample_free_text() -> <<"ABCDEFGHIJ","1234567890","Some free text">>.
