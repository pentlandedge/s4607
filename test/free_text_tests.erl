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
    [decode_checks(), new_checks(), new_fail_checks(), encode_checks(),
     to_dict_checks(), payload_size_checks()].

decode_checks() ->
    Bin = sample_free_text(),
    {ok, FT} = free_text:decode(Bin),
    [?_assertEqual("ABCDEFGHIJ", free_text:get_originator(FT)),
     ?_assertEqual("1234567890", free_text:get_recipient(FT)),
     ?_assertEqual("Some free text", free_text:get_text(FT))].

new_checks() ->
    {ok, FT} = free_text:new("ABC", "DEF", "Some important message"),
    free_text:display(FT),
    [?_assertEqual("ABC", free_text:get_originator(FT)),
     ?_assertEqual("DEF", free_text:get_recipient(FT)),
     ?_assertEqual("Some important message", free_text:get_text(FT))].

new_fail_checks() ->
    {error, Reason1} = free_text:new("ABC", "DEF", "£3.50 please"),
    {error, Reason2} = free_text:new("ABC", "£DEF", "$3.50 please"),
    {error, Reason3} = free_text:new("£ABC", "DEF", "$3.50 please"),
    [?_assertEqual(invalid_characters, Reason1),
     ?_assertEqual(invalid_characters, Reason2),
     ?_assertEqual(invalid_characters, Reason3)].

encode_checks() ->
    {ok, FT} = free_text:new("ABC", "DEF", "Some important message"),
    Bin = free_text:encode(FT),
    {ok, FT2} = free_text:decode(Bin),
    [?_assertEqual("ABC       ", free_text:get_originator(FT2)),
     ?_assertEqual("DEF       ", free_text:get_recipient(FT2)),
     ?_assertEqual("Some important message", free_text:get_text(FT2))].

to_dict_checks() ->
    Text = "The gas was on in the Institute,", 
    {ok, FT} = free_text:new("A", "B", Text),
    D1 = free_text:to_dict(FT),
    [?_assertEqual("A", dict:fetch(originator, D1)),
     ?_assertEqual("B", dict:fetch(recipient, D1)),
     ?_assertEqual(Text, dict:fetch(text, D1))].

payload_size_checks() ->
    {ok, FT} = free_text:new("ABC", "DEF", "words"),
    [?_assertEqual(25, free_text:payload_size(FT))].

sample_free_text() -> <<"ABCDEFGHIJ","1234567890","Some free text">>.
