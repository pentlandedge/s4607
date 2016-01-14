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
-module(stanag_types_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the unsigned integer types.
unsigned_test_() ->
    [i8_checks(), i16_checks(), i32_checks()].

i8_checks() ->
    [?_assertEqual(255, stanag_types:i8_to_integer(<<255>>)),    
     ?_assertEqual(250, stanag_types:i8_to_integer(<<250>>)),    
     ?_assertEqual(0, stanag_types:i8_to_integer(<<0>>)),
    
     ?_assertEqual(<<255>>, stanag_types:integer_to_i8(255)),
     ?_assertEqual(<<250>>, stanag_types:integer_to_i8(250)),
     ?_assertEqual(<<0>>, stanag_types:integer_to_i8(0))].

i16_checks() ->
    [?_assertEqual(255, stanag_types:i16_to_integer(<<0,255>>)),
     ?_assertEqual(65280, stanag_types:i16_to_integer(<<255,0>>)),
     
     ?_assertEqual(<<0,255>>, stanag_types:integer_to_i16(255)),
     ?_assertEqual(<<255,0>>, stanag_types:integer_to_i16(65280)),
     ?_assertEqual(<<255,255>>, stanag_types:integer_to_i16(65535))]. 

i32_checks() ->
    [?_assertEqual(255, stanag_types:i32_to_integer(<<0,0,0,255>>)),
     ?_assertEqual(4294967295, stanag_types:i32_to_integer(<<255,255,255,255>>)),
     ?_assertEqual(16909060, stanag_types:i32_to_integer(<<1,2,3,4>>)),

     ?_assertEqual(<<1,2,3,4>>, stanag_types:integer_to_i32(16909060)),
     ?_assertEqual(<<0,0,255,255>>, stanag_types:integer_to_i32(65535))]. 

%% Define a test generator for the signed integer types.
signed_test_() ->
    [s8_checks()].

s8_checks() ->
    [?_assertEqual(-1, stanag_types:s8_to_integer(<<255>>)),
     ?_assertEqual(127, stanag_types:s8_to_integer(<<127>>)),
     ?_assertEqual(0, stanag_types:s8_to_integer(<<0>>)),

     ?_assertEqual(127, stanag_types:s8_to_integer(<<127>>)),
     ?_assertEqual(-128, stanag_types:s8_to_integer(<<128>>))].
    
