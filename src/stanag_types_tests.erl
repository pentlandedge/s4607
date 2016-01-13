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
