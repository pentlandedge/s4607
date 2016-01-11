-module(stanag_types_tests).

-include_lib("eunit/include/eunit.hrl").

i8_test() ->
    255 = stanag_types:i8_to_integer(<<255>>),    
    250 = stanag_types:i8_to_integer(<<250>>),    
    0   = stanag_types:i8_to_integer(<<0>>),
    
    <<255>> = stanag_types:integer_to_i8(255),
    <<250>> = stanag_types:integer_to_i8(250),
    <<0>>   = stanag_types:integer_to_i8(0).
