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

-module(bcs_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator for the BCS character set functions.
bcs_test_() ->
    [valid_checks(), invalid_checks()].

valid_checks() ->
    [?_assert(bcs:is_valid_string("A")), 
     ?_assert(bcs:is_valid_string("a")), 
     ?_assert(bcs:is_valid_char(16#0A)),     % Line feed 
     ?_assert(bcs:is_valid_char(16#0C)),     % Form feed 
     ?_assert(bcs:is_valid_char(16#0D)),     % Carriage return
     ?_assert(bcs:is_valid_string("\r")),      % Carriage return
     ?_assert(bcs:is_valid_string("That will be $4.50 please.")),
     ?_assert(bcs:is_valid_string("\~"))].     

invalid_checks() ->
    [?_assert(not bcs:is_valid_string("£")), 
     ?_assert(not bcs:is_valid_string("That will be £4.50 please.")),
     ?_assert(not bcs:is_valid_char(16#08))].    % Backspace

