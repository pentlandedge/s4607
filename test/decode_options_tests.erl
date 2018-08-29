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

-module(decode_options_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator.
decode_option_test_() ->
    [valid_option_checks(), invalid_option_checks()].

valid_option_checks() ->
    [?_assert(decode_options:valid_option(strict)), 
     ?_assert(decode_options:valid_option(permissive)), 
     ?_assert(decode_options:valid_option(stop_on_error)), 
     ?_assert(decode_options:valid_option(continue_on_error))
    ].

invalid_option_checks() ->
    [?_assert(not decode_options:valid_option(anything_goes))].

