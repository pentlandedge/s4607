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
%% This module contains functions relating the the BCS (Basic Character Set)
%% defined in the Stanag 4607 standard.

-module(bcs).

-export([is_valid_string/1, is_valid_char/1]).

%% Function to test whether a string contains only valid BCS characters.
is_valid_string([]) -> 
    true;
is_valid_string([H|T]) ->
    case is_valid_char(H) of
        true -> is_valid_string(T);
        false -> false
    end.

%% Function to test whether a character belongs to the BCS set.
%% Allows the caller to provide a character as a single element string or as
%% an integer
is_valid_char(16#0A) -> true;   % Line feed
is_valid_char(16#0C) -> true;   % Form feed
is_valid_char(16#0D) -> true;   % Carriage return
is_valid_char(X) when is_integer(X), X >= 16#20, X =< 16#7E -> true;
is_valid_char(_) -> false.

