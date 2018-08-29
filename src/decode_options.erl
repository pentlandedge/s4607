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
-module(decode_options).

-export([valid_option/1, valid_option_list/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

-type decode_option() :: strict |
                         permissive | 
                         stop_on_error | 
                         continue_on_error. 

-export_type([decode_option/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function definitions.

%% @doc Declare the valid decode options
-spec valid_option(atom()) -> boolean().
valid_option(strict) -> 
    true;
valid_option(permissive) -> 
    true;
valid_option(stop_on_error) -> 
    true;
valid_option(continue_on_error) -> 
    true;
valid_option(Option) when is_atom(Option) -> 
    false.

%% @doc Check that all supplied decode options are valid.
-spec all_valid_options(Options) -> boolean() when
    Options :: [atom()].
all_valid_options(Options) ->
    lists:all(fun valid_option/1, Options).

%% @doc Check that the set of options is valid.
-spec valid_option_list(Options) -> boolean when
    Options :: [atom()].
valid_option_list(Options) when is_list(Options) ->
    all_valid_options(Options) and 
    (not contains_invalid_combination(Options)).

%% @doc Check for invalid combinations of options. 
-spec contains_invalid_combination(Options) -> boolean() when
    Options :: [atom()].
contains_invalid_combination(Options) when is_list(Options) -> 
    M = fun(X) ->
            lists:member(X, Options)
        end,

    (M(strict) and M(permissive)) or 
    (M(stop_on_error) and M(continue_on_error)).

