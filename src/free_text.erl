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
-module(free_text).

-export([
    decode/1, 
    encode/1,
    get_originator/1, 
    get_recipient/1, 
    get_text/1]).

-record(free_text, {originator, recipient, text}).

%% Function to decode a binary free text segment.
decode(<<Orig:10/binary,Recip:10/binary,Text/binary>>) ->
    #free_text{
        originator = binary_to_list(Orig), 
        recipient = binary_to_list(Recip), 
        text = binary_to_list(Text)}. 

%% Function to encode a free text record as a binary.
encode(#free_text{originator = Or, recipient = Re, text = Text}) ->
    PadOr = sutils:add_trailing_spaces(Or, 10),
    PadRe = sutils:add_trailing_spaces(Re, 10),
    <<PadOr,PadRe,Text>>.

% Accessor functions.
get_originator(#free_text{originator = X}) -> X.
get_recipient(#free_text{recipient = X}) -> X.
get_text(#free_text{text = X}) -> X.

