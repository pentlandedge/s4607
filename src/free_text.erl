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
    new/3,
    payload_size/1,
    to_dict/1,
    display/1,
    get_originator/1, 
    get_recipient/1, 
    get_text/1]).

-record(free_text, {originator, recipient, text}).

-define(ORIGINATOR_LENGTH, 10).
-define(RECIPIENT_LENGTH, 10).

%% Function to decode a binary free text segment.
decode(<<Orig:?ORIGINATOR_LENGTH/binary,Recip:?RECIPIENT_LENGTH/binary,
         Text/binary>>) ->
    #free_text{
        originator = binary_to_list(Orig), 
        recipient = binary_to_list(Recip), 
        text = binary_to_list(Text)}. 

%% Function to encode a free text record as a binary.
encode(#free_text{originator = Or, recipient = Re, text = Text}) ->
    PadOr = sutils:add_trailing_spaces(Or, ?ORIGINATOR_LENGTH),
    PadRe = sutils:add_trailing_spaces(Re, ?RECIPIENT_LENGTH),
    list_to_binary(PadOr ++ PadRe ++ Text).

%% Function to create a new free text record. Checks that the supplied 
%% originator and recipient parameters are within the maximum length and all
%% strings contain only valid BCS characters.
new(Orig, Recip, Text) when is_list(Orig), is_list(Recip), is_list(Text),
    length(Orig) =< ?ORIGINATOR_LENGTH, length(Recip) =< ?RECIPIENT_LENGTH ->
    % Check that the strings contain only valid BCS characters.
    OrigOk = bcs:is_valid_string(Orig),
    RecipOk = bcs:is_valid_string(Recip),
    TextOk = bcs:is_valid_string(Text),
    case OrigOk and RecipOk and TextOk of 
        true ->
            {ok, new0(Orig, Recip, Text)};
        false ->
            {error, invalid_characters}
    end.

%% Internal function to build the free_text record. Performs no parameter 
%% checking.
new0(Orig, Recip, Text) ->
    #free_text{originator = Orig, recipient = Recip, text = Text}.

%% Function to return the expected payload size for the free text segment.
payload_size(#free_text{text = Text}) ->
    ?ORIGINATOR_LENGTH + ?RECIPIENT_LENGTH + length(Text).

%% Function to convert a free text record into a dictionary.
to_dict(#free_text{originator = Orig, recipient = Recip, text = Text}) ->
    D1 = dict:new(),
    D2 = dict:store(originator, Orig, D1),
    D3 = dict:store(recipient, Recip, D2),
    dict:store(text, Text, D3).

%% Function to display the contents of a free text segment.
display(#free_text{originator = Orig, recipient = Recip, text = Text}) ->
    io:format("****************************************~n"),
    io:format("** @free_text~n"),
    io:format("originator: ~p~n", [Orig]),
    io:format("recipient: ~p~n", [Recip]),
    io:format("text: ~p~n", [Text]).
    
% Accessor functions.
get_originator(#free_text{originator = X}) -> X.
get_recipient(#free_text{recipient = X}) -> X.
get_text(#free_text{text = X}) -> X.

