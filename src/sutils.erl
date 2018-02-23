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
%% @doc Utility functions for Stanag 4607 used in more than one module.
%%

-module(sutils).

-export([
    trim_trailing_spaces/1,
    add_trailing_spaces/2,
    conditional_extract/5,
    conditional_display/3,
    conditional_format/3,
    extract_data/2,
    extract_conv_data/3,
    extract_param_or_default/3,
    encode_param_list/2]).

-type mask_bit() :: 0..1.
-export_type[mask_bit/0].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility functions

% @doc Remove trailing space characters (not all whitespace) from a
% list
-spec trim_trailing_spaces(Str::string()) -> string().
trim_trailing_spaces(Str) ->
    Rev = lists:reverse(Str),
    F = fun(C) -> C =:= $\  end,
    RStrip = lists:dropwhile(F, Rev),
    lists:reverse(RStrip).

% @doc Add spaces to the specified string to make it of length N.
-spec add_trailing_spaces(Str::string(), N::non_neg_integer()) -> string().
add_trailing_spaces(Str, N) when length(Str) < N ->
    % Inelegant list splice, creates a list of spaces first.
    Trail = [$  || _ <- lists:seq(1, N - length(Str))],
    lists:append(Str, Trail);
add_trailing_spaces(Str, N) when length(Str) =:= N ->
    Str.

%% @doc Conditionally extract a paramater from the front of a binary
%% based on the state of a mask bit. If the mask bit is set, then the 
%% parameter is extracted and the conversion function is applied. If the 
%% mask bit is not set then no parameter is extracted and the default value
%% is returned.
-spec conditional_extract(Bin, MaskBit, Size, ConvFn, Default) -> {Val, Rem}
    when Bin :: binary(),
    MaskBit :: mask_bit(),
    Size :: non_neg_integer(),
    ConvFn :: function(),
    Default :: any(),
    Val :: any(),
    Rem :: binary().
conditional_extract(Bin, MaskBit, Size, ConvFn, Default) ->
    case MaskBit of
        1 ->
            {ok, Param, Bin1} = extract_data(Bin, Size),
            {ConvFn(Param), Bin1};
        0 ->
            {Default, Bin}
    end.

%% @doc Conditionally display a parameter based on a mask bit. If the bit is
%% set then the parameters are displayed. No action is taken if the mask bit
%% is not set.
-spec conditional_display(FmtStr, Params, MaskBit) -> ok 
    when FmtStr :: string(),
    Params :: list(),
    MaskBit :: mask_bit().
conditional_display(FmtStr, Params, MaskBit) ->
    case MaskBit of
        1 ->
            io:format(FmtStr, Params),
            ok;
        0 ->
            ok
    end.

%% @doc Conditionally format a string based on a mask bit. If the bit is
%% set then the format is performed. If the bit is not set, then a comma to
%% represent an empty field is returned.
-spec conditional_format(FmtStr, Params, MaskBit) -> iolist() 
    when FmtStr :: string(),
    Params :: list(),
    MaskBit :: mask_bit().
conditional_format(FmtStr, Params, MaskBit) ->
    case MaskBit of
        1 -> io_lib:format(FmtStr, Params); 
        0 -> "," 
    end.

%% @doc Take the first part of a binary and return the rest.
-spec extract_data(Bin::binary(), Len) -> {ok, Data, Rem}
    when Len :: non_neg_integer(),
    Data :: binary(),
    Rem :: binary().
extract_data(Bin, Len) ->
    Data = binary:part(Bin, 0, Len),
    Rem = binary:part(Bin, Len, (byte_size(Bin) - Len)),
    {ok, Data, Rem}.

%% @doc Take the first part of a binary, apply the conversion function to it
%% and return the rest.
-spec extract_conv_data(Bin, Len, ConvFn) -> {ok, Ret, Rem}
    when Bin :: binary(),
    Len :: non_neg_integer(),
    ConvFn :: function(),
    Ret :: any(),
    Rem :: binary().
extract_conv_data(Bin, Len, ConvFn) ->
    Data = binary:part(Bin, 0, Len),
    Rem = binary:part(Bin, Len, (byte_size(Bin) - Len)),
    {ok, ConvFn(Data), Rem}.

%% @doc Extract a parameter from the list or use a default.
-spec extract_param_or_default(Param, ParamList, Default) -> any()
    when Param::atom(), ParamList::list(), Default::any().

extract_param_or_default(Param, ParamList, Default) 
    when is_atom(Param), is_list(ParamList) ->

    case lists:keyfind(Param, 1, ParamList) of
        {Param, V} -> V;
        false  -> Default
    end.

%% @doc Encode a list of parameters as an iolist.
%% Each entry in ParamList should be a {GetFun, EncFun} tuple. The GetFun and
%% EncFun parameters must accept a Rec argument."
encode_param_list(Rec, ParamList) when is_list(ParamList) ->
    % Function to encode each parameter in the list. Does not stitch together
    % into a single binary: just return an iolist which can be flattened by 
    % the caller if required.
    F = fun({GetFun, EncFun}) ->
            P = GetFun(Rec),
            EncFun(P)
        end,

    lists:map(F, ParamList).

