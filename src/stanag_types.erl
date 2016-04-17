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

-module(stanag_types).

-export([
    i8_to_integer/1,
    integer_to_i8/1,
    i16_to_integer/1,
    integer_to_i16/1,
    i32_to_integer/1,
    integer_to_i32/1,
    s8_to_integer/1,
    integer_to_s8/1,
    s16_to_integer/1,
    integer_to_s16/1,
    s32_to_integer/1,
    integer_to_s32/1,
    s64_to_integer/1,
    integer_to_s64/1,
    b16_to_float/1,
    float_to_b16/1,
    b32_to_float/1,
    float_to_b32/1,
    h32_to_float/1,
    float_to_h32/1,
    ba16_to_float/1,
    float_to_ba16/1,
    ba32_to_float/1,
    float_to_ba32/1,
    sa16_to_float/1,
    float_to_sa16/1,
    sa32_to_float/1,
    float_to_sa32/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

-type i8()  :: <<_:8>>.
-type i16() :: <<_:16>>.
-type i32() :: <<_:32>>.

-type i8_int()  :: 0..255.
-type i16_int() :: 0..65535.
-type i32_int() :: 0..4294967296.

-export_type([i8/0, i16/0, i32/0]).
-export_type([i8_int/0, i16_int/0, i32_int/0]).

-type s8()  :: -128..127.
-type s16() :: -32768..32767.
-type s32() :: -2147483648..2147483647.
-type s64() :: -9223372036854775808..9223372036854775807.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unsigned integer type conversion functions. 

%% @doc Function to convert unsigned I8 binary to an integer.
-spec i8_to_integer(i8()) -> i8_int().
i8_to_integer(<<X>>) -> X.

%% @doc Function to convert an unsigned integer to fixed width I8 binary.
-spec integer_to_i8(i8_int()) -> i8().
integer_to_i8(I) when I >= 0, I =< 255 ->
    <<I:8/integer-unsigned-big>>.

%% @doc Function to convert unsigned I16 binary to an integer.
-spec i16_to_integer(i16()) -> i16_int().
i16_to_integer(<<X:16/integer-unsigned-big>>) -> X.

%% @doc Function to convert an unsigned integer to fixed width I16 binary.
-spec integer_to_i16(i16_int()) -> i16().
integer_to_i16(I) when I >= 0, I =< 65535 ->
    <<I:16/integer-unsigned-big>>.

%% @doc Function to convert unsigned I32 binary to an integer.
-spec i32_to_integer(i32()) -> i32_int().
i32_to_integer(<<X:32/integer-unsigned-big>>) -> X.

%% @doc Function to convert an unsigned integer to fixed width I32 binary.
-spec integer_to_i32(i32_int()) -> i32().
integer_to_i32(I) when I >= 0, I =< 4294967296 ->
    <<I:32/integer-unsigned-big>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Signed integer type conversion functions. 

%% @doc Function to convert a signed S8 binary to an integer.
-spec s8_to_integer(binary()) -> s8().
s8_to_integer(<<X:8/integer-signed-big>>) -> X.

%% @doc Function to convert a signed integer to fixed width S8 binary.
-spec integer_to_s8(s8()) -> binary().
integer_to_s8(I) when I >= -128, I =< 127 ->
    <<I:8/integer-signed-big>>.

%% @doc Function to convert a signed S16 binary to an integer.
-spec s16_to_integer(binary()) -> s16().
s16_to_integer(<<X:16/integer-signed-big>>) -> X.

%% @doc Function to convert a signed integer to fixed width S16 binary.
-spec integer_to_s16(s16()) -> binary().
integer_to_s16(I) when I >= -32768, I =< 32767 ->
    <<I:16/integer-signed-big>>.

%% @doc Function to convert a signed S32 binary to an integer.
-spec s32_to_integer(binary()) -> s32().
s32_to_integer(<<X:32/integer-signed-big>>) -> X.

%% @doc Function to convert a signed integer to fixed width S32 binary.
-spec integer_to_s32(s32()) -> binary().
integer_to_s32(I) when I >= -2147483648, I =< 2147483647 ->
    <<I:32/integer-signed-big>>.

%% @doc Function to convert a signed S64 binary to an integer.
-spec s64_to_integer(binary()) -> s64().
s64_to_integer(<<X:64/integer-signed-big>>) -> X.

%% @doc Function to convert a signed integer to fixed width S64 binary.
-spec integer_to_s64(s64()) -> binary().
integer_to_s64(I) when I >= -9223372036854775808, I =< 9223372036854775807 ->
    <<I:64/integer-signed-big>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Signed binary decimal conversion functions.

b16_to_float(<<S:1,I:8/integer-unsigned,F:7/integer-unsigned>>) ->
    Val = I + F / 128.0,
    case S of 
        1 -> -Val;
        0 -> Val
    end.

float_to_b16(X) when X >= 0 ->
    Scaled = round(X * 128),
    <<Scaled:16/integer-unsigned-big>>;
float_to_b16(X) ->
    Scaled = round(abs(X) * 128),
    <<1:1,Scaled:15/integer-unsigned-big>>.

b32_to_float(<<S:1,I:8/integer-unsigned,F:23/integer-unsigned>>) ->
    Val = I + F / 8388608.0,
    case S of 
        1 -> -Val;
        0 -> Val
    end.

float_to_b32(X) when X >= 0 ->
    Scaled = round(X * 8388608),
    <<Scaled:32/integer-unsigned-big>>;
float_to_b32(X) ->
    Scaled = round(abs(X) * 8388608),
    <<1:1,Scaled:31/integer-unsigned-big>>.

h32_to_float(<<S:1,I:15/integer-unsigned,F:16/integer-unsigned>>) ->
    Val = I + F / 65536.0,
    case S of 
        1 -> -Val;
        0 -> Val
    end.

float_to_h32(X) when X >= 0 ->
    Scaled = round(X * 65536),
    <<Scaled:32/integer-unsigned-big>>;
float_to_h32(X) ->
    Scaled = round(abs(X) * 65536),
    <<1:1,Scaled:31/integer-unsigned-big>>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Binary angle conversion functions.

ba16_to_float(<<X:16/integer-unsigned-big>>) ->
    X * 1.40625 / 256.0.

float_to_ba16(X) ->
    Val = round(X * (64.0 / 45.0) * 128),
    <<Val:16/integer-unsigned-big>>.

ba32_to_float(<<X:32/integer-unsigned-big>>) ->
    X * 1.40625 / 16777216.0.

float_to_ba32(X) ->
    Val = round(X * (64.0 / 45.0) * 8388608),
    <<Val:32/integer-unsigned-big>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Signed binary angle conversion functions.
    
sa16_to_float(<<X:16/integer-signed-big>>) ->
    X * 1.40625 / 512.

float_to_sa16(X) ->
    Val = round(X * (64.0 / 45.0) * 256),
    <<Val:16/integer-signed-big>>.
    
sa32_to_float(<<X:32/integer-signed-big>>) ->
    X * 1.40625 / 33554432.

float_to_sa32(X) ->
    Val = round(X * (64.0 / 45.0) * 16777216),
    <<Val:32/integer-signed-big>>.

