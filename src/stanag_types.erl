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
%% @doc The Stanag 4607 standard defines a number of types which are used 
%%      throughout the document. This module defines each of these types 
%%      and provides functions to convert to and from the binary encodings.

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
%% Integer range limits.

-define(MIN_I8, 0).
-define(MAX_I8, 255).

-define(MIN_I16, 0).
-define(MAX_I16, 65535).

-define(MIN_I32, 0).
-define(MAX_I32, 4294967295).

-define(MIN_S8, -128).
-define(MAX_S8, 127).

-define(MIN_S16, -32768).
-define(MAX_S16, 32767).

-define(MIN_S32, -2147483648).
-define(MAX_S32, 2147483647).

-define(MIN_S64, -9223372036854775808).
-define(MAX_S64, 9223372036854775807).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

%% Unsigned integer types.
-type i8()  :: <<_:8>>.
-type i16() :: <<_:16>>.
-type i32() :: <<_:32>>.

-type i8_int()  :: 0..255.
-type i16_int() :: 0..65535.
-type i32_int() :: 0..4294967295.

-export_type([i8/0, i16/0, i32/0]).
-export_type([i8_int/0, i16_int/0, i32_int/0]).

%% Signed integers.
-type s8()  :: <<_:8>>.
-type s16() :: <<_:16>>.
-type s32() :: <<_:32>>.
-type s64() :: <<_:64>>.

-type s8_int()  :: -128..127.
-type s16_int() :: -32768..32767.
-type s32_int() :: -2147483648..2147483647.
-type s64_int() :: -9223372036854775808..9223372036854775807.

-export_type([s8/0, s16/0, s32/0, s64/0]).
-export_type([s8_int/0, s16_int/0, s32_int/0, s64_int/0]).

%% Binary decimal formats.
-type b16() :: <<_:16>>.
-type b32() :: <<_:32>>.
-type h32() :: <<_:32>>.

-export_type([b16/0, b32/0, h32/0]).

%% Binary angle formats (unsigned and signed).
-type ba16() :: <<_:16>>.
-type ba32() :: <<_:32>>.

-type sa16() :: <<_:16>>.
-type sa32() :: <<_:32>>.

-export_type([ba16/0, ba32/0, sa16/0, sa32/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unsigned integer type conversion functions. 

%% @doc Convert an unsigned I8 binary to an integer.
-spec i8_to_integer(i8()) -> i8_int().
i8_to_integer(<<X>>) -> X.

%% @doc Convert an unsigned integer to fixed width I8 binary.
-spec integer_to_i8(i8_int()) -> i8().
integer_to_i8(I) when I >= ?MIN_I8, I =< ?MAX_I8 ->
    <<I:8/integer-unsigned-big>>.

%% @doc Convert an unsigned I16 binary to an integer.
-spec i16_to_integer(i16()) -> i16_int().
i16_to_integer(<<X:16/integer-unsigned-big>>) -> X.

%% @doc Convert an unsigned integer to fixed width I16 binary.
-spec integer_to_i16(i16_int()) -> i16().
integer_to_i16(I) when I >= ?MIN_I16, I =< ?MAX_I16 ->
    <<I:16/integer-unsigned-big>>.

%% @doc Convert an unsigned I32 binary to an integer.
-spec i32_to_integer(i32()) -> i32_int().
i32_to_integer(<<X:32/integer-unsigned-big>>) -> X.

%% @doc Convert an unsigned integer to fixed width I32 binary.
-spec integer_to_i32(i32_int()) -> i32().
integer_to_i32(I) when I >= ?MIN_I32, I =< ?MAX_I32 ->
    <<I:32/integer-unsigned-big>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Signed integer type conversion functions. 

%% @doc Convert a signed S8 binary to an integer.
-spec s8_to_integer(s8()) -> s8_int().
s8_to_integer(<<X:8/integer-signed-big>>) -> X.

%% @doc Convert a signed integer to fixed width S8 binary.
-spec integer_to_s8(s8_int()) -> s8().
integer_to_s8(I) when I >= ?MIN_S8, I =< ?MAX_S8 ->
    <<I:8/integer-signed-big>>.

%% @doc Convert a signed S16 binary to an integer.
-spec s16_to_integer(s16()) -> s16_int().
s16_to_integer(<<X:16/integer-signed-big>>) -> X.

%% @doc Convert a signed integer to fixed width S16 binary.
-spec integer_to_s16(s16_int()) -> s16().
integer_to_s16(I) when I >= ?MIN_S16, I =< ?MAX_S16 ->
    <<I:16/integer-signed-big>>.

%% @doc Convert a signed S32 binary to an integer.
-spec s32_to_integer(s32()) -> s32_int().
s32_to_integer(<<X:32/integer-signed-big>>) -> X.

%% @doc Convert a signed integer to fixed width S32 binary.
-spec integer_to_s32(s32_int()) -> s32().
integer_to_s32(I) when I >= ?MIN_S32, I =< ?MAX_S32 ->
    <<I:32/integer-signed-big>>.

%% @doc Convert a signed S64 binary to an integer.
-spec s64_to_integer(s64()) -> s64_int().
s64_to_integer(<<X:64/integer-signed-big>>) -> X.

%% @doc Convert a signed integer to fixed width S64 binary.
-spec integer_to_s64(s64_int()) -> s64().
integer_to_s64(I) when I >= ?MIN_S64, I =< ?MAX_S64 ->
    <<I:64/integer-signed-big>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Signed binary decimal conversion functions.

%% @doc Convert a 16-bit signed binary decimal to a float.
-spec b16_to_float(b16()) -> float().
b16_to_float(<<S:1,I:8/integer-unsigned,F:7/integer-unsigned>>) ->
    Val = I + F / 128.0,
    case S of 
        1 -> -Val;
        0 -> Val
    end.

%% @doc Convert a float to a 16-bit signed binary decimal.
-spec float_to_b16(float()) -> b16().
float_to_b16(X) when X >= 0 ->
    Scaled = round(X * 128),
    <<Scaled:16/integer-unsigned-big>>;
float_to_b16(X) ->
    Scaled = round(abs(X) * 128),
    <<1:1,Scaled:15/integer-unsigned-big>>.

%% @doc Convert a 32-bit signed binary decimal to a float.
-spec b32_to_float(b32()) -> float().
b32_to_float(<<S:1,I:8/integer-unsigned,F:23/integer-unsigned>>) ->
    Val = I + F / 8388608.0,
    case S of 
        1 -> -Val;
        0 -> Val
    end.

%% @doc Convert a float to a 32-bit signed binary decimal.
-spec float_to_b32(float()) -> b32().
float_to_b32(X) when X >= 0 ->
    Scaled = round(X * 8388608),
    <<Scaled:32/integer-unsigned-big>>;
float_to_b32(X) ->
    Scaled = round(abs(X) * 8388608),
    <<1:1,Scaled:31/integer-unsigned-big>>.

%% @doc Convert a high range 32-bit signed binary decimal to a float.
-spec h32_to_float(h32()) -> float().
h32_to_float(<<S:1,I:15/integer-unsigned,F:16/integer-unsigned>>) ->
    Val = I + F / 65536.0,
    case S of 
        1 -> -Val;
        0 -> Val
    end.

%% @doc Convert a float to a high range 32-bit signed binary decimal.
-spec float_to_h32(float()) -> h32().
float_to_h32(X) when X >= 0 ->
    Scaled = round(X * 65536),
    <<Scaled:32/integer-unsigned-big>>;
float_to_h32(X) ->
    Scaled = round(abs(X) * 65536),
    <<1:1,Scaled:31/integer-unsigned-big>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Binary angle conversion functions.

%% @doc Convert a 16-bit binary angle to a float.
-spec ba16_to_float(ba16()) -> float().
ba16_to_float(<<X:16/integer-unsigned-big>>) ->
    X * 1.40625 / 256.0.

%% @doc Convert a float to a 16-bit binary angle.
-spec float_to_ba16(float()) -> ba16().
float_to_ba16(X) ->
    Val = round(X * (64.0 / 45.0) * 128),
    <<Val:16/integer-unsigned-big>>.

%% @doc Convert a 32-bit binary angle to a float.
-spec ba32_to_float(ba32()) -> float().
ba32_to_float(<<X:32/integer-unsigned-big>>) ->
    X * 1.40625 / 16777216.0.

%% @doc Convert a float to a 32-bit binary angle.
-spec float_to_ba32(float()) -> ba32().
float_to_ba32(X) ->
    Val = round(X * (64.0 / 45.0) * 8388608),
    <<Val:32/integer-unsigned-big>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Signed binary angle conversion functions.
    
%% @doc Convert a 16-bit signed binary angle to a float.
-spec sa16_to_float(sa16()) -> float().
sa16_to_float(<<X:16/integer-signed-big>>) ->
    X * 1.40625 / 512.

%% @doc Convert a float to a 16-bit signed binary angle.
-spec float_to_sa16(float()) -> sa16().
float_to_sa16(X) ->
    Val = round(X * (64.0 / 45.0) * 256),
    <<Val:16/integer-signed-big>>.
    
%% @doc Convert a 32-bit signed binary angle to a float.
-spec sa32_to_float(sa32()) -> float().
sa32_to_float(<<X:32/integer-signed-big>>) ->
    X * 1.40625 / 33554432.

%% @doc Convert a float to a 32-bit signed binary angle.
-spec float_to_sa32(float()) -> sa32().
float_to_sa32(X) ->
    Val = round(X * (64.0 / 45.0) * 16777216),
    <<Val:32/integer-signed-big>>.

