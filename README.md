# s4607
Stanag 4607 library written in Erlang. Stanag 4607 is a NATO standard for sharing radar information. Erlang is a wonderful programming language for developing distributed systems, providing a good platform on which to develop software to connect and control sensors of various kinds. 

This software is still under development, but can now decode the packet header, dwell, mission and job definition segments. Further improvements including better test coverage will follow in due course. 

The software has been released under an Apache free software license.
# Building
It is necessary to have Erlang installed, and the compiler erlc available on the path. The software can be built (on a Linux platform) by moving to the src directory and running make:
```
# cd src
# make
```
# Decoding a Stanag 4607 file
From the root directory, the Erlang shell can be started as follows:
```
# erl -pa ebin
```
From the Erlang prompt, open a file in Stanag 4607 format and display its contents in the following manner:
```
1> Bin = s4607:read_file("/path/to/file").
2> s4607:display_packets(Bin).
```
In the not too distant future, it will be possible to read the entire list of packets into a single, hierarchical structure suitable for data processing applications.

# Running the regression tests
The project uses Erlang's eunit test system. From the Erlang shell, to run all of the unit tests:
```
3> eunit:test(all_tests).
```
