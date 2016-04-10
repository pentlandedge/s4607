# s4607
Stanag 4607 library written in Erlang. Stanag 4607 is a NATO standard for sharing radar information. Erlang is a wonderful programming language for developing distributed systems, providing a good platform on which to develop software to connect and control sensors of various kinds.

This software is still under development, but can now decode and encode the packet header, dwell, mission, job definition, free text and platform location segments. Work on the high-range resolution (HRR) segment is now underway.

The software has been released under an Apache free software license.
## Building
It is necessary to have Erlang installed, and the compiler erlc available on the path. The rebar tool is used to control the build process, so it is also necessary to have a copy of rebar available on the path. The software can be built (on a Linux platform) using rebar:
```
# rebar compile
```
## Decoding a Stanag 4607 file
From the root directory, the Erlang shell can be started as follows:
```
# erl -pa ebin
```
From the Erlang prompt, open a file in Stanag 4607 format and display its contents in the following manner:
```
1> Bin = s4607:read_file("/path/to/file").
2> PacketList = s4607:decode(Bin).
3> s4607:display_packets(PacketList).
```
The PacketList is a single, hierarchical structure suitable for use in data processing applications.

## Running the escript
For convenience, the display_4607 script has been provided. The software must have been built before running this. Make the script executable:
```
# chmod a+x display_4607
```
The script can then be run, and the results written to a file as follows:
```
# ./display_4607 /path/to/file > 4607.log
```

## Running the regression tests
The project uses Erlang's eunit test system, controlled from rebar.
```
# rebar compile eunit
```
This will rebuild the project including the eunit tests and then execute all of the tests.
## Generating a code coverage report
To generate a code coverage report, first open the rebar.config file and enable code coverage:
```
{cover_enabled, true}.
```
Then recompile the project and run the unit tests:
```
# rebar clean
# rebar compile eunit
```
Once this completes, the code coverage report should be available in at .eunit/index.html
## Encoding a segment inside a packet structure.
As an example, consider creating a mission segment, encapsulating it inside a packet, then encoding into Stanag 4607 binary form. The first step is to create the mission segment payload.
```
5> MS = mission:new("Drifter 1", "A1234", other, "Build 1", 2016, 2, 5).
```
The parameters supplied to mission:new() correspond to the segment parameters. The next step is to create the full segment containing the segment header and payload:

```
6> Seg = segment:new(mission, MS).
```
The segment:new() function automatically creates the segment header from the segment type specifier and from the length of the mission segment payload.
To create a full Stanag 4607 packet, it is necessary to add a packet header. The header contains a number of fields. Most of these are constant over a job, but the size field may change with each new packet. For ease of use a packet generator function has been provided which returns a function that captures (using a closure) the list of constant header parameters. Each time this returned function is called, the size field is updated with the length of the new list of segments. For example, the variable PL below is a property list containing the header parameters to use, and the variable Gen is bound to a function taking a list of segments as parameters:
```
7> PL = [{version, {3, 1}}, {nationality, "UK"},
         {classification, unclassified}, {class_system, "UK"},
         {packet_code, none}, {exercise_ind, exercise_real},
         {platform_id, "Plat1"}, {mission_id, 16#11223344},
         {job_id, 16#55667788}].

8> Gen = s4607:packet_generator(PL).
```
We can then pass in our list of segments (just one in this case) to create a complete packet structure:
```
9> Packet = Gen([Seg]).
```
The function Gen can be called repeatedly with new lists of segments, and the size field will be recalculated. To convert this packet structure to binary encoded form, do the following:
```
10> EncodedPacket = s4607:encode_packet(Packet).
```
This encoded packet can then be written to file using the normal IO libraries.

## Extracting segments from a list of packets.
A flattened list of segments can be extracted from a list of packets in the following manner:
```
11> SegList = s4607:get_segments(PacketList).
```
It is also possible to extract a filtered list of segments of specified types:
```
12> FiltSegList = s4607:get_segments_by_type([dwell, mission], PacketList).
```

