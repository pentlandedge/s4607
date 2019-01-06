# s4607
Stanag 4607 library written in Erlang. Stanag 4607 is a NATO standard for sharing radar information. Erlang is a wonderful programming language for developing distributed systems, providing a good platform on which to develop software to connect and control sensors of various kinds.

NOTE: the library is currently being migrated from rebar to rebar3. Some of these instructions may not work as advertised while the transition is taking place (particularly those relating to running escript files which are known to be broken). If this is a problem, then please use the v1.0.0 release for the time being which is based entirely on rebar.

This software is still under development, but can now decode and encode the packet header, dwell, mission, HRR, job definition, free text, platform location, test and status, job request and job acknowledge segments. Once complete, the focus will shift to improving the top level API and documentation to make the library more convenient to use.

The metadata (platform types/radar modes etc.) is in the process of being updated to match the list available at http://jitc.fhu.disa.mil/projects/mti/registers/metadata.aspx

Errata sheet E3 has also recently appeared. It defines some new platforms, sensors and modes. These are also in the process of being incorporated.

The software has been released under an Apache free software license.

## Prerequisites
It is necessary to have Erlang installed, and the compiler erlc available on the path. The rebar3 tool is used to control the build process, so it is necessary to have this installed and on the path too. 

If using the (optional) Makefile, then the make utility must be available.

## Building

The simplest way to build the software, run the unit tests, perform static analysis and generate the module documentation in one step is to use make:
```
# make
```
The makefile has rules for each of these steps which can be run separately if preferred. It uses rebar3 to do the real work.

The software can be compiled (on a Linux platform) using rebar3:
```
# rebar3 compile
```
## Decoding a Stanag 4607 file
From the root directory, the Erlang shell can be started as follows:
```
# rebar3 shell 
```
From the Erlang prompt, open a file in Stanag 4607 format and display its contents in the following manner:
```
1> Bin = s4607:read_file("/path/to/file").
2> PacketList = s4607:decode(Bin).
3> s4607:display_packets(PacketList).
```
The PacketList is a single, hierarchical structure suitable for use in data processing applications.

## Running the escript

NOTE: this does not currently work with rebar3. Needs to be updated.

For convenience, the display_4607 script has been provided. The software must have been built before running this. Make the script executable:
```
# chmod a+x display_4607
```
The script can then be run, and the results written to a file as follows:
```
# ./display_4607 /path/to/file > 4607.log
```

## Running the regression tests
The project uses Erlang's eunit test system, controlled from rebar3.
```
# rebar3 eunit
```
This will rebuild the project including the eunit tests and then execute all of the tests.
## Generating a code coverage report
To generate a code coverage report, first open the rebar.config file and enable code coverage:
```
{cover_enabled, true}.
```
Then recompile the project and run the unit tests:
```
# rebar3 clean
# rebar3 compile 
# rebar3 eunit
# rebar3 cover
```
Once this completes, the code coverage report should be available at _build/test/cover/index.html 

## Running the static analysis tool.

Erlang provides the Dialyzer, which performs static type checking of the code. Rebar3 can be used to run the dialyzer on all of the source files in the repository. This should always be clean (no errors or warnings) for code in the repository. To run it, from the root directory type:
```
rebar3 dialyzer
```

## Generating the module documentation.
Erlang supplies the EDoc tool which can automatically generate cross-referenced documentation from a combination of the code itself and any annotations added by the programmer. We are in the process of adding more tags and type specifications to improve the generated output. So far, the modules stanag_types.erl and test_status.erl have been annotated and provide the most useful information. More modules will be documented soon. Rebar3 can be used to call EDoc to generate the HTML formatted documents. From the root directory type:
```
rebar3 edoc
```
Once complete a doc directory will be created with the generated HTML files for each module.

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

## Online application.
The [Hawkstream Stanag 4607 Viewer](http://www.hawkstream.net "Hawkstream Stanag 4607 Viewer Demo") uses this library to decode and display dwell segment data on a map. This will be steadily updated to display more of the 4607 information.
