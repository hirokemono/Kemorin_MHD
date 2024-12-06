PassMark (TM) Software's PerformanceTest for Linux/Mac
Copyright (C) 2023 PassMark Software
All Rights Reserved
https://www.passmark.com

Overview 
========
Passmark's PerformanceTest for Linux/Mac is a software tool that allows everybody to quickly assess the performance of their computer and compare it to a number of standard 'baseline' computer systems.

Installation
============
Unzip the executable from the downloaded zip file using "unzip pt_linux_<arch>.zip"

How to launch application
==========================
To run application via the command line, from your terminal:
	1. Change the current working directory to where you have unzipped PerformanceTest
	2. Launch PerformanceTest using the command ./pt_linux_<arch>

System Requirements
======================
Linux:

x86 Version: Supports 64-bit x86 CPU's
ARM 32-bit: Supports ARMv7 and ARMv8 CPU's in AArch32 mode
ARM 64-bit: Supports ARMv8 CPU's in AArch64 mode

The following distros have been tested;
Ubuntu 16.04, 18.04, 20.04
CentOS 8
Raspberry Pi OS
Fedora 32

Linux distributions that meet the following reqirments should also work;
Linux Kernel 4.1 or higher
glibc 2.20 or higher
libstdc++ 6.0.20
ncurses 5, newer distributions with ncurses 6 will need to install the ncurses 5 library for compatability

Mac:

Mac App: macOS 11.0 or later
Command Line Tool: macOS 11.0 or later
CPU: Apple M1/M2 or x86_64 CPU

Support
=======
For technical support please visit the PassMark website.
https://www.passmark.com/support/

Version History
===============

11.0 Build 1001 - 19th October 2023
- Added autorun command line argument "-bg" for running in the background

11.0 Build 1001 - 31st August 2023
- Increased max thread count from 256 to 1024
- (Mac) - Included NEON FMA subtest for Apple M1/M2 machines

11.0 Build 1000 - 1st May 2023
- Updated version number to 11 to match current Windows release
- Added extra timeout lengths based on test thread count to CPU test (30 seconds for > 200, 60 seconds for > 500)

10.2 Build 1003 - 27th June 2022
- Remove (TM) and (R) from CPU name

10.2 Build 1002 - 17th May 2022
- Fixed baseline uploading that was broken due to passmark.com moving to a new webserver

10.2 Build 1001 - 2nd March 2022
- Add support for locales other than English

10.2 Build 1000 - 16th December 2021
- CPU Integer test, made some changes to the order of operations in the test. This should stop them being optimised away on non-windows builds
- CPU Test results, changed how results (Prime, Compression, Encryption and Sorting tests) are saved in baseline/results files so they are the same as the windows version of PerformanceTest and can be more easily used to manually calculate the CPU/Memory mark

10.1 Build 1005 - 20th October 2021
- Linux and Mac (command line tool): Added flag to enable debugging "-debug" </li>
- Mac (command line tool): Added ability to specify which test suite(s) to autorun "-r [1-3]" [CPU, Memory, All tests]</li>

10.1 Build 1004 - 27th August 2021
- Linux: Added ability to specify which test suite(s) to autorun "-r [1-3]" [CPU, Memory, All tests]

10.1 Build 1003 - 26th August 2021
- Linux: Added autorun command line argument "-r" for easier automation of testing process.

10.1 Build 1002 - 11th June 2021
- Fixed problem generating upload checksum on Red Hat Linux

10.1 Build 1001 - 3rd May 2021
- Added memory test suite for Linux and Mac
- Memory test scores are directly comparable with Windows Version

10.1 Build 1000 - 3rd March 2021
- First release of Linux and Mac version of PerformanceTest CPU test suite
