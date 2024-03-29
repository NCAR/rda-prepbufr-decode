# rda-prepbufr-decode
This project contains Fortran source code to read NCEP PrepBUFR files and dump 
output in ASCII format.  The executable produced by this code accepts a configuration
file as a command line argument, which allows users to extract subsets from the
PREPBUFR files based on subsetting parameters defined in the configuration file.

For more information, see the RDA PrepBUFR dataset archive at 
https://rda.ucar.edu/datasets/ds337.0.

The Github repository for this source code is available at 
https://github.com/NCAR/rda-prepbufr-decode.

Background on PREPBUFR processing and files:
==================================================================================

The "PREPBUFR" processing is the final step in preparing 
the majority of observational data for assimilation into 
the various NCEP analyses.  This step involves the
execution of series of programs designed to assemble 
observations collected from multiple sources, 
encode information about the observational error 
for each data type as well the background (first guess) 
interpolated to each data location, perform both rudimentary 
multi-platform quality control and more complex
platform-specific quality control, and store the output in a 
monolithic BUFR file, known as PREPBUFR. The background guess 
information is used by certain quality control programs while 
the observation error is used by the analysis to weigh the observations.
The structure of the BUFR file is such that each PREPBUFR processing 
step which changes a datum (either the observation itself, 
or its quality marker) records the change as an "event" with a
program code and a reason code. Each time an event is stored, 
the previous events for the datum are "pushed down" in the stack. 
In this way, the PREPBUFR file contains a complete history 
of changes to the data throughout all of the PREPBUFR processing.
The most recent changes are always at the top of the stack and 
are thus read first by any subsequent data decoder routine. It is 
expected that the data at the top of the stack are of 
the highest quality. 
 
 
Description and instructions to use the included extraction program:
=================================================================================

The included program can be used to extract general 
atmospheric variables from PREPBUFR files into simple text 
files.  The PREPBUFR files are archived in the RDA dataset
ds337.0.

To compile the PREPBUFR libraries and extraction code, go to the 
install directory. Execute the install.sh script to complete the 
compilation.

The executable will be placed in the exe directory.  
(exe/readpb_config.x:  program used to extract data from PREPBUFR files)
The command syntax to run the executable is as follows:

<pre>
<code>
readpb_config.x prepbufr.in prepbufr.out config_file
</code>
</pre>

where 'prepbufr.in' is the input PREPBUFR file, 'prepbufr.out' is 
the output ASCII file, and config_file is the configuration file 
passed into readpb_config.x.  An example template for the configuration
file is located in the 'config' directory.

Definitions for output file MNEMONICS:
================================================================================
<pre>
ADPUPA:  Upper-Air (RAOB, PIBAL, RECCO, DROPS) Reports.
AIRCAR:  ACARS Aircraft Reports.
AIRCFT:  Conventional (AIREP, PIREP) and ASDAR Aircraft Reports.
SATWND:  Satellite-derived Wind Reports.
PROFLR:  Wind Profiler Reports.
VADWND:  VAD (NEXRAD) Wind Reports.
SATBOG:  Satellite Moisture Bogus Reports.
SATEMP:  TOVS Satellite Data (Soundings, Retrievals, Radiances).
ADPSFC:  Surface Land (Synoptic, Metar) Reports.
SFCSHP:  Surface Marine (Ship, Buoy, C-man, Platform) Reports.
SFCBOG:  Mean Sea-Level Pressure Bogus Reports.
SPSSMI:  SSM/I Retrieval Products (Reprocessed Wind Speed, TPW).
SYNDAT:  Synthetic Tropical Cyclone Bogus Reports.
ERS1DA:  ERS Scatterometer Data (Reprocessed Wind Speed).
GOESND:  Quikscat Scatterometer Data (Reprocessed Wind Speed).
</pre>

References:
================================================================================

A description of PREPBUFR processing at NCEP can be found at:
http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/document.htm.

Definitions for PREPBUFR MNEMONIC headers and some code tables can be found at:
http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_1.htm.

Definitions for general BUFR MNEMONIC headers can be found at:
https://www.emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib/tables/bufrtab_tableb.html.

The BUFRLIB software and user guide is available at:
https://github.com/NOAA-EMC/NCEPLIBS-bufr.


Output file header definitions:  [unit]
================================================================================
<pre>
SID = Station ID
XOB = Lon [DegE]
YOB = Lat [DegN]
DHR = Obs time - Cycle time  [Hours]
ELV = Station Elevation [m]
TYP = Report Type [code table]
T29 = Input Report Type [code table]
ITP = Insturment Type [code table]
lev = Observation level
var = Observation variable
OB  = Observation value
QM  = quality marker [code table]
PC  = program code [code table]
RC  = reason code [code table]
FC  = forecast value
AN  = analyzed value
OE  = observation error
CAT = PREPBUFR level category [code table]

Variables for OB, QM, PC, RC, FC, AN, and OE:

P = Pressure [mb]
Q = Specific Humidity [MG/KG]
T = Temp [DEG C]
Z = Height [m]
U = U-wind component [m/s]
V = V-wind component [m/s]
</pre>

Code Definitions:
================================================================================

-Quality marker (QM) ranks the quality of the observation value. 
 See docs/Quality_mark.txt, or 
 http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_7.htm

-Program code (PC) indicates program used for QC processing.
 See docs/Program_code.txt

-Reason code (RC) is generated by the program used for QC processing.
 See the files located under docs/Reason_codes for Reason codes
 associated with each program code.  There are currently no
 reason codes associated with program codes 003, 011, 013, 014.

-Level catagory (cat) identifies the report level.
 See docs/LevelCat_code.txt

-Report Type (TYP) identifies reporting platforms and specfic variables 
 derived from those reporting platforms. See docs/Report_type.txt

-Input Report Type (T29) identifies reporting platforms.
 See docs/Input_Report_type.txt

-Instrument Type (ITP) identifies the instrument and the instrument's 
 origin of manufacture.  See docs/Instrument_type.txt

Quality Control Events Stack:
================================================================================

-The first value of a variable in the column is the one used for data 
 assimilation processing (QC'd obs value), if the variable is not rejected.

-The following values of the same variable (e.g. the second Q value to 
 show up in the column) have one less layer of QC processing.

-The final variable value in the column is the original observation. 

-There may be several steps of QC, and several corresponding values.

-A quality marker(qm) value > 3 indicates that the observation has been
 rejected for use in data assimilation processing. 

A note regarding virtual temperature processing:
================================================================================
Many temperature observations at the top of the event stack in the NCEP 
PrepBufr data files have gone through the VIRTMP PREPBUFR processing step at 
NCEP and have been converted from sensible temperature to virtual 
temperature.  This conversion is done at NCEP as part of the PREPBUFR data 
processing which prepares the data for assimilation into the GDAS 
assimilation system.  Further information on this processing step can be 
found in the NCEP documentation at 
http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_14.htm.

By default, the source code in this repository ignores all temperature 
observations in the PrepBUFR stack that have been converted to virtual 
temperature and extracts *only* sensible temperature values.  If you wish to
retain virtual temperature in the output, set the value of the "VTMP" 
parameter in the input configuration file to "TRUE".

Sample output generated by readpb.x for ADPSFC:
================================================================================
<pre>
#----------------------------------------------------------------------------------------------------------------------------------------------------
# SID        XOB    YOB      ELV     DHR      TYP     T29    ITP  lev   var       OB       QM       PC       RC       FC       AN       OE      CAT
#----------------------------------------------------------------------------------------------------------------------------------------------------
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    1     P   1000.0      8.0      4.0      1.0                                 1.0 
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    1     P   1000.0      2.0      1.0    100.0                                     
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    1     Z     69.0      2.0      1.0    100.0     88.0                        1.0  
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    2     P    925.0      2.0      1.0    100.0                                 1.0 
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    2     Z    734.0      2.0      1.0    100.0    745.0                        1.0  
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    3     P    850.0      2.0      1.0    100.0                                 1.0  
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    3     Z   1439.0      2.0      1.0    100.0   1447.0                        1.0  
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    4     P    832.0      2.0      1.0    100.0    833.2               1.0      0.0  
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    4     Q   2351.0      2.0      8.0      1.0   2154.0               2.0      0.0  
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    4     Q   2351.0      2.0      1.0    100.0                                      
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    4     T      7.2      2.0      1.0    100.0                                      
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    4     Z   1611.0      2.0      1.0    100.0   1623.0                        0.0  
72469     255.13  39.77   1611.0   0.000    120.0    11.0  111.0    4     U     -5.8      2.0      1.0    100.0     -2.9               1.5      0.0  
</pre>
