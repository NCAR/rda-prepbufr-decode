# rda-prepbufr-decode
Fortran source code to read PrepBUFR files and dump output in ASCII format.  For more information, see the RDA PrepBUFR dataset archive at http://rda.ucar.edu/datasets/ds337.0.

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
 
 
Prompt commands to get desired tar file (YYMM.prepqmDDDD) off of the Mass Store
System and into a usable format:
=================================================================================

msread -fBI localfilename /DSS/AXXXXX
tar -xvf localfilename

or 

msread -ftr localfilename /DSS/AXXXXX
cosconvert -b localfilename
tar -xvf localfilename


Description and instructions to use the included extraction program:
=================================================================================

The included program can be used to extract general 
atmospheric variables from PREPBUFR files into simple text 
files.  The PREPBUFR files (prepqm*) are found within the NCEP reanalysis 
dataset, ds090.0.

** DATATSET files containing obs prior to March 1, 1997 need to be blocked
   using the cwordsh NCEP utility located in the blk_ublk
   directory.  The README file within that directory 
   describes the process. ******

** Files to be decoded on little-endian linux machines must first
   be processed through the "grabbufr/grabbufr.sh" script, with
   the resulting output used as input for the decoders. **

To compile the PREPBUFR libraries and extraction code, go to the 
install directory.  Set the CPLAT variable in the install.sh 
script to reflect the correct platform.  Currently CPLAT=sun.
Execute the install.sh script to complete the compilations.

The executable will be placed in the exe directory.  
(exe/readpb.x:  program used to extract data from PREPBUFR files)
Execute the executable and enter the PREPBUFR input file name to extract
the basic meteorlogical varibles into text format.  Output text
files corresponding to respective MNEMONICS will be generated.

Definitions for output file MNEMONICS:
================================================================================

ADPUPA:  Upper-Air (RAOB, PIBAL, RECCO, DROPS) Reports. 
AIRCAR:  ACARS Aircraft Reports.
AIRCFT:  Conventional (AIREP, PIREP) and ASDAR Aircraft Reports.
SATWND:  Satellite-derived Wind Reports.
PROFLR:  Wind Profiler Reports.
VADWND:  VAD (NEXRAD) Wind Reports.
SATBOG:  Satellite Moisture Bogus Reports
SATEMP:  TOVS Satellite Data (Soundings, Retrievals, Radiances).
ADPSFC:  Surface Land (Synoptic, Metar) Reports.
SFCSHP:  Surface Marine (Ship, Buoy, C-man, Platform) Reports.
SFCBOG:  Mean Sea-Level Pressure Bogus Reports.
SPSSMI:  SSM/I Retrieval Products (Reprocessed Wind Speed, TPW).
SYNDAT:  Synthetic Tropical Cyclone Bogus Reports.
ERS1DA:  ERS Scatterometer Data (Reprocessed Wind Speed).
GOESND:  Quikscat Scatterometer Data (Reprocessed Wind Speed).

References:
================================================================================

A guide to the PREPBUFR libraries can be found at:
http://www.ncep.noaa.gov/NCO/DMQAB/Decoders/BUFRLIB/

Definitions for PREPBUFR MNEMONIC headers and some code tables can be found at:
http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_1.htm
and
http://www.ncep.noaa.gov/NCO/DMQAB/Decoders/BUFRLIB/prepbufr_bftab.html

A description of PREPBUFR processing at NCEP can be found at:
http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/document.htm

Definitions for general BUFR MNEMONIC headers can be found at:
http://www.emc.ncep.noaa.gov/mmb/data_processing/bufrtab_tableb.htm


Output file header definitions:  [unit]
================================================================================
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
qm  = quality marker [code table]
pc  = program code [code table]
rc  = reason code [code table]
fc  = forecast value
an  = analyzed value
cat = level category [code table]

Variables for OB, fc, and an:

P = Pressure [mb]
Q = Specific Humidity [MG/KG]
T = Temp [DEG C]
Z = Height [m] 
U = U-wind component [m/s]
V = V-wind component [m/s] 

Code Definitions:
================================================================================

-Quality marker (qm) ranks the quality of the observation value. 
 See docs/Quality_mark.txt 
-Program code (pc) indicates program used for QC processing.
 See docs/Program_code.txt
-Reason code (rc) is generated by the program used for QC processing.
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

-The first value of a variable in the column is the one used for reanalysis
 processing (QC'd obs value), if the variable is not rejected.
-The following values of the same variable (e.g. the second Q value to 
 show up in the column) have one less layer of QC processing.
-The final variable value in the column is the original observation. 
-There may be several steps of QC, and several corresponding values.
-A quality marker(qm) value > 3 indicates that the observation has been
 rejected for use in reanalysis processing. 


Sample output generated by readpb.x for ADPSFC:
================================================================================

#------------------------------------------------------------------------------------------------------------------------------------
#SID         XOB    YOB     DHR      ELV     TYP     T29    ITP lev var      OB       qm       pc       rc       fc       an      cat
#------------------------------------------------------------------------------------------------------------------------------------
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  P   1014.0     15.0      1.0            1013.0   1013.9      6.0                 
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  Q  17414.0      9.0      8.0      0.0  16603.0  16635.0      6.0                 
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  Q  17414.0      9.0      4.0      3.0                                            
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  Q  17414.0      2.0      1.0                                                     
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  T     30.7      8.0      8.0      2.0     29.4     28.9      6.0                 
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  T     27.5      9.0      4.0      3.0                                            
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  T     27.5      2.0      1.0                                                     
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  Z      7.0      2.0      1.0              -1.0      6.0      6.0                 
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  U     -8.1      9.0      4.0      3.0    -11.4    -11.0      6.0                 
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  U     -8.1     15.0      1.0                                                     
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  V     -1.4      9.0      4.0      3.0     -3.3     -2.7      6.0                 
78958     298.22  12.00  -3.000      7.0   281.0   511.0   19.0   1  V     -1.4     15.0      1.0                                                     
#------------------------------------------------------------------------------------------------------------------------------------
#SID         XOB    YOB     DHR      ELV     TYP     T29    ITP lev var      OB       qm       pc       rc       fc       an      cat
#------------------------------------------------------------------------------------------------------------------------------------
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  P    999.4     15.0      1.0            1001.1   1003.8      6.0                 
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  Q   6088.0      9.0      8.0      0.0   8411.0   8234.0      6.0                 
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  Q   6088.0      9.0      4.0      3.0                                            
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  Q   6088.0      2.0      1.0                                                     
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  T     12.4      8.0      8.0      2.0     15.2     14.2      6.0                 
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  T     11.3      9.0      4.0      3.0                                            
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  T     11.3      2.0      1.0                                                     
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  Z     15.0      2.0      1.0              29.0     52.0      6.0                 
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  U      0.0      9.0      4.0      3.0     -7.9     -7.3      6.0                 
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  U      0.0     15.0      1.0                                                     
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  V     -0.5      9.0      4.0      3.0    -10.0    -10.0      6.0                 
16362      16.25  38.90  -3.000     15.0   281.0   511.0   19.0   1  V     -0.5     15.0      1.0                                     
