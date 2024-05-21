#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make readpb_config.x which extracts data
#  from PREPBUFR input files, and place the data into ASCII text files.
#  ------------------------------------------------------------------------

set -eua
 
SRC=../src
EXE=../exe

# create /exe directory if it doesn't exist yet
mkdir -p $EXE

# LIB = Path to BUFRLIB.  Library and documentation available at 
#    https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php
# 
# Replace LIB path with the path to BUFRLIB on your local system
LIB=/path/to/bufrlib/lib/directory

# Path to fortran compiler (Intel)
fflag=""
FC=/glade/apps/opt/cmpwrappers/ifort

#  Compile the decode program
#  ---------------------------------------

echo "Compiling readpb_config ..." 
$FC $fflag -c $SRC/readpb_config.f

#  link and load the executable
#  -----------------------------

echo "Linking ..."
$FC $fflag -o $EXE/readpb_config.x readpb_config.o $LIB/libbufr.a

#  clean up
#  --------
rm -f *.o

echo "Finished."
