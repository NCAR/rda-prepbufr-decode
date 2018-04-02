#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make readpb_config.x which extracts data
#  from PREPBUFR input files, and place the data into ASCII text files.
#  ------------------------------------------------------------------------

set -eua
 
SRC=../src
EXE=../exe

# LIB = Path to BUFRLIB.  Library available at 
#      http://www.nco.ncep.noaa.gov/sib/decoders/BUFRLIB/

LIB=/glade/apps/opt/BUFRLIB/11.0.0/intel/12.1.5/lib

# Path to fortran compiler
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
