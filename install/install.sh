#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make readpb_config.x which extracts data
#  from PREPBUFR input files, and place the data into ASCII text files.
#  ** Make sure the "ar" command location has been set in your path
#  environment variable.  Type "which ar" to check if this is done. **
#  ------------------------------------------------------------------------
 
set -eua
 
#  ------------------------------------------------------------------------
#  CPLAT - platform type (linux,sgi,aix,sun)
#  ------------------------------------------------------------------------
 
CPLAT=linux
SRC=../src
LIB=/glade/apps/opt/BUFRLIB/11.0.0/intel/12.1.5/lib  # path to BUFRLIB
EXE=../exe

#  different platforms use different link name protocols
#  -----------------------------------------------------

cflag=""
fflag=""

if [ $CPLAT = linux ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_

   cflag="-DUNDERSCORE"
   fflag=""
   CC=/glade/apps/opt/cmpwrappers/icc    # activated 2014.02.27
   FC=/glade/apps/opt/cmpwrappers/ifort  # activated 2014.02.27

# uncomment following if ff=gfortran #
#   cflag="-DUNDERSCORE"
#   fflag="-DUNDERSCORE -fno-second-underscore"
#   CC=/glade/apps/opt/cmpwrappers/gcc
#   FC=/glade/apps/opt/cmpwrappers/gfortran  # activated 2014.02.27

elif [ $CPLAT = sgi ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   cflag=-DUNDERSCORE
   CC=cc; FC=f77
elif [ $CPLAT = aix ]
then
   openrb=openrb
   openwb=openwb
   crdbfr=crdbufr
   cwrbfr=cwrbufr
   lenmsg=lenm
   CC=cc; FC=f77
elif [ $CPLAT = sun ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   cflag=-DUNDERSCORE 
   CC=cc; FC=f77
fi

#  Compile the decode programs
#  ---------------------------------------

echo "Compiling readpb_config ..." 
$FC $fflag -c $SRC/readpb_config.f

#  link and load the executables
#  -----------------------------

echo "Linking ..."
$FC $fflag -o $EXE/readpb_config.x readpb_config.o $LIB/libbufr.a

#  clean up
#  --------
rm -f *.o

echo "Finished."
