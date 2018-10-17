#!/bin/bash

#------------------------------------------------------------------------------
#                  GEOS-Chem Global Chemical Transport Model                  !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: build.sh
# 
# !DESCRIPTION: Cleans and/or compiles a GEOS-Chem High Performance (GCHP) 
#  source code. Accepts one keyword argument indicating combination of clean 
#  and compile settings. 
#\\
#\\
# !REMARKS:
#  (1) Implemented arguments options include:
#         help (lists options)
#         clean_gc
#         clean_nuclear
#         clean_all
#         clean_mapl
#         compile_debug
#         compile_standard
#         compile_mapl
#         compile_clean
#  (2) To add a new one simply add an 'elif' block in the clean section
#      and/or the compile section.
#
# !REVISION HISTORY: 
#  19 Oct 2016 - E. Lundgren - Initial version
#  Navigate to your unit tester directory and type 'gitk' at the prompt
#  to browse the revision history.
#EOP
#------------------------------------------------------------------------------
#BOC

# Check usage
if [[ $# == 0 ]]; then
  echo "ERROR: No argument passed to build.sh!"
  exit 1
fi

# Source your environment file. This requires first setting the gchp.env
# symbolic link using script setEnvironment in the run directory. 
# Be sure gchp.env points to the same file for both compilation and 
# running. You can copy or adapt sample environment files located in 
# ./envSamples subdirectory.
gchp_env=$(readlink -f gchp.env)
if [ ! -f ${gchp_env} ] 
then
   echo "ERROR: gchp.rc symbolic link is not set!"
   echo "Copy or adapt an environment file from the ./envSamples "
   echo "subdirectory prior to running. Then set the gchp.env "
   echo "symbolic link to point to it using ./setEnvironment."
   echo "Exiting."
   exit 1
fi
echo "WARNING: You are using environment settings in ${gchp_env}"
source ${gchp_env}

# Check environment
if [[ "x${MPI_ROOT}" == x ]]; then
   echo "Source environment settings before continuing. See environmentFileSamples subdir for examples."
fi

if [[ "x${ESMF_COMM}" == x ]]; then
   echo "ERROR: ESMF_COMM is not set! See environmentFileSamples subdir for examples."
   exit 1
elif [[ "x${ESMF_COMPILER}" == x ]]; then
   echo "ERROR: ESMF_COMPILER is not set! See environmentFileSamples subdir for examples."
   exit 1
elif [[ ! -e CodeDir ]]; then
  echo "ERROR: CodeDir symbolic link to source code directory does not exist!"
  echo "You may use the setCodeDir function for this, e.g."
  echo "   ./setCodeDir /path/to/your/code"
  exit 1
fi

# Set run directory
rundir=$PWD          

# Go to the source code directory
cd ${rundir}/CodeDir 

###############################
###       Clean             ###
###############################

# help
if [[ $1 == "help" ]]; then
  echo "Script name:"
  echo "   build.sh"
  echo "Arguments:"
  echo "   Accepts single argument indicating clean and/or compile settings."
  echo "   Currently implemented arguments include:"
  echo "      clean_gc         - classic only"
  echo "      clean_nuclear    - GCHP, ESMF, MAPL, FVdycore (be careful!)"
  echo "      clean_all        - classic, GCHP, ESMF, MAPL, FVdycore (be careful!)"
  echo "      clean_mapl       - mapl and fvdycore only"
  echo "      compile_debug    - turns on debug flags, no cleaning"
  echo "      compile_standard - no cleaning"
  echo "      compile_mapl     - includes fvdycore"
  echo "      compile_clean    - cleans and compiles everything (be careful!)"
  echo "Example usage:"
  echo "   ./build.sh compile_standard"
  exit 0
fi

# Clean GEOS-Chem main code (non-GCHP repo)
if [[ $1 == "clean_gc"      ]] || \
   [[ $1 == "clean_all"     ]] || \
   [[ $1 == "compile_clean" ]]; then
   make HPC=yes realclean
elif [[ $1 == "clean_mapl" ]] || \
     [[ $1 == "compile_mapl" ]]; then
   make realclean
fi

# Change directory to GCHP
cd GCHP

# Clean GCHP
if [[ $1 == "compile_debug"    ]] || \
   [[ $1 == "compile_standard" ]]; then
    make clean
elif [[ $1 == "clean_all" ]]; then
    make the_nuclear_option
elif [[ $1 == "clean_nuclear" ]] || \
     [[ $1 == "compile_clean"   ]]; then
    make EXTERNAL_GRID=y the_nuclear_option
elif [[ $1 == "clean_mapl" ]] || \
     [[ $1 == "compile_mapl" ]]; then
    make EXTERNAL_GRID=y  DEBUG=y   GRID=4x5      MET=geosfp      \
         NO_REDUCED=y     wipeout_fvdycore
    make EXTERNAL_GRID=y  DEBUG=y   GRID=4x5      MET=geosfp      \
         NO_REDUCED=y     wipeout_mapl
else
  echo "Argument passed to build.sh is not defined"
  exit 1
fi

# Change directory back to highest level
cd ..

# Remove executable, if it exists
rm -f ${rundir}/CodeDir/bin/geos

# Compile
if [[ $1 == "compile_debug" ]]; then
    make -j${SLURM_NTASKS} NC_DIAG=y   CHEM=standard  EXTERNAL_GRID=y  \
                           DEBUG=y     TRACEBACK=y    MET=geosfp       \
                           GRID=4x5    NO_REDUCED=y   BOUNDS=y         \
                           FPEX=y      hpc
elif [[ $1 == "compile_standard" ]] || \
     [[ $1 == "compile_mapl"     ]] || \
     [[ $1 == "compile_clean"    ]]; then
    make -j${SLURM_NTASKS} NC_DIAG=y   CHEM=standard EXTERNAL_GRID=y   \
                           DEBUG=n     TRACEBACK=y   MET=geosfp        \
                           GRID=4x5    NO_REDUCED=y  hpc
fi

# Change back to the run directory
cd ${rundir}

# Cleanup and quit
if [[ -e ${rundir}/CodeDir/bin/geos ]]; then
   echo '###################################'
   echo '### GCHP compiled successfully! ###'
   echo '###################################'
   cp CodeDir/bin/geos .
else
   echo '###################################################'
   echo '### WARNING: GCHP executable does not yet exist ###'
   echo '###################################################'
fi

unset rundir

exit 0
