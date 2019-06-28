#!/bin/bash

# Script to archive files after a run. 
# 
# Argument: archive directory name (can be non-existent)
#
# Example usage: ./archiveRun.sh c48_1hr_emissionsOff
#
# The output data (OutputDir/*.nc4) is moved but everything else is copied, including 
# log files (*.log, slurm-*), config files (*.rc, input.geos), run files (*.run, 
# *.env, runConfig.sh), and restarts (only gcchem*). Files are stored in 
# subdirectories within the archive directory.
#
# Clean the run directory after archiving with 'make cleanup_output' prior to
# rerunning and archiving a new set of run outputs. Otherwise previous run files
# will be copied to new run archives.

# Initial version: Lizzie Lundgren - 7/12/2018

if [[ $# == 1 ]]; then
    archivedir=$1
else
   echo "Usage: ./archiveRun.sh {ArchiveDirName}"
   exit 
fi

if [ -d "${archivedir}" ]; then
   echo "Warning: Directory ${archivedir} already exists."
   echo "Remove or rename that directory, or choose a different name."
   exit 1
else
   mkdir -p ${archivedir}
   mkdir -p ${archivedir}/output
   mkdir -p ${archivedir}/plots
   mkdir -p ${archivedir}/build
   mkdir -p ${archivedir}/logs
   mkdir -p ${archivedir}/run
   mkdir -p ${archivedir}/config
   mkdir -p ${archivedir}/restarts
fi

echo "Archiving files..."

# Move diagnostic output
for f in OutputDir/*.nc4; do
   if [ -f $f ]; then
      mv -v $f ${archivedir}/output
   else      
      echo "Warning: OutputDir is empty"
   fi
done

# Move plots
mv -v Plots/* ${archivedir}/plots

# Function to copy arg1 all files matching arg2
copyfiles () {
   for file in $2; do
      if [ -e $file ]; then
         echo "-> $1/$file"
         cp -tv $1 $file
      else
         echo "Warning: $file not found"
      fi
   done
}

# Customize as needed to best fit your workflow

# compilation logs
copyfiles ${archivedir}/build lastbuild
copyfiles ${archivedir}/build compile.log

# config files
copyfiles ${archivedir}/config input.geos
copyfiles ${archivedir}/config "*.rc"

# restarts (add/remove as needed - beware this is copying so may take a while if high res!)
copyfiles ${archivedir}/restarts "gcchem_*"

# run logs
copyfiles ${archivedir}/logs "*.log"
copyfiles ${archivedir}/logs "slurm-*"

# miscellaneous run files
copyfiles ${archivedir}/run runConfig.sh
copyfiles ${archivedir}/run "*.run"
copyfiles ${archivedir}/run "*.env"
copyfiles ${archivedir}/run *.multirun.sh

printf "Complete!\n"

exit 0
