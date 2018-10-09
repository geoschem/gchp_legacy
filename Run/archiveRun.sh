#!/bin/bash

# Script to archive files after a run. 
# 
# Argument: archive directory name (can be non-existent)
#
# Example usage: ./archiveRun.sh c48_1hr_emissionsOff
#
# The output data (OutputDir/*.nc4) is moved but everything else is copied, including 
# log files (*.log, slurm-*), config files (*.rc, input.geos), run files (*.run, 
# *.bashrc, runConfig.sh), and restarts (only gcchem*). Files are stored in 
# subdirectories within the archive directory.
#
# Clean the run directory after archiving with 'make cleanup_output' prior to
# rerunning and archiving a new set of run outputs. Otherwise previous run files
# will be copied to new run archives.

# Initial version: Lizzie Lundgren - 7/12/2018

archivedir=$1
if [ -d "${archivedir}" ]; then
   printf -v "Warning: Directory ${archivedir} already exists.\nRemove or rename that directory, or choose a different name."
   exit 1
else
   mkdir -p ${archivedir}
   mkdir -p ${archivedir}/data
   mkdir -p ${archivedir}/build
   mkdir -p ${archivedir}/logs
   mkdir -p ${archivedir}/run
   mkdir -p ${archivedir}/config
   mkdir -p ${archivedir}/restarts
fi

echo "Archiving files..."

# Move diagnostic data
for f in OutputDir/*.nc4; do
   if [ -f $f ]; then
      mv $f ${archivedir}/data
   else      
      echo "Warning: OutputDir is empty"
   fi
done

# Function to copy arg1 all files matching arg2
copyfiles () {
   for file in $2; do
      if [ -e $file ]; then
         echo "-> $1/$file"
         cp -t $1 $file
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
copyfiles ${archivedir}/run "*.bashrc"
copyfiles ${archivedir}/run *.multirun.sh

printf "Complete!\n"

exit 0
