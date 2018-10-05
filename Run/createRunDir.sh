#!/bin/bash

# createRunDir.sh: Create GCHP run directory
#
# Optional argument: run directory name
#
# Usage: ./createRunDir.sh [name]
#
# Initial version: E. Lundgren,10/5/2018

curdir=$(pwd)
cd ${curdir}/Run

#------------------------
# Prompt user for inputs
#------------------------

# ExtData path
default=/n/holylfs/EXTERNAL_REPOS/GEOS-CHEM/gcgrid/data/ExtData
printf "\nEnter path for ExtData, or press return to accept default:\n"
printf "(Default = ${default})\n"
read extdata
if [[ -z ${extdata} ]]; then
    extdata=$default
fi
#if [[ ! -d ${extdata} ]]; then
#    printf "\nError: ${extdata} does not exist! Exiting.\n"
#    exit 1
#fi

# Simulation type
printf "Choose simulation type:\n"
printf "  1. RnPbBe\n"
printf "  2. Standard\n"
printf "  3. Benchmark\n"
valid_sim=0
while [ "$valid_sim" -eq 0 ]
do
    read sim_num
    if [[ ${sim_num} = "1" ]]; then
	sim_name=RnPbBe
	sim_name_long=RnPbBePasv
	sim_type=${sim_name}
	valid_sim=1
    elif [[ ${sim_num} = "2" ]]; then
	sim_name=standard
	sim_name_long=${sim_name}
	sim_type=fullchem
	valid_sim=1
    elif [[ ${sim_num} = "3" ]]; then
	sim_name=benchmark
	sim_name_long=${sim_name}
	sim_type=fullchem
	valid_sim=1
    else
	printf "Invalid simulation option. Try again.\n"
    fi
done

# Run directory path
printf "\nEnter run directory path (excluding run directory name):\n"
read rundir_path
if [[ ! -d ${rundir_path} ]]; then
    printf "\nError: ${rundir_path} does not exist! Exiting.\n"
    exit 1
fi
 
#-----------------------------------
# Create and populate run directory
#-----------------------------------

# Set run directory path, either gchp_{simulation} or optionally passed name
if [ ! -z "$1" ]; then
    rundir_name=$1
else
    rundir_name=gchp_${sim_name}
fi
rundir=${rundir_path}/${rundir_name}

# Make run directory, prompting for a new name if it already exists
valid_rundir=0
while [ "${valid_rundir}" -eq 0 ]
do
    if [[ -d ${rundir} ]]; then
	printf "Warning! ${rundir_name} already exists at that path.\n"
        printf "Enter a different run directory name, or q to quit:\n"
	read new_rundir
	if [[ ${new_rundir} = "q" ]]; then
	    printf "Exiting.\n"
	    exit 1
	else
	    rundir=${rundir_path}/${new_rundir}
	fi
    else
        valid_rundir=1
    fi
done
mkdir -p ${rundir}

# Copy generic files for all GCHP run directories
cp ./archiveRun.sh         ${rundir} 
cp ./build.sh              ${rundir} 
cp ./fvcore_layout.rc      ${rundir} 
cp ./input.nml             ${rundir} 
cp ./README                ${rundir}
cp ./setCodeDir            ${rundir}
cp ./Makefile              ${rundir}
cp ./gitignore             ${rundir}/.gitignore
cp ./GCHP.rc_template      ${rundir}/GCHP.rc
cp ./runConfig.sh_template ${rundir}/runConfig.sh

# Copy generic subdirectories for all GCHP run directories
cp -r ./bashrcSamples    ${rundir} 
cp -r ./OutputDir        ${rundir} 
cp -r ./runScriptSamples ${rundir}

# Copy simulation-dependent files
cp ./HISTORY.rc_templates/HISTORY.rc.${sim_name}         ${rundir}/HISTORY.rc
cp ./input.geos_templates/input.geos.${sim_name}         ${rundir}/input.geos
cp ./CAP.rc_templates/CAP.rc.${sim_type}                 ${rundir}/CAP.rc
cp ./ExtData.rc_templates/ExtData.rc.${sim_type}         ${rundir}/ExtData.rc
cp ./HEMCO_Config.rc_templates/HEMCO_Config.rc.${sim_type}  ${rundir}/HEMCO_Config.rc
cp ./HEMCO_Diagn.rc_templates/HEMCO_Diagn.rc.${sim_type} ${rundir}/HEMCO_Diagn.rc

## Replace token strings in certain files
sed -i -e "s|{SIMULATION}|${sim_name_long}|" ${rundir}/GCHP.rc
sed -i -e "s|{SIMULATION}|${sim_name_long}|" ${rundir}/runConfig.sh
sed -i -e "s|{DATA_ROOT}|${extdata}|"        ${rundir}/input.geos

# Create symbolic links to data directories
ln -s ${extdata}/CHEM_INPUTS               ${rundir}/ChemDataDir
ln -s ${extdata}/HEMCO                     ${rundir}/MainDataDir
ln -s ${extdata}/GEOS_0.25x0.3125/GEOS_FP  ${rundir}/MetDir
ln -s ${extdata}/GCHP/TileFiles            ${rundir}/TileFiles

# Create symbolic links to restart files
for N in 24 48 90 180 360
do
    ln -s ${restarts}/initial_GEOSChem_rst.c${N}_${sim_name_long}.nc  ${rundir}
done

# Set permissions
chmod 744 ${rundir}/setCodeDir
chmod 744 ${rundir}/build.sh
chmod 744 ${rundir}/Makefile
chmod 744 ${rundir}/runConfig.sh
chmod 744 ${rundir}/archiveRun.sh
chmod 744 ${rundir}/runScriptSamples/*
chmod 744 ${rundir}/bashrcSamples/*

#-----------------------------------
# Save version information
#-----------------------------------

version_log=${rundir}/rundir.version
echo "This run directory was created by createRunDir.sh from the GCHP repository on $(date)." > ${version_log}
echo " " >> ${version_log}
echo "Version information (see also github.com/geoschem/gchp):" >> ${version_log}
echo $(git log -n 1) >> ${version_log}

#---------------------------------------------------
# Put run directory under version control (optional)
#---------------------------------------------------

printf "\nDo you want to track run directory changes with git? (y/n)\n"
read enable_git
valid_response=0
while [ "$valid_response" -eq 0 ]
do
    if [[ ${enable_git} = "y" ]]; then
	cd ${rundir}
	echo "Changes to this run directory are tracked by git:\n" >> ${version_log}
	git init
	git add *.rc *.sh bashrcSamples/* runScriptSamples/* README .gitignore
	git add setCodeDir Makefile input.geos input.nml
	echo " " >> ${version_log}
	git commit -m "Initial run directory" >> ${version_log}
	cd ${curdir}
	valid_response=1
    elif [[ ${enable_git} = "n" ]]; then
	valid_response=1
    else
	printf "Input not recognized. Try again.\n"
    fi
done

#-------
# Done!
#-------
printf "\nCreated ${rundir}\n"
