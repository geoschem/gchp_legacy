#!/bin/bash

# createRunDir.sh: Create GCHP run directory
#
# Optional argument: run directory name
#
# If optional run directory name argument is not passed then the user
# will be prompted to enter a name interactively, or choose to use the
# default name gchp_{simulation}/
#
# Usage: ./createRunDir.sh [rundirname]
#
# Initial version: E. Lundgren,10/5/2018

curdir=$(pwd)
cd ..
gchpdir=$(pwd)
cd ..
gcdir=$(pwd)
cd ${curdir}

#-----------------------------------------------------------------
# Export data root path in ~/.geoschem/config if file exists
#-----------------------------------------------------------------
if [[ -f ${HOME}/.geoschem/config ]]; then
    source ${HOME}/.geoschem/config
    if [[ ! -d ${GC_DATA_ROOT} ]]; then
	printf "\nWarning: Default root data directory ${GC_DATA_ROOT} does not exist. Set new path below or edit in ${HOME}/.geoschem/config.\n"
    fi
else
    mkdir -p ${HOME}/.geoschem
fi

#-----------------------------------------------------------------
# One-time configuration of data root path in ~/.geoschem/config
#-----------------------------------------------------------------
if [[ -z "${GC_DATA_ROOT}" ]]; then
    printf "\nEnter path for ExtData. This will be saved to ${HOME}/.geoschem/config for future automatic use.\n"
    valid_path=0
    while [ "$valid_path" -eq 0 ]
    do
	read extdata
	if [[ ${extdata} = "q" ]]; then
	    printf "\nExiting.\n"
	    exit 1
	elif [[ ! -d ${extdata} ]]; then
            printf "\nError: ${extdata} does not exist. Enter a new path or hit q to quit.\n"
	else
	    valid_path=1
	    echo "export GC_DATA_ROOT=${extdata}" > ${HOME}/.geoschem/config
	fi
    done
fi

#-----------------------------------------------------------------
# Ask user to select simulation type
#-----------------------------------------------------------------
printf "\nChoose simulation type:\n"
printf "  1. RnPbBe\n"
printf "  2. Standard\n"
printf "  3. Benchmark\n"
valid_sim=0
while [ "${valid_sim}" -eq 0 ]
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

#-----------------------------------------------------------------
# Ask user to define path where directoy will be created
#-----------------------------------------------------------------
printf "\nEnter path where the run directory will be created:\n"
valid_path=0
while [ "$valid_path" -eq 0 ]
do
    read rundir_path
    if [[ ${rundir_path} = "q" ]]; then
	printf "\nExiting.\n"
	exit 1
    elif [[ ! -d ${rundir_path} ]]; then
        printf "\nError: ${rundir_path} does not exist. Enter a new path or hit q to quit.\n"
    else
	valid_path=1
    fi
done
 
#-----------------------------------------------------------------
# Ask user to define run directoy name if not passed as argument
#-----------------------------------------------------------------
if [ -z "$1" ]; then
    printf "\nEnter run directory name, or press return to use default:\n"
    read rundir_name
    if [[ -z "${rundir_name}" ]]; then
	rundir_name=gchp_${sim_name}
	printf "Using default directory name ${rundir_name}\n"
    fi
else
    rundir_name=$1
fi

#-----------------------------------------------------------------
# Ask user for a new run directory name if specified one exists
#-----------------------------------------------------------------
rundir=${rundir_path}/${rundir_name}
valid_rundir=0
while [ "${valid_rundir}" -eq 0 ]
do
    if [[ -d ${rundir} ]]; then
	printf "Warning! ${rundir} already exists.\n"
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

#-----------------------------------------------------------------
# Create run directory
#-----------------------------------------------------------------
mkdir -p ${rundir}

#-----------------------------------------------------------------
# Copy run directory files and subdirectories
#-----------------------------------------------------------------
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
cp ./CAP.rc_template       ${rundir}/CAP.rc
cp -r ./bashrcSamples      ${rundir} 
cp -r ./OutputDir          ${rundir} 
cp -r ./runScriptSamples   ${rundir}
cp ./HISTORY.rc_templates/HISTORY.rc.${sim_name}            ${rundir}/HISTORY.rc
cp ./input.geos_templates/input.geos.${sim_name}            ${rundir}/input.geos
cp ./ExtData.rc_templates/ExtData.rc.${sim_type}            ${rundir}/ExtData.rc
cp ./HEMCO_Config.rc_templates/HEMCO_Config.rc.${sim_type}  ${rundir}/HEMCO_Config.rc
cp ./HEMCO_Diagn.rc_templates/HEMCO_Diagn.rc.${sim_type}    ${rundir}/HEMCO_Diagn.rc

#--------------------------------------------------------------------
# Create symbolic links to data directories, restart files, and code
#--------------------------------------------------------------------
ln -s ${gcdir}                                  ${rundir}/CodeDir
# NOTE: CodeDir is set to point to GCHP/..; reset using setCodeDir in rundir.
ln -s ${GC_DATA_ROOT}/CHEM_INPUTS               ${rundir}/ChemDataDir
ln -s ${GC_DATA_ROOT}/HEMCO                     ${rundir}/MainDataDir
ln -s ${GC_DATA_ROOT}/GEOS_0.25x0.3125/GEOS_FP  ${rundir}/MetDir
ln -s ${GC_DATA_ROOT}/GCHP/TileFiles            ${rundir}/TileFiles
restarts=${GC_DATA_ROOT}/SPC_RESTARTS
for N in 24 48 90 180 360
do
    ln -s ${restarts}/initial_GEOSChem_rst.c${N}_${sim_name_long}.nc  ${rundir}
done

#-----------------------------------------------------------------
# Replace token strings in certain files
#-----------------------------------------------------------------
sed -i -e "s|{SIMULATION}|${sim_name_long}|" ${rundir}/GCHP.rc
sed -i -e "s|{SIMULATION}|${sim_name_long}|" ${rundir}/runConfig.sh
sed -i -e "s|{DATA_ROOT}|${GC_DATA_ROOT}|"   ${rundir}/input.geos

# Special handling for start/end date based on simulation so that
# start matches default initial restart files. Run directory is
# always initially set up for a 1-hour duration run.
if [ "${sim_type}" == "RnPbBe" ]; then
    startdate="20160101"
elif [ "${sim_type}" == "fullchem" ]; then
    startdate="20160701"
else
    printf "\nError: Start date is not defined for simulation ${sim_type}."
fi
enddate=${startdate}
starttime="000000"
endtime="010000"    
dYYYYMMDD="00000000"
dHHmmSS="010000"
sed -i -e "s|{DATE1}|${startdate}|"     ${rundir}/runConfig.sh
sed -i -e "s|{TIME1}|${starttime}|"     ${rundir}/runConfig.sh
sed -i -e "s|{DATE2}|${enddate}|"       ${rundir}/runConfig.sh
sed -i -e "s|{TIME2}|${endtime}|"       ${rundir}/runConfig.sh
sed -i -e "s|{dYYYYMMDD}|${dYYYYMMDD}|" ${rundir}/runConfig.sh
sed -i -e "s|{dHHmmss}|${dHHmmSS}|"     ${rundir}/runConfig.sh
sed -i -e "s|{DATE1}|${startdate}|"     ${rundir}/CAP.rc
sed -i -e "s|{TIME1}|${starttime}|"     ${rundir}/CAP.rc
sed -i -e "s|{DATE2}|${enddate}|"       ${rundir}/CAP.rc
sed -i -e "s|{TIME2}|${endtime}|"       ${rundir}/CAP.rc
sed -i -e "s|{dYYYYMMDD}|${dYYYYMMDD}|" ${rundir}/CAP.rc
sed -i -e "s|{dHHmmss}|${dHHmmSS}|"     ${rundir}/CAP.rc

#-----------------------------------------------------------------
# Set permissions
#-----------------------------------------------------------------
chmod 744 ${rundir}/setCodeDir
chmod 744 ${rundir}/build.sh
chmod 744 ${rundir}/Makefile
chmod 744 ${rundir}/runConfig.sh
chmod 744 ${rundir}/archiveRun.sh
chmod 744 ${rundir}/runScriptSamples/*
chmod 744 ${rundir}/bashrcSamples/*
chmod 644 ${rundir}/runScriptSamples/README

#----------------------------------------------------------------------
# Archive GCHP repository version in run directory file rundir.version
#----------------------------------------------------------------------
version_log=${rundir}/rundir.version
echo "This run directory was created with GCHP/Run/createRunDir.sh." > ${version_log}
echo " " >> ${version_log}
echo "GCHP version information:" >> ${version_log}
cd ${gchpdir}
remote_url=$(git config --get remote.origin.url)   
code_branch=$(git rev-parse --abbrev-ref HEAD)   
last_commit=$(git log -n 1 --pretty=format:"%s") 
commit_date=$(git log -n 1 --pretty=format:"%cd")
commit_user=$(git log -n 1 --pretty=format:"%cn")
commit_hash=$(git log -n 1 --pretty=format:"%h") 
cd ${curdir}
printf "\n  Remote URL: ${remote_url}" >> ${version_log}
printf "\n  Branch: ${code_branch}"    >> ${version_log}
printf "\n  Commit: ${last_commit}"    >> ${version_log}
printf "\n  Date: ${commit_date}"      >> ${version_log}
printf "\n  User: ${commit_user}"      >> ${version_log}
printf "\n  Hash: ${commit_hash}"      >> ${version_log}

#-----------------------------------------------------------------
# Ask user whether to track run directory changes with git
#-----------------------------------------------------------------
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

#-----------------------------------------------------------------
# Done!
#-----------------------------------------------------------------
printf "\nCreated ${rundir}\n"
