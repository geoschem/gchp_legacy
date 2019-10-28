#!/bin/bash

# createRunDir.sh: Create GCHP run directory
#
# Optional arguments:
#   1. -s or --silent : turn on silent mode (non-interactive) (optional)
#   2. filename       : config file for silent model (optional)
#                       default filename is createRunDir.cfg
#
# Usage:
#    Interactive mode: ./createRunDir.sh
#    Silent model    : ./createRundir.sh -s createRunDir.cfg
#
# Initial version: E. Lundgren,10/5/2018

curdir=$(pwd)
cd ..
gchpdir=$(pwd)
cd ..
gcdir=$(pwd)
cd ${curdir}

#-----------------------------------------------------------------
# Silent mode handling (optional)
#
# WARNING: ExtData and gFTL paths will be taken from config file
#          if running in silent mode
#-----------------------------------------------------------------
silent=
configfile=
while [ "$1" != "" ]; do
    case $1 in
        -s )  silent=1
              shift
              configfile=$1
              ;;
        * )   usage
              exit 1
    esac
    shift
done
if [ "$silent" == "1" ]; then
   if [[ -z "$configfile" ]]; then
      configfile=createRunDir.cfg
   source $configfile; fi
   if [[ ! -f ${configfile} ]]; then
      printf "Config file ${configfile} does not exist.\n"
      exit 1
   fi
   if [[ ! -d ${extdata} ]]; then
      printf "${extdata} does not exist. Update path in ${configfile}.\n"
      exit 1
   fi
   if [[ ! -d ${gftl} ]]; then
      printf "${gftl} does not exist. Update path in ${configfile}.\n"
      exit 1
   fi
   if [[ ! -d ${rundir_path} ]]; then
      printf "${rundir_path} does not exist. Update path in ${configfile}.\n"
      exit 1
   fi
   GC_DATA_ROOT=${extdata}
   GFTL=${gftl}
fi

#-----------------------------------------------------------------
# If interactive, export data root path in ~/.geoschem/config if file exists
#-----------------------------------------------------------------
if [[ -z "$silent" ]]; then
   if [[ -f ${HOME}/.geoschem/config ]]; then
       source ${HOME}/.geoschem/config
       if [[ ! -d ${GC_DATA_ROOT} ]]; then
          printf "\nWarning: Default root data directory does not exist!"
          printf "\nSet new path below or manually edit ${HOME}/.geoschem/config.\n"
       fi
       if [[ ! -d ${GFTL} ]]; then
          printf "\nWarning: Default Goddard Fortran Template Library (gFTL) does not exist!"
          printf "\nSet new path below or manually edit ${HOME}/.geoschem/config.\n"
       fi
   else
       printf "\nDefine paths to ExtData and the Goddard Fortran Template Library (gFTL)."
       printf "\nThese will be stored in ${HOME}/.geoschem/config for future automatic use.\n"
       mkdir -p ${HOME}/.geoschem
   fi
fi

#-----------------------------------------------------------------
# If interactive, one-time configuration of data root path
# in ~/.geoschem/config.
#-----------------------------------------------------------------
if [[ -z "${GC_DATA_ROOT}" && -z "$silent" ]]; then
    printf "\nEnter path for ExtData:\n"
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
            echo "export GC_DATA_ROOT=${extdata}" >> ${HOME}/.geoschem/config
            source ${HOME}/.geoschem/config
	fi
    done
fi

#-----------------------------------------------------------------
# If interactive, one-time configuration of Goddard Fortran template
# library (gFTL) path in ~/.geoschem/config. Only in interactive mode.
#-----------------------------------------------------------------
if [[ -z "${GFTL}" && -z "$silent" ]]; then
    printf "\nIf you have not downloaded gFTL then enter q to exit."
    printf "\nFollow these instructions at the command prompt to install:\n"
    printf "\n      1. Navigate to directory where you want to download gFTL" 
    printf "\n      2. Type the following at the command prompt:"
    printf "\n         $ git clone https://github.com/Goddard-Fortran-Ecosystem/gFTL"
    printf "\n         $ cd gFTL"
    printf "\n         $ git checkout v1.0.0"
    printf "\n         $ mkdir build"
    printf "\n         $ cd build"
    printf "\n         $ cmake .. -DCMAKE_INSTALL_PREFIX=../install"
    printf "\n         $ make install"
    printf "\n      3. Verify success by checking that gFTL/install/include/templates and gFTL/install/include/types exist\n" 
    printf "\nEnter path for gFTL/install:\n"
    valid_path=0
    while [ "$valid_path" -eq 0 ]
    do
	read gftl
	if [[ ${gftl} = "q" ]]; then
	    printf "\nExiting.\n"
	    exit 1
	elif [[ ! -d ${gftl} ]]; then
            printf "\nError: ${gftl} does not exist. Enter a new path or hit q to quit.\n"
	else
	    valid_path=1
	    echo "export GFTL=${gftl}" >> ${HOME}/.geoschem/config
            source ${HOME}/.geoschem/config
	fi
    done
fi

#-----------------------------------------------------------------
# Set simulation type
#-----------------------------------------------------------------
if [[ -z "$silent" ]]; then
   printf "\nChoose simulation type:\n"
   printf "  1. TransportTracers\n"
   printf "  2. Standard\n"
   printf "  3. Benchmark\n"
fi
valid_sim=0
while [ "${valid_sim}" -eq 0 ]
do
    if [[ -z "$silent" ]]; then read sim_num; fi
    if [[ ${sim_num} = "1" ]]; then
	sim_name=TransportTracers
	sim_name_long=${sim_name}
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
        if [[ "$silent" == "1" ]]; then exit 1; fi
    fi
done

#-----------------------------------------------------------------
# Set meteorology source
#-----------------------------------------------------------------
if [[ -z "$silent" ]]; then
   printf "\nChoose meteorology source:\n"
   printf "  1. GEOS-FP\n"
   printf "  2. MERRA2\n"
fi
valid_met=0
while [ "${valid_met}" -eq 0 ]
do
    if [[ -z "$silent" ]]; then read met_num; fi
    if [[ ${met_num} = "1" ]]; then
	met_name='GEOSFP'
	met_resolution='025x03125'
	met_native='0.25x0.3125'
	met_latres='025'
	met_lonres='03125'
	met_extension='nc'
	met_cn_year='2011'
	pressure_unit='hPa'
	pressure_scale='1.0 '
	valid_met=1
    elif [[ ${met_num} = "2" ]]; then
	met_name='MERRA2'
	met_resolution='05x0625'
	met_native='0.5x0.625'
	met_latres='05'
	met_lonres='0625'
	met_extension='nc4'
	met_cn_year='2015'
	pressure_unit='Pa '
	pressure_scale='0.01'
	valid_met=1
    else
	printf "Invalid meteorology option. Try again.\n"
        if [[ "$silent" == "1" ]]; then exit 1; fi
    fi
done

#-----------------------------------------------------------------
# If interactive, ask user to define path where directoy will be
# created
#-----------------------------------------------------------------
if [[ -z "$silent" ]]; then
   printf "\nEnter path where the run directory will be created:\n";
   read rundir_path
   valid_path=0
   while [ "$valid_path" -eq 0 ]
   do
       if [[ ${rundir_path} = "q" ]]; then
           printf "\nExiting.\n"
           exit 1
       elif [[ ! -d ${rundir_path} ]]; then
           printf "\nError: ${rundir_path} does not exist. Enter a new path or hit q to quit.\n"
       else
   	valid_path=1
       fi
   done
fi

#-----------------------------------------------------------------
# Define run directoy name. Set run directory path to default if
# not provided.
#-----------------------------------------------------------------
if [[ -z "$silent" ]]; then
   printf "\nEnter run directory name:\n"
   read rundir_name
fi
if [[ -z "${rundir_name}" ]]; then
   rundir_name=gchp_${sim_name}
   printf "Using default directory name ${rundir_name}\n"
fi

#-----------------------------------------------------------------
# Check if run directory already exists. If yes and in interactive
# mode, ask user for a new run directory name.
#-----------------------------------------------------------------
rundir=${rundir_path}/${rundir_name}
valid_rundir=0
while [ "${valid_rundir}" -eq 0 ]
do
    if [[ -d ${rundir} ]]; then
	printf "Warning! ${rundir} already exists.\n"
        if [[ "$silent" == "1" ]]; then exit 1; fi
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
cp -r ./environmentFileSamples ${rundir} 
cp -r ./OutputDir              ${rundir} 
cp -r ./runScriptSamples       ${rundir}
cp ./archiveRun.sh             ${rundir} 
cp ./build.sh                  ${rundir} 
cp ./fvcore_layout.rc          ${rundir} 
cp ./input.nml                 ${rundir} 
cp ./README                    ${rundir}
cp ./setCodeDir                ${rundir}
cp ./setEnvironment            ${rundir}
cp ./Makefile                  ${rundir}
cp ./gitignore                 ${rundir}/.gitignore
cp ./GCHP.rc.template          ${rundir}/GCHP.rc
cp ./CAP.rc.template           ${rundir}/CAP.rc
cp ./runConfig.sh.template     ${rundir}/runConfig.sh
cp ./HISTORY.rc.templates/HISTORY.rc.${sim_name}            ${rundir}/HISTORY.rc
cp ./input.geos.templates/input.geos.${sim_name}            ${rundir}/input.geos
cp ./ExtData.rc.templates/ExtData.rc.${sim_type}            ${rundir}/ExtData.rc
cp ./HEMCO_Config.rc.templates/HEMCO_Config.rc.${sim_type}  ${rundir}/HEMCO_Config.rc
cp ./HEMCO_Diagn.rc.templates/HEMCO_Diagn.rc.${sim_name}    ${rundir}/HEMCO_Diagn.rc

# If benchmark simulation, put gchp.run script in directory; else do not.
if [ "${sim_name}" == "benchmark" ]; then
    cp ./runScriptSamples/gchp.benchmark.run           ${rundir}/gchp.benchmark.run
    chmod 744 ${rundir}/gchp.benchmark.run
fi

#--------------------------------------------------------------------
# Create symbolic links to data directories, restart files, and code
#--------------------------------------------------------------------
ln -s ${gcdir}                                  ${rundir}/CodeDir
# NOTE: CodeDir is set to point to GCHP/..; reset using setCodeDir in rundir.
ln -s ${GC_DATA_ROOT}/CHEM_INPUTS               ${rundir}/ChemDataDir
ln -s ${GC_DATA_ROOT}/HEMCO                     ${rundir}/MainDataDir
ln -s ${GFTL}                                   ${rundir}/gFTL
if [ "${met_name}" == "GEOSFP" ]; then
   ln -s ${GC_DATA_ROOT}/GEOS_0.25x0.3125/GEOS_FP  ${rundir}/MetDir
else
   ln -s ${GC_DATA_ROOT}/GEOS_0.5x0.625/MERRA2  ${rundir}/MetDir
fi
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
sed -i -e "s|{MET}|${met_name}|"             ${rundir}/input.geos
sed -i -e "s|{DATA_ROOT}|${GC_DATA_ROOT}|"   ${rundir}/HEMCO_Config.rc
sed -i -e "s|{NATIVE_RES}|${met_native}|"    ${rundir}/HEMCO_Config.rc
sed -i -e "s|{LATRES}|${met_latres}|"        ${rundir}/HEMCO_Config.rc
sed -i -e "s|{LONRES}|${met_lonres}|"        ${rundir}/HEMCO_Config.rc
sed -i -e "s|{MET_SOURCE}|${met_name}|"      ${rundir}/ExtData.rc # 1st in line
sed -i -e "s|{MET_SOURCE}|${met_name}|"      ${rundir}/ExtData.rc # 2nd in line
sed -i -e "s|{MET_RES}|${met_resolution}|"   ${rundir}/ExtData.rc
sed -i -e "s|{NATIVE_RES}|${met_native}|"    ${rundir}/ExtData.rc
sed -i -e "s|{LATRES}|${met_latres}|"        ${rundir}/ExtData.rc
sed -i -e "s|{LONRES}|${met_lonres}|"        ${rundir}/ExtData.rc
sed -i -e "s|{MET_EXT}|${met_extension}|"    ${rundir}/ExtData.rc
sed -i -e "s|{MET_CN_YR}|${met_cn_year}|"    ${rundir}/ExtData.rc # 1st in line
sed -i -e "s|{MET_CN_YR}|${met_cn_year}|"    ${rundir}/ExtData.rc # 2nd in line
sed -i -e "s|{PRES_UNIT}|${pressure_unit}|"  ${rundir}/ExtData.rc
sed -i -e "s|{PRES_SCALE}|${pressure_scale}|" ${rundir}/ExtData.rc

# Special handling for start/end date based on simulation so that
# start year/month/day matches default initial restart file.
if [ "${sim_type}" == "TransportTracers" ]; then
    startdate="20160101"
    enddate="20160101"
elif [ "${sim_name}" == "benchmark" ]; then
    startdate="20160701"
    enddate="20160801"
elif [ "${sim_type}" == "fullchem" ]; then
    startdate="20160701"
    enddate="20160701"
else
    printf "\nError: Start date is not defined for simulation ${sim_type}."
fi
sed -i -e "s|{DATE1}|${startdate}|"     ${rundir}/runConfig.sh
sed -i -e "s|{DATE2}|${enddate}|"       ${rundir}/runConfig.sh
sed -i -e "s|{DATE1}|${startdate}|"     ${rundir}/CAP.rc
sed -i -e "s|{DATE2}|${enddate}|"       ${rundir}/CAP.rc

# Special handling for benchmark simulation
if [ "${sim_name}" == "benchmark" ]; then
    total_cores=48
    num_nodes=3
    num_cores_per_node=16
    grid_res=48
    diag_freq="7440000"
    start_time="000000"
    end_time="000000"    
    dYYYYMMDD="00000100"
    dHHmmSS="000000"
else
    total_cores=6
    num_nodes=1
    num_cores_per_node=6
    grid_res=24
    diag_freq="010000"
    start_time="000000"
    end_time="010000"    
    dYYYYMMDD="00000000"
    dHHmmSS="010000"
fi
diag_dur=${diag_freq}
sed -i -e "s|{TotalCores}|${total_cores}|"             ${rundir}/runConfig.sh
sed -i -e "s|{NumNodes}|${num_nodes}|"                 ${rundir}/runConfig.sh
sed -i -e "s|{NumCoresPerNode}|${num_cores_per_node}|" ${rundir}/runConfig.sh
sed -i -e "s|{GridRes}|${grid_res}|"                   ${rundir}/runConfig.sh
sed -i -e "s|{DiagFreq}|${diag_dur}|"                  ${rundir}/runConfig.sh
sed -i -e "s|{DiagDur}|${diag_freq}|"                  ${rundir}/runConfig.sh
sed -i -e "s|{TIME1}|${start_time}|"     ${rundir}/runConfig.sh
sed -i -e "s|{TIME2}|${end_time}|"       ${rundir}/runConfig.sh
sed -i -e "s|{dYYYYMMDD}|${dYYYYMMDD}|"  ${rundir}/runConfig.sh
sed -i -e "s|{dHHmmss}|${dHHmmSS}|"      ${rundir}/runConfig.sh
sed -i -e "s|{TIME1}|${start_time}|"     ${rundir}/CAP.rc
sed -i -e "s|{TIME2}|${end_time}|"       ${rundir}/CAP.rc
sed -i -e "s|{dYYYYMMDD}|${dYYYYMMDD}|"  ${rundir}/CAP.rc
sed -i -e "s|{dHHmmss}|${dHHmmSS}|"      ${rundir}/CAP.rc

#-----------------------------------------------------------------
# Set permissions
#-----------------------------------------------------------------
chmod 744 ${rundir}/setCodeDir
chmod 744 ${rundir}/setEnvironment
chmod 744 ${rundir}/build.sh
chmod 744 ${rundir}/Makefile
chmod 744 ${rundir}/runConfig.sh
chmod 744 ${rundir}/archiveRun.sh
chmod 744 ${rundir}/runScriptSamples/*
chmod 744 ${rundir}/environmentFileSamples/*
chmod 644 ${rundir}/runScriptSamples/README

#----------------------------------------------------------------------
# Archive GCHP repository version in run directory file rundir.version
#----------------------------------------------------------------------
version_log=${rundir}/rundir.version
echo "This run directory was created with GCHP/Run/createRunDir.sh." > ${version_log}
echo " " >> ${version_log}
echo "GCHP repository version information:" >> ${version_log}
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
# Set whether to track run directory changes with git
#-----------------------------------------------------------------
if [[ -z "$silent" ]]; then
   printf "\nDo you want to track run directory changes with git? (y/n)\n"
fi
valid_response=0
while [ "$valid_response" -eq 0 ]
do
    if [[ -z "$silent" ]]; then read enable_git; fi
    if [[ ${enable_git} = "y" ]]; then
	cd ${rundir}
	printf "\n\nChanges to the following run directory files are tracked by git:\n\n" >> ${version_log}
	git init
	git add *.rc *.sh environmentFileSamples/* runScriptSamples/* README .gitignore
	git add setCodeDir Makefile input.geos input.nml
	printf " " >> ${version_log}
	git commit -m "Initial run directory" >> ${version_log}
	cd ${curdir}
	valid_response=1
    elif [[ ${enable_git} = "n" ]]; then
	valid_response=1
    else
	printf "Input not recognized. Try again.\n"
        if [[ "$silent" == "1" ]]; then exit 1; fi
    fi
done

#-----------------------------------------------------------------
# Done!
#-----------------------------------------------------------------
printf "\nCreated ${rundir}\n"
