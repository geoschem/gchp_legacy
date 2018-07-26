#!/bin/bash

# This copies all relevant files from GEOSagcm to GEOS-Chem.
# Sym links to the GEOS-Chem repo are then created in GEOSagcm.
# This allows git tracking of changes to the GEOSagcm files, and
# merging or cherry-picking commits from other GEOS-Chem branches.

# Workflow:
# 
# TODO: To be filled in
#
# After creating a tag, go to the GEOS-Chem repositories and commit.
# Include the CVS tag name in your commit message.

###################################################################
# Set Paths
###################################################################

curdir=$(pwd)

# Prompt for GEOSagcm path
printf "Enter GEOSagcm path:\n"
read geosdir

# Prompt for GEOS-Chem git repository path (gc_bleeding_edge)
printf "Enter GEOSChem git repository path:\n"
read gcdir

# Helpful constants
gridcompname="GEOSCHEMchem_GridComp"

# Define dependent paths
geoschem_gridcomp=${geosdir}/src/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSchem_GridComp
geoschemchem_gridcomp=${geoschem_gridcomp}/${gridcompname}
gc_column=${geoschemchem_gridcomp}/gc_column
gigc=${gc_column}/GIGC
gchp=${gcdir}/GCHP

###################################################################
# Check paths
###################################################################

#-------------------------------------------
# Define function to check that path exists
# arg1 = path to check
#-------------------------------------------
checkpath () {
  if [[ ! -d $1 ]]; then
    inputdir=$1 
    if [[ ${inputdir##*/} = ${gridcompname} ]]; then
      printf "\nWarning: ${gridcompname} does not exist in GEOSagcm!\n"
      return 1
    else
      printf "\nError: Path $1 does not exist!\n"
      exit 1
    fi
  fi
}

#----------------------------
# Always check certain paths
#----------------------------
checkpath ${gcdir}
checkpath ${gchp}
checkpath ${geosdir}
checkpath ${geoschem_gridcomp}


###################################################################
# Make GEOSagcm GEOSCHEMchem_GridComp backup
###################################################################

#-------------------------------------
# Ask the user whether to make backup
#-------------------------------------
printf "\nDo you want to backup the existing ${gridcompname} directory? (y/n/q)\n"
read reply
if [[ ${reply} =~ ^[Yy]$ ]]; then
  printf "Enter path for storing backup (does not need to exist yet):\n"
  read backupdir
  if [ ! -d ${backupdir} ]; then
    mkdir -p ${backupdir}
  fi
  cp -rf ${geoschemchem_gridcomp} ${backupdir}
  printf "Backup complete."
elif [[ ${reply} =~ ^[Qq]$ ]]; then
  exit 1
elif [[ ! ${reply} =~ ^[Nn]$ ]]; then
  printf "Undefined answer. Exiting.\n"
  exit 1
fi 

###################################################################
# Restore GEOSagcm GEOSCHEMchem_GridComp backup
###################################################################

#----------------------------------------
# Ask the user whether to restore backup
#----------------------------------------
printf "\nDo you want to restore a ${gridcompname} backup? (y/n/q)\n"
read reply
if [[ ${reply} =~ ^[Yy]$ ]]; then
  printf "Enter path of backup to copy (do not include ${gridcompname}):\n"
  read backupdir
  rm -rf ${geoschemchem_gridcomp}
  cp -rf ${backupdir}/${gridcompname} ${geoschem_gridcomp}

  # Check that paths are there
  checkpath ${geoschemchem_gridcomp}
  if [[ $? -eq "0" ]]; then
    checkpath ${gc_column}
    checkpath ${gigc}
  fi

  if [ $? -eq "0" ]; then
    printf "Restore complete.\n"
  else
    printf "Backup not restored. Exiting.\n"
    exit 1
  fi
elif [[ ${reply} =~ ^[Qq]$ ]]; then
  exit 1
elif [[ ! ${reply} =~ ^[Nn]$ ]]; then
  printf "Undefined answer. Exiting.\n"
  exit 1
fi

###################################################################
# Check git
###################################################################

#----------------------------------------
# Define function to retrieve git status
# arg1 = model name string
# arg2 = repo path 
# arg3 = current path 
#----------------------------------------
checkgitstatus () {
  cd $2
  printf "\nEntering $1 repository. Fetching updates from remote...\n"
  git fetch
  printf "\nRetrieving git status for $2...\n"
  printf "$(git status)\n\n"
  printf "Inspect git info above. Look especially at branch name and uncommitted files.\nDo you want to proceed? (y) Hit any other key to quit.\n"
  read reply
  if [[ ${reply} =~ ^[Yy]$ ]]; then
    cd $3
  else
    cd $3
    exit 1
  fi
}

#-----------------------------------
# Ask the user whether to check git
#-----------------------------------
printf "\nDo you want to inspect the GEOS-Chem git repo status? (y/n/q) (RECOMMENDED)\n"
read reply
if [[ ${reply} =~ ^[Yy]$ ]]; then
  checkgitstatus "GEOS-Chem" ${gcdir} ${curdir}
  checkgitstatus "GCHP" ${gchp} ${curdir}
elif [[ ${reply} =~ ^[Qq]$ ]]; then
  exit 1
elif [[ ! ${reply} =~ ^[Nn]$ ]]; then
  printf "Undefined answer. Exiting.\n"
  exit 1
fi

###################################################################
# List files/directories missing in one or the other models
###################################################################

#---------------------------------------------------------------------
# Define function to recursively compare files in the two models
# arg1 = model1 item path             (initially called with root)
# arg2 = model1 root dir name         (constant during recursion) 
# arg3 = model2 root path             (constant during recursion)
# arg4 = model2 root dir name         (constant during recursion)
# arg5 = model1 tree path beyond root (used internally for recursion)
# arg6 = model2 tree path beyond root (used internally for recursion)
#---------------------------------------------------------------------
comparefiles () {
  for item in $1/*
  do
    itemname1=${item##*/}  

    # Assume same name in other model
    itemname2=${itemname1}

    # If directory
    if [[ -d ${item} ]]; then

      # Always skip CVS directories
      if [[ ${itemname1} == "CVS" ]]; then continue; fi

      # Special handling for GIGC: not same name in other model
      if [[ ${itemname1} == "GCHP" ]]; then
        printf "  * $2/GCHP/ is compared to $4/GIGC/\n"
        itemname2="GIGC"
      elif  [[ ${itemname1} == "GIGC" ]]; then
        printf "  * $2/GIGC/ is compared to $4/GCHP/\n"
	itemname2="GCHP"
      fi

      # Print and continue if directory not in other model
      if [[ ! -d $3/$6/${itemname2} ]]; then
        printf "  $2$5/${itemname1}/\n"
        continue
      fi 

      # Recursively step into directory, keeping track of tree
      model1path=$5/${itemname1}
      model2path=$6/${itemname2}
      comparefiles ${item} $2 $3 $4 ${model1path} ${model2path}

    # If file
    elif [[ -f ${item} ]]; then
   
      # Print file path if not found in other model
      if [[ ! -f $3/$6/${itemname1} ]]; then
        printf "  $2$5/${itemname2}\n"
      fi  

    # Give warning if not a file or directory
    else
      printf "  * Warning: no file or directory: $2/$5/${itemname2}\n"
    fi
  done
}

#---------------------------------------
# Ask the user whether to compare files
#---------------------------------------
printf "\nDo you want to display files/directories that are in one model only? (y/n/q)\n"
read reply
if [[ ${reply} =~ ^[Yy]$ ]]; then

  printf "\nComparing ${gridcompname} files to files in GEOS-Chem...\n"
  printf "===================================================================\n"
  printf "*** Warning ***: The below items cannot be replaced with links.\n"
  printf "Copy to the GEOS-Chem git repos if you wish to use and track, and\n"
  printf "include them in the next commit so they are linked in the future.\n"
  printf "Otherwise, delete them in the next CVS tag.\n"
  printf "===================================================================\n"
  printf "The following are in ${gridcompname} but NOT ${gcdir##*/}:\n"
  comparefiles ${gc_column} ${gc_column##*/} ${gcdir} ${gcdir##*/}

  printf "Comparing GEOS-Chem files to files in ${gridcompname}...\n"
  printf "===================================================================\n"
  printf "*** Warning ***: links cannot be created to the below items.\n"
  printf "Create a symlink to files in GEOS-5 if you wish to use and track, and\n"
  printf "include them in the next CVS tag so they are linked in the future.\n"
  printf "Otherwise, be conscious that they are not in GEOS-5.\n"
  printf "===================================================================\n"
  printf "The following are in ${gcdir##*/} but NOT in ${gridcompname}:\n"
  comparefiles ${gcdir} ${gcdir##*/} ${gc_column} ${gc_column##*/}

elif [[ ${reply} =~ ^[Qq]$ ]]; then
  exit 1
elif [[ ! ${reply} =~ ^[Nn]$ ]]; then
  printf "Undefined answer. Exiting.\n"
  exit 1
fi

###################################################################
# Copy files from GEOSCHEMchem_GridComp to git repos
###################################################################

#-------------------------------------------------------------
# Ask the user whether to copy files from GEOS-5 to GEOS-Chem
#-------------------------------------------------------------
printf "\nDo you want to copy ${gridcompname} files to GEOS-Chem repos? (y/n/q)\n"
read reply
if [[ ${reply} =~ ^[Yy]$ ]]; then

  printf "***Warning: Uncommitted changes in GEOS-Chem repos will be lost!\n"
  printf "Consider rerunning this script to check git repo status if you have not already.\n"
  printf "To keep uncommitted changes, checkout branches and commit, or use git stash.\n"
  printf "NOTE: Eventually all work should be done off of a previous git commit and so this copy step would always be skipped."
  printf "Are you sure you want to proceed to copy files? (y/n/q)\n"

  read reply
  if [[ ${reply} =~ ^[Yy]$ ]]; then

    # GEOSCHEMchem_GridComp (do manually for now)
    cp ${geoschemchem_gridcomp}/${gridcompname}Mod.F90   ${gchp}/Chem_GridCompMod.F90
    cp ${geoschemchem_gridcomp}/Includes_Before_Run.H    ${gchp}
    cp ${geoschemchem_gridcomp}/Includes_After_Run.H     ${gchp}
    cp ${geoschemchem_gridcomp}/GEOSCHEMchem_Registry.rc ${gchp}/Registry/Chem_Registry.rc

    # Files stored in GCHP/RC (rundir files and GEOS-5 perl scripts)
    cp ${geoschemchem_gridcomp}/brc.dat.rc               ${gchp}/RC

    cp ${geoschemchem_gridcomp}/diaginfo.dat.rc          ${gchp}/RC
    cp ${geoschemchem_gridcomp}/dust.dat.rc              ${gchp}/RC
    cp ${geoschemchem_gridcomp}/ExtData.rc              ${gchp}/RC
    cp ${geoschemchem_gridcomp}/FJX_j2j.dat.rc           ${gchp}/RC
    cp ${geoschemchem_gridcomp}/FJX_spec.dat.rc          ${gchp}/RC
    cp ${geoschemchem_gridcomp}/gcIncAft                 ${gchp}/RC
    cp ${geoschemchem_gridcomp}/gcIncBef                 ${gchp}/RC
    cp ${geoschemchem_gridcomp}/gcUtImp                  ${gchp}/RC
    cp ${geoschemchem_gridcomp}/gcUtInt                  ${gchp}/RC
    cp ${geoschemchem_gridcomp}/GEOSCHEMchem_ExtData.rc  ${gchp}/RC
    cp ${geoschemchem_gridcomp}/GEOSCHEMchem_GridComp.rc ${gchp}/RC
    cp ${geoschemchem_gridcomp}/h2so4.dat.rc             ${gchp}/RC
    cp ${geoschemchem_gridcomp}/HEMCO_Config.rc          ${gchp}/RC
    cp ${geoschemchem_gridcomp}/HEMCO_DiagnFile.rc       ${gchp}/RC
    cp ${geoschemchem_gridcomp}/input.geos.rc            ${gchp}/RC
    cp ${geoschemchem_gridcomp}/jv_spec_mie.dat.rc       ${gchp}/RC
    cp ${geoschemchem_gridcomp}/MAPL.rc                  ${gchp}/RC
    cp ${geoschemchem_gridcomp}/org.dat.rc               ${gchp}/RC
    cp ${geoschemchem_gridcomp}/so4.dat.rc               ${gchp}/RC
    cp ${geoschemchem_gridcomp}/soot.dat.rc              ${gchp}/RC
    cp ${geoschemchem_gridcomp}/ssa.dat.rc               ${gchp}/RC
    cp ${geoschemchem_gridcomp}/ssc.dat.rc               ${gchp}/RC
    cp ${geoschemchem_gridcomp}/tracerinfo.dat.rc        ${gchp}/RC

    # GIGC (do manually for now)
    cp ${gigc}/gigc_chunk_mod.F90 ${gchp}
    cp ${gigc}/gigc_historyexports_mod.F90 ${gchp}
    cp ${gigc}/gigc_providerservices_mod.F90 ${gchp}
    
    # GIGC (automatic) - do this eventually?
    #rsync -r ${gigc}/ --exclude CVS/ ${gchp} 
    
    # gc_column
    rsync -r ${gc_column}/ --exclude CVS/ --exclude GIGC/ ${gcdir} 
    
    printf "Copy complete\n"
    
    # Check git
    printf "\nDo you want to check the GEOS-Chem repo status again? (y/n/q) (RECOMMENDED)\n"
    read reply
    if [[ ${reply} =~ ^[Yy]$ ]]; then
      checkgitstatus "GEOS-Chem" ${gcdir} ${curdir}
      checkgitstatus "GCHP" ${gchp} ${curdir}
    elif [[ ${reply} =~ ^[Qq]$ ]]; then
      exit 0
    fi

  elif [[ ${reply} =~ ^[Qq]$ ]]; then
    exit 1
  elif [[ ! ${reply} =~ ^[Nn]$ ]]; then
    printf "Undefined answer. Exiting.\n"
    exit 1
  fi

elif [[ ${reply} =~ ^[Qq]$ ]]; then
  exit 1
elif [[ ! ${reply} =~ ^[Nn]$ ]]; then
  printf "Undefined answer. Exiting.\n"
  exit 1
fi

###################################################################
# Replace GEOSCHEMchem_GridComp files with links to git repo files
###################################################################

#---------------------------------------
# Define function to set single symlink
# arg1 = target
# arg2 = pointer
#---------------------------------------
makelink () {
  if [ -f $1 ] || [ -L $1 ]; then
    ln -sf $1 $2
  else
    printf "Warning: ${1##*/} not in target repo\n"
  fi
}

#--------------------------------------------------------------
# Define function to recursively set multiple symlinks
# arg1 = target repo path
# arg2 = tree path within repo (used internally for recursion)
#        (common for both GEOS-5 and GEOS-CHEM)
#--------------------------------------------------------------
makelinks () {
  for item in $1/*
  do
    itemname=${item##*/}  

    # If subdirectory
    if [[ -d ${item} ]]; then

      # Always skip CVS and GIGC directories
      if [[ ${itemname} == "CVS" || ${itemname} == "GIGC" ]]; then continue; fi
      commonpath=$2/${itemname}

      # Always skip directories not in the GEOS-Chem repo
      if [[ ! -d ${gcdir}/${commonpath} ]]; then
        printf "Warning: skipping files in gc_column/${commonpath}/: directory not in git repo\n"
        continue
      fi

      # Recursively step into directory
      makelinks ${item} ${commonpath}

    # If file
    elif [ -f ${item} ] || [ -L ${item} ]; then

      # Always skip files not in the GEOS-Chem repo; otherwise make a link
      if [[ ! -f ${gcdir}/$2/${itemname} ]]; then
        printf "Warning: target not found for gc_column/$2/${itemname}\n"
      else
        ln -sf ${gcdir}/$2/${itemname} ${gc_column}/$2/${itemname}
      fi  

    # Give warning if not a file or directory
    else
      printf "Warning: no file or directory: gc_column/$2/${itemname}\n"
    fi
  done
}

#-------------------------------------------------------------------
# Ask the user whether to set links in GEOS-5 pointing to GEOS-CHEM
#-------------------------------------------------------------------
printf "\nDo you want to replace GEOSCHEMchem_GridComp files with GEOS-Chem symbolic links? (y/n/q)\n"
read reply
if [[ ${reply} =~ ^[Yy]$ ]]; then

  printf "***Warning: Files in GEOSCHEMchem_GridComp will be deleted!\n"
  printf "Consider rerunning this script to create backup if you have not already.\n"
  printf "Are you sure you want to proceed to create links? (y/n/q)\n"
  read reply
  if [[ ${reply} =~ ^[Yy]$ ]]; then

    # Point to GEOS-Chem repo files (this is automated)
    makelinks ${gc_column}

    # Point to GCHP repo files (do this manually for now since exceptions)

    # GCHP core source code
    makelink ${gchp}/gigc_chunk_mod.F90 ${gigc}
    makelink ${gchp}/gigc_historyexports_mod.F90   ${gigc}
    makelink ${gchp}/gigc_providerservices_mod.F90 ${gigc}

    # gridcomp file and supporting files
    makelink ${gchp}/Chem_GridCompMod.F90          ${geoschemchem_gridcomp}/GEOSCHEMchem_GridCompMod.F90 
    makelink ${gchp}/Includes_Before_Run.H         ${geoschemchem_gridcomp}
    makelink ${gchp}/Includes_After_Run.H          ${geoschemchem_gridcomp}
    makelink ${gchp}/Registry/Chem_Registry.rc     ${geoschemchem_gridcomp}/GEOSCHEMchem_Registry.rc
    
    # Files tracked in GCHP/RC folder (rundir files and GEOS-5 perl scripts)
    makelink ${gchp}/RC/brc.dat.rc               ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/diaginfo.dat.rc          ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/dust.dat.rc              ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/ExtData.rc               ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/FJX_j2j.dat.rc           ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/FJX_spec.dat.rc          ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/gcIncAft                 ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/gcIncBef                 ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/gcUtImp                  ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/gcUtInt                  ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/GEOSCHEMchem_ExtData.rc  ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/GEOSCHEMchem_GridComp.rc ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/h2so4.dat.rc             ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/HEMCO_Config.rc          ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/HEMCO_DiagnFile.rc       ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/input.geos.rc            ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/jv_spec_mie.dat.rc       ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/MAPL.rc                  ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/org.dat.rc               ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/so4.dat.rc               ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/soot.dat.rc              ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/ssa.dat.rc               ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/ssc.dat.rc               ${geoschemchem_gridcomp}
    makelink ${gchp}/RC/tracerinfo.dat.rc        ${geoschemchem_gridcomp}
    
    printf "Creating symlinks complete.\n"

  elif [[ ${reply} =~ ^[Qq]$ ]]; then
    exit 1
  fi 

elif [[ ${reply} =~ ^[Qq]$ ]]; then
  exit 1
fi 


