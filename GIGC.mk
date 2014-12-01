#------------------------------------------------------------------------------
#                  GEOS-Chem Global Chemical Transport Model                  !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: GIGC.mk
#
# !DESCRIPTION: Makefile fragment that specifies include and library
#  paths for the various phases of the GIGC build sequence.
#\\
#\\
# !REMARKS:
#  Updated from Mike Long
#
# !REVISION HISTORY:
#  18 Nov 2014 - M. Long     - Initial version
#  18 Nov 2014 - R. Yantosca - Now use env vars to specify MPI inc & lib dirs
#  01 Dec 2014 - R. Yantosca - Now put FV_LIB before MPI_LIB in link command
#EOP
#------------------------------------------------------------------------------
#BOC

#==============================================================================
# (1) Root directories
#==============================================================================

# %%%%% Root dir for MAPL etc %%%%%
ifndef ESMADIR
 export ESMADIR=$(PWD)/GIGC/Shared
endif

# %%%%% Root dir for ESMF %%%%%
ifndef ESMF_DIR
 export ESMF_DIR=$(PWD)/GIGC/ESMF
endif

# %%%%% Root dir for FVdycore %%%%%
ifndef FVDIR
 export FVDIR=$(PWD)/GIGC/FVdycoreCubed_GridComp
endif

#==============================================================================
# (2) GIGC/GEOS-Chem general settings
#
# The following are environment settings for GIGC to compile within the
# GEOS-Chem framework. They are dependent upon the settings in Section (1).
#==============================================================================

# %%%%% Architecture %%%%%
ifndef ARCH
  ARCH        := $(shell uname -s)
endif

# %%%%% ESMF settings %%%%%
ESMF_MOD      := -I$(ESMF_DIR)/$(ARCH)/mod
ESMF_INC      := -I$(ESMF_DIR)/$(ARCH)/include
ESMF_LIB      := -lrt $(ESMF_DIR)/$(ARCH)/lib/libesmf.so

# %%%%% FVdycore settings %%%%%
FV_INC        := -I$(FVDIR)
FV_LIB        := -L$(ESMADIR)/$(ARCH)/lib -lFVdycoreCubed_GridComp -lfvdycore -lGFDL_fms -lGEOS_Shared -lGMAO_hermes

# %%%%% MAPL settings %%%%%
MAPL_INC      := -I$(ESMADIR)/$(ARCH)/include/MAPL_Base
MAPL_INC      += -I$(ESMADIR)/$(ARCH)/include/GMAO_mpeu
MAPL_INC      += -I$(ESMADIR)/$(ARCH)/include/GMAO_pilgrim
MAPL_INC      += -I$(ESMADIR)/$(ARCH)/include/GMAO_hermes
MAPL_LIB      := -L$(ESMADIR)/$(ARCH)/lib -lMAPL_Base -lMAPL_cfio -lGMAO_mpeu -lGMAO_pilgrim

# %%%%% OpenMPI settings %%%%%
MPI_LIB       := $(shell mpif90 --showme:link)
MPI_LIB       += $(shell mpicxx --showme:link)

# %%%%% Link command %%%%%
LINK          := -lGIGC $(MAPL_LIB) $(ESMF_LIB) $(FV_LIB) $(MPI_LIB) $(LINK) -lGIGC 

# %%%%% Fortran flags %%%%%
FFLAGS        := -double-size 32 -real-size 32 -r4
USER_FFLAGS   += -DESMF_ -DSPMD -DMAPL_MODE -DGLOBAL_GRID_CREATE
USER_DEFS     += -DESMF_
#EOC
