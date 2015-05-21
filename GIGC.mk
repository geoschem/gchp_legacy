#ifndef BASEDIR
#export BASEDIR=$(ROOTDIR)/GIGC
#endif 
#ifndef ESMADIR
#export ESMADIR=$(ROOTDIR)/GIGC/Shared
#endif
#ifndef ESMF_DIR
#export ESMF_DIR=$(ROOTDIR)/GIGC/ESMF
#endif
#<<MSL>># Make sure ESMADIR is defined
#<<MSL>># ----------------------------
#<<MSL>>
#<<MSL>>ifndef BASEDIR
#<<MSL>>export BASEDIR=$(PWD)/GIGC
#<<MSL>>endif 
ifndef ESMADIR
export ESMADIR=$(PWD)/GIGC/Shared
endif
ifndef ESMF_DIR
export ESMF_DIR=$(PWD)/GIGC/ESMF
endif
#<<MSL>>       RCDIR   :=$(BASEDIR)/Registry/
#<<MSL>>
#<<MSL>># Compilation rules, flags, etc
#<<MSL>># -----------------------------
#<<MSL>>ifeq ($(HPC),yes)
#<<MSL>>  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
#<<MSL>>  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
#<<MSL>>else
#<<MSL>>  include ./Shared/Config/ESMA_base.mk  # Generic stuff
#<<MSL>>  include ./Shared/Config/ESMA_arch.mk  # System dependencies
#<<MSL>>endif
#<<MSL>># ESMF-specific settings
#<<MSL>># ----------------------------
#<<MSL>>export ESMF_COMPILER=intel
#<<MSL>>export ESMF_COMM=openmpi
#<<MSL>>export ESMF_INSTALL_PREFIX=$(ESMF_DIR)/$(ARCH)
#<<MSL>>export ESMF_INSTALL_LIBDIR=$(ESMF_DIR)/$(ARCH)/lib
#<<MSL>>export ESMF_INSTALL_MODDIR=$(ESMF_DIR)/$(ARCH)/mod
#<<MSL>>export ESMF_INSTALL_HEADERDIR=$(ESMF_DIR)/$(ARCH)/include
#<<MSL>>export ESMF_F90COMPILEOPTS=-align all -fPIC -traceback 
#<<MSL>>export ESMF_CXXCOMPILEOPTS=-fPIC
#<<MSL>>export ESMF_OPENMP=OFF
#<<MSL>>
#----------

# 2) GIGC/GEOS-Chem general settings
# The following are environment settings
# for GIGC to compile within the GEOS-Chem
# framework. They are dependent upon what 
# has already be set above in this file
ifndef ARCH
  ARCH := $(shell uname -s)
endif
USER_FFLAGS   += -DESMF_ -DSPMD -DMAPL_MODE -DGLOBAL_GRID_CREATE
USER_DEFS     += -DESMF_
ESMF_MOD      := -I$(ESMF_DIR)/$(ARCH)/mod
ESMF_INC      := -I$(ESMF_DIR)/$(ARCH)/include
ESMF_LIB      := -lrt $(ESMF_DIR)/$(ARCH)/lib/libesmf.so
FV_INC        := -I./FVdycoreCubed_GridComp/
FV_LIB        := -L$(ESMADIR)/$(ARCH)/lib -lFVdycoreCubed_GridComp -lfvdycore -lGFDL_fms -lGEOS_Shared -lGMAO_hermes
MAPL_INC      := -I$(ESMADIR)/$(ARCH)/include/MAPL_Base
MAPL_INC      += -I$(ESMADIR)/$(ARCH)/include/GMAO_mpeu
MAPL_INC      += -I$(ESMADIR)/$(ARCH)/include/GMAO_pilgrim
MAPL_INC      += -I$(ESMADIR)/$(ARCH)/include/GMAO_hermes
MAPL_LIB      := -L$(ESMADIR)/$(ARCH)/lib -lMAPL_Base -lMAPL_cfio -lGMAO_mpeu -lGMAO_pilgrim
MPI_INC       := $(dir $(shell which mpif90))../include
#OPENMPI-- MPI_LIB       := -L$(dir $(shell which mpif90))../lib -lmpi_mpifh -lmpi_cxx -lmpi -lopen-rte -lopen-pal
#MVAPICH-- MPI_LIB       := -L$(dir $(shell which mpif90))../lib -lmpich -lmpichf90
MPI_LIB       := -L$(dir $(shell which mpif90))../lib -lmpi -lmpi++
LINK          := -lGIGC  $(MAPL_LIB) $(FV_LIB) $(ESMF_LIB) $(MPI_LIB)  $(LINK) -lGIGC 
FFLAGS        := -double-size 32 -real-size 32 -r4
#endif
# end - GEOS-Chem environment settings
