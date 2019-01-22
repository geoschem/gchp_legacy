#
# Earth System Modeling Applications (ESMA) PILGRIM makefile fragment.
# This fragment customize GNUmakefile for each architecture. 
#
# REVISION HISTORY:
#
# 04Aug2003  Zaslavsky Settings for IRIX64
# 13Jan2005  Todling   Commentted out SET_CPUS (no openMP stuff) on SGI.
# 24Feb2005  Kokron   Added include to MPI for MIPSpro.7.4.2.0 
#
#--------------------------------------------------------------------------


#                               ------
#                               IRIX64
#                               ------

ifeq ($(ARCH),IRIX64)

FOPT = $(FOPT3)
COPT = $(COPT2)
#USER_FFLAGS = $(D)SET_CPUS
USER_FFLAGS = -I$(INC_MPI) 
endif  #    IRIX64

ifeq ($(ARCH),Linux)

ifneq ("$(BOPT)","g")
   FOPT = -O2
   COPT = -O2
endif
USER_FDEFS = -DNO_TYPE_INIT
endif  #    Linux

ifeq ($(ARCH),Darwin)

ifneq ("$(BOPT)","g")
   FOPT = -O2
   COPT = -O2
endif
USER_FDEFS = -DNO_TYPE_INIT
endif  #    Darwin
