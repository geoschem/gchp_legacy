#
# Earth System Modeling Applications (ESMA) base makefile fragment.
# This fragment customizes ESMF_base.mk for each each architecture. 
#
#--------------------------------------------------------------------------

  ##################################
  #                                #
  # Required Environment Variables #
  #                                #
  ##################################

  ifndef BASEDIR
     $(error BASEDIR is undefined)
  endif

  ifndef ESMADIR
     $(error ESMADIR is undefined)
  endif

  #########################
  #                       #
  # User defined defaults #
  #                       #
  #########################

  # -----------
  # FC Defaults
  # -----------

  # The code triggers off ESMA_FC. 
  # The default is ifort for now.

  ifdef ESMA_FC
     FC := $(ESMA_FC)
  endif

  ifndef ESMA_FC
     ESMA_FC := ifort
  endif

  # -------------
  # BOPT Defaults
  # -------------

  ifdef ESMA_DEVEL_BOPT
     ifeq ($(ESMA_DEVEL_BOPT),Og)
        BOPT = Og
     endif
     ifeq ($(ESMA_DEVEL_BOPT),g)
        BOPT = g
     endif
  endif

  ################################
  #                              #
  #    Arch Specific Defaults    #
  #                              #
  ################################

  -include $(ESMACFG)/linux.mk

  -include $(ESMACFG)/darwin.mk

  #####################
  #                   #
  # Compiler Defaults #
  #                   #
  #####################

  -include $(ESMACFG)/compiler.mk

  ######################
  #                    #
  #    MPI Defaults    #
  #                    #
  ######################

  -include $(ESMACFG)/mpi.mk

  ############################
  #                          #
  # Math Library (MKL, BLAS) #
  #                          #
  ############################

  -include $(ESMACFG)/math.mk

  ######################
  #                    #
  #    TAU Profiler    #
  #                    #
  ######################

  -include $(ESMACFG)/tau.mk

  #########################
  #                       #
  # Default Flags Recipes #
  #                       #
  #########################

  FULLFC    = $(shell which $(FC))

  # Customize for GCHP (ewl, 1/31/19)
  #CFLAGS   += $(FPIC)
  #fFLAGS   += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
  #FFLAGS   += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
  #f90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
  #F90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
  CFLAGS   += $(FPIC)
  fFLAGS   += $(EXTENDED_SOURCE) $(FPIC)
  FFLAGS   += $(EXTENDED_SOURCE) $(FPIC)
  f90FLAGS += -ffree-line-length-none $(FPIC)
  F90FLAGS += -ffree-line-length-none $(FPIC)

  # -------------
  # F2PY Defaults
  # -------------

  ifdef ESMA_F2PY
     F2PY := $(ESMA_F2PY)
  endif

  ifdef ESMA_F2PY_FLAGS
     F2PY_FLAGS += $(ESMA_F2PY_FLAGS)
  endif
