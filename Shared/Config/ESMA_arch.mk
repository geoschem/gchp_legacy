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

  # Replace with compiler-specific code for GCHP (ewl, 1/23/19)
  #CFLAGS   += $(FPIC)
  #fFLAGS   += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
  #FFLAGS   += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
  #f90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
  #F90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)

  # -------------
  # F2PY Defaults
  # -------------

  ifdef ESMA_F2PY
     F2PY := $(ESMA_F2PY)
  endif

  ifdef ESMA_F2PY_FLAGS
     F2PY_FLAGS += $(ESMA_F2PY_FLAGS)
  endif

  # Other things needed for GCHP. Not sure if all of it needed. (ewl, 1/23/19)


  ###################
  # Intel compiler
  ###################
  ifeq ($(word 1,$(shell $(FC) --version)), ifort)

    CC  = icc
    CXX = g++
    CPP = cpp
    EXTENDED_SOURCE := -extend_source
    FREE_SOURCE := -free
    FIXED_SOURCE := -fixed
    OMPFLAG  := -openmp
    FPIC := -fPIC
    FREAL4 =
    FREAL8 = -r8
    BIG_ENDIAN := -convert big_endian
    BYTERECLEN := -assume byterecl
    FPE = -fpe0
    ALIGNCOM = -align dcommons
    FOPT2 += 

    ifeq ("$(BOPT)","g")
       FOPT = $(FOPTG) -O0 -ftz -traceback -debug -nolib-inline -check bounds -check uninit -fp-stack-check -ftrapuv
    else
       FOPT = $(FOPT3) -ftz -align all -fno-alias
    endif

    FC :=mpif90
    ifdef MPT_VERSION
        FC := mpif90
        INC_MPI := $(MPI_ROOT)/include
        LIB_MPI := -L$(MPI_ROOT)/lib  -lmpi -lmpi++
    endif
    LIB_SCI := 
    LIB_SYS := -ldl -lc -lpthread -lrt 

    # Version-specific
    IFORT_VER := $(subst ., ,$(word 3,$(shell ifort --version)))
    IFORT_MAJOR := $(word 1,$(IFORT_VER))
    IFORT_MINOR := $(word 2,$(IFORT_VER))
    ifeq ($(IFORT_MAJOR),15)
       FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias
       LIB_SYS := -lirc -ldl -lc -lpthread -lrt 
    endif

    # Set LIB_SCI
    ifdef MKLPATH
       ifeq ($(wildcard $(MKLPATH)/libmkl_intel_lp64.so),)
           LIB_SCI += -L$(MKLPATH) -lmkl_lapack -lmkl 
       else
           LIB_SCI += -L$(MKLPATH) -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
       endif
    else
    ifeq ($(MACH), ia64)
       LIB_SCI += -lscs 
    endif
    endif
    ifeq ($(MACH), i686)
          LIB_SCI += -llapack -lblas
    endif 
    
    GCC_DIR = $(shell dirname `gcc --print-libgcc-file-name`)
    ifeq ($(MACH), x86_64) 
       OVERRIDE_LIMITS =
       OMPFLAG =
       LOOP_VECT =
       FDEFS += $(D)HAVE_SHMEM
    else
    ifeq ($(MACH), ia64)
      OVERRIDE_LIMITS = -override_limits 
      FDEFS += $(D)HAVE_SHMEM
    endif # x86_64
    endif # ia64
    LIB_SYS += -L$(GCC_DIR) -lstdc++

    CFLAGS += $(FPIC)
    fFLAGS += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    FFLAGS += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    f90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    F90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)

  endif # ifort

  ####################
  # gfortran compiler
  ####################
  ifeq ($(word 1,$(shell $(FC) --version)), GNU)

      CC = gcc
      CXX = c++
      CPP = cpp
      EXTENDED_SOURCE := -ffixed-line-length-132
      FREE_SOURCE = 
      FIXED_SOURCE = -ffixed-form
      OMPFLAG = 
      FPIC := -fPIC
      FREAL4   := 
      FREAL8   := -fdefault-real-8 -fdefault-double-8
      CFLAGS   += $(FPIC)
      fFLAGS   += $(D)__GFORTRAN__ $(EXTENDED_SOURCE) $(FPIC)
      FFLAGS   += $(D)__GFORTRAN__ $(EXTENDED_SOURCE) $(FPIC)
      f90FLAGS += $(D)__GFORTRAN__ -ffree-line-length-none $(FPIC)
      F90FLAGS += $(D)__GFORTRAN__ -ffree-line-length-none $(FPIC)

      ifeq ("$(BOPT)","g")
         FOPT = $(FOPTG) -fbacktrace -fcheck=bounds,do,mem,pointer,recursion -ffpe-trap=invalid,overflow,underflow
      else
         FOPT = $(FOPT3) -falign-commons -funroll-loops
      endif

      ifdef MPT_VERSION
          FC := mpif90
          INC_MPI := $(MPI_ROOT)/include
          LIB_MPI := -L$(MPI_ROOT)/lib  -lmpi -lmpi++
      endif	
      LIB_SYS = -ldl -lc -lpthread -lrt -lstdc++

  endif # gfortran
