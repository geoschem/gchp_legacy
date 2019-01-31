  # -------------
  # Intel Fortran
  # -------------

#  ifeq ($(ESMA_FC), ifort)
#
#     ifdef ESMA_CC
#        CC := $(ESMA_CC)
#     endif
#
#     MKL_COMPILER = intel_f
#
#     F2PY += --fcompiler=intelem
#
#     # Determine compiler version
#     # --------------------------
#     IFORT_VER := $(subst ., ,$(word 3,$(shell ifort --version)))
#     IFORT_MAJOR := $(word 1,$(IFORT_VER))
#     IFORT_MINOR := $(word 2,$(IFORT_VER))
#
#     FPIC := -fPIC
#     EXTENDED_SOURCE := -extend_source
#     FREE_SOURCE := -free
#     FIXED_SOURCE := -fixed
#     OMPFLAG  := -qopenmp
#     PP  := -fpp
#     BIG_ENDIAN := -convert big_endian
#     BYTERECLEN := -assume byterecl
#     HEAPARRAYS = -heap-arrays 32
#     FPE = -fpe0 -fp-model source $(HEAPARRAYS) -assume noold_maxminloc
#     ALIGNCOM = -align dcommons
#     MCMODEL = -mcmodel medium  -shared-intel
#     FREAL4 =
#     FREAL8 = -r8
#
#     ifeq ("$(BOPT)","g")
#        FOPT = $(FOPTG) -O0 -ftz -align all -fno-alias -traceback -debug -nolib-inline -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -check bounds -check uninit -fp-stack-check -warn unused 
#        ifneq ($(IFORT_MAJOR),15)
#           FOPT += -init=snan,arrays
#        endif
#     else
#     ifeq ("$(BOPT)","fast")
#        -include $(ESMACFG)/vectorize.mk
#     else
#        FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias
#
#        ifneq ($(IFORT_MAJOR),15)
#           # Intel 17 has a new fp-model
#           #FPE = -fpe0 -fp-model consistent -heap-arrays 32
#
#           # Intel 16 seems to require -fimf-arch-consistency=true to allow zero-diff on Haswell and Sandy
#           FPE += -fimf-arch-consistency=true
#        endif
#     endif # BOPT=fast
#     endif # BOPT=g
#
#     # Always add traceback to FOPT on Intel
#     FOPT += -traceback
#
#     # F2003 compliance for ifort 16
#     ifneq ($(IFORT_MAJOR),15)
#        FOPT += -assume realloc_lhs
#     endif
#
#     ifeq ("$(BOPT)","Og")
#        FOPT += -g
#     endif
#
#     OVERRIDE_LIMITS =
#     LOOP_VECT =
#     FDEFS += $(D)HAVE_SHMEM
#
#     # Define LIB_SYS
#     # --------------
#     LIB_SYS := -lirc -ldl -lc -lpthread 
#     ifeq ($(ARCH),Linux)
#        LIB_SYS += -lrt
#     endif
#
#     USING_CLANG := $(word 1,$(shell $(CC) --version))
#     ifeq ("$(USING_CLANG)","Apple")
#        LIB_SYS += -lc++
#     else
#        GCC_DIR := $(shell dirname `gcc --print-libgcc-file-name`)
#        GXX_DIR := $(shell dirname `gcc -print-file-name=libstdc++.a`)
#
#        #LIB_SYS += -L$(GCC_DIR) -L$(GXX_DIR) -lstdc++
#        LIB_SYS += -L$(GCC_DIR) -L$(GXX_DIR) -lstdc++ -lgcc_eh
#     endif
#
#  endif # ifort


  ###################
  # Intel compiler - Custom for GCHP (ewl, 1/31/19)
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
