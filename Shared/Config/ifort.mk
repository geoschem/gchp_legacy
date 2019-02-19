  # -------------
  # Intel Fortran
  # -------------

  # Change for GCHP
  #ifeq ($(ESMA_FC), ifort)
  ifeq ($(word 1,$(shell $(FC) --version)), ifort)
  #---

     # Remove for GCHP
     #ifdef ESMA_CC
     #   CC := $(ESMA_CC)
     #endif
     #---

     # Add for GCHP
     CC  = icc
     CXX = g++
     CPP = cpp
     #---

     MKL_COMPILER = intel_f

     F2PY += --fcompiler=intelem

     # Determine compiler version
     # --------------------------
     IFORT_VER := $(subst ., ,$(word 3,$(shell ifort --version)))
     IFORT_MAJOR := $(word 1,$(IFORT_VER))
     IFORT_MINOR := $(word 2,$(IFORT_VER))

     FPIC := -fPIC
     EXTENDED_SOURCE := -extend_source
     FREE_SOURCE := -free
     FIXED_SOURCE := -fixed

     # Change for GCHP
     #OMPFLAG  := -qopenmp
     OMPFLAG  := -openmp
     #---

     # Remove for GCHP
     #PP  := -fpp
     #---

     BIG_ENDIAN := -convert big_endian
     BYTERECLEN := -assume byterecl

     # Remove for GCHP
     #HEAPARRAYS = -heap-arrays 32
     #---

     # Change for GCHP
     #FPE = -fpe0 -fp-model source $(HEAPARRAYS) -assume noold_maxminloc
     FPE = -fpe0
     #---

     ALIGNCOM = -align dcommons

     # Remove for GCHP
     #MCMODEL = -mcmodel medium  -shared-intel
     #---

     FREAL4 =
     FREAL8 = -r8

     # Change for GCHP
     #ifeq ("$(BOPT)","g")
     #   FOPT = $(FOPTG) -O0 -ftz -align all -fno-alias -traceback -debug -nolib-inline -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -check bounds -check uninit -fp-stack-check -warn unused 
     #   ifneq ($(IFORT_MAJOR),15)
     #      FOPT += -init=snan,arrays
     #   endif
     #else
     #ifeq ("$(BOPT)","fast")
     #   -include $(ESMACFG)/vectorize.mk
     #else
     #   FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias
     #
     #   ifneq ($(IFORT_MAJOR),15)
     #      # Intel 17 has a new fp-model
     #      #FPE = -fpe0 -fp-model consistent -heap-arrays 32
     #
     #      # Intel 16 seems to require -fimf-arch-consistency=true to allow zero-diff on Haswell and Sandy
     #      FPE += -fimf-arch-consistency=true
     #   endif
     #endif # BOPT=fast
     #endif # BOPT=g
     #
     ## Always add traceback to FOPT on Intel
     #FOPT += -traceback
     #
     ## F2003 compliance for ifort 16
     #ifneq ($(IFORT_MAJOR),15)
     #   FOPT += -assume realloc_lhs
     #endif
     #
     #ifeq ("$(BOPT)","Og")
     #   FOPT += -g
     #endif
     FOPT2 += 
     ifeq ("$(BOPT)","g")
        FOPT = $(FOPTG) -O0 -ftz -traceback -debug -nolib-inline -check bounds -check uninit -fp-stack-check -ftrapuv
     else
        FOPT = $(FOPT3) -ftz -align all -fno-alias
     endif
     ifeq ($(IFORT_MAJOR),15)
        FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias
     endif
     #---

     # Change for GCHP
     #OVERRIDE_LIMITS =
     #LOOP_VECT =
     #FDEFS += $(D)HAVE_SHMEM
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
     #---

     # Define LIB_SYS
     # --------------

     # Change for GCHP
     #LIB_SYS := -lirc -ldl -lc -lpthread 
     #ifeq ($(ARCH),Linux)
     #   LIB_SYS += -lrt
     #endif
     #
     #USING_CLANG := $(word 1,$(shell $(CC) --version))
     #ifeq ("$(USING_CLANG)","Apple")
     #   LIB_SYS += -lc++
     #else
     #   GCC_DIR := $(shell dirname `gcc --print-libgcc-file-name`)
     #   GXX_DIR := $(shell dirname `gcc -print-file-name=libstdc++.a`)
     #
     #   #LIB_SYS += -L$(GCC_DIR) -L$(GXX_DIR) -lstdc++
     #   LIB_SYS += -L$(GCC_DIR) -L$(GXX_DIR) -lstdc++ -lgcc_eh
     #endif
     LIB_SYS := -ldl -lc -lpthread -lrt 
     ifeq ($(IFORT_MAJOR),15)
        LIB_SYS := -lirc -ldl -lc -lpthread -lrt 
     endif
     GCC_DIR = $(shell dirname `gcc --print-libgcc-file-name`)
     LIB_SYS += -L$(GCC_DIR) -lstdc++
     #---

     # Add for GCHP
     fFLAGS += $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
     FFLAGS += $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
     f90FLAGS += $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
     F90FLAGS += $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
     #---

  endif # ifort
