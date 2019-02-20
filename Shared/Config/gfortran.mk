  # -----------
  # GNU Fortran
  # -----------

  # Changed for GCHP
  #ifeq ($(ESMA_FC), gfortran)
  ifeq ($(word 1,$(shell $(FC) --version)), GNU)
  #---

     # Added for GCHP
     CC = gcc
     CXX = c++
     CPP = cpp
     #---

     MKL_COMPILER = gnu_f

     EXTENDED_SOURCE := -ffixed-line-length-132
     FREE_SOURCE = 
     FIXED_SOURCE = -ffixed-form
     FREAL4   := 
     FREAL8   := -fdefault-real-8 -fdefault-double-8
     FINT8    := -fdefault-integer-8
     # This is needed for m_fpe.F90
     NO_RANGE_CHECK   := -fno-range-check 
     FPIC   := -fPIC
     # For some reason this does not work at the moment.
     #BIG_ENDIAN := -fconvert=swap
     FPE = -ffpe-trap=zero,overflow -fbacktrace
     ALIGNCOM = -falign-commons
     BYTERECLEN = -frecord-marker=4

     # Changed for GCHP
     #OMPFLAG = -fopenmp
     OMPFLAG =
     #---

     PP = -cpp

     ifeq ("$(BOPT)","g")

        # Changed for GCHP
        #FOPT = -O0 -g -fcheck=all,no-array-temps -finit-real=snan
        FOPT = $(FOPTG) -fbacktrace -fcheck=bounds,do,mem,pointer,recursion -ffpe-trap=invalid,overflow,underflow
        #---

     else
     ifeq ("$(BOPT)","Og")
        FOPT = -Og -g
     else
     ifeq ("$(BOPT)","fast")
        -include $(ESMACFG)/vectorize.mk
     else
        #FOPT = $(FOPT3)
        #FOPT = $(FOPT3) -march=native -funroll-loops
        #FOPT = $(FOPT3) -march=native -funroll-loops -ffast-math

        # Changed for GCHP
        #FOPT = $(FOPT3) -march=westmere -mtune=generic -funroll-loops -g
        FOPT = $(FOPT3) -falign-commons -funroll-loops -fcray-pointer
        #---

     endif
     endif
     endif

     # Suggested by ESMF
     # This is broken in GCC 8.1. For now just do not use
     #FOPT += -fcoarray=single

     CFLAGS   += -Wno-missing-include-dirs
     fFLAGS   += $(D)__GFORTRAN__ $(NO_RANGE_CHECK) -Wno-missing-include-dirs
     FFLAGS   += $(D)__GFORTRAN__ $(NO_RANGE_CHECK) -Wno-missing-include-dirs
     f90FLAGS += $(D)__GFORTRAN__ -ffree-line-length-none $(NO_RANGE_CHECK) -Wno-missing-include-dirs
     F90FLAGS += $(D)__GFORTRAN__ -ffree-line-length-none $(NO_RANGE_CHECK) -Wno-missing-include-dirs

     # Define LIB_SYS
     # --------------

     LIB_SYS := -ldl -lc -lpthread
     ifeq ($(ARCH),Linux)
        LIB_SYS += -lrt
     endif

     USING_CLANG := $(word 1,$(shell $(CC) --version))
     ifeq ("$(USING_CLANG)","Apple")
        LIB_SYS += -lc++
     else
        GCC_DIR := $(shell dirname `gcc --print-libgcc-file-name`)
        GXX_DIR := $(shell dirname `gcc -print-file-name=libstdc++.a`)

        LIB_SYS += -L$(GCC_DIR) -L$(GXX_DIR) -lstdc++ -lgcc_eh
     endif

  endif # gfortran
