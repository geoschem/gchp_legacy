  ############################
  #                          #
  # Math Library (MKL, BLAS) #
  #                          #
  ############################

  ifeq ($(wildcard $(ESMABIN)/mklpath.pl),$(ESMABIN)/mklpath.pl)
      MKLPATH := $(shell $(ESMABIN)/mklpath.pl)
  endif

  # On Darwin, there does not exist MKL for gfortran. Use Framework
  ifeq ($(ARCH),Darwin)
     ifeq ($(ESMA_FC),gfortran)
        LIB_SCI = -framework Accelerate
     endif
  else

  # If using ifort, just use -mkl
  ifeq ($(ESMA_FC), ifort)
     LIB_SCI = -mkl=sequential
  else

  ifdef MKLPATH
     ifeq ($(ESMA_FC), gfortran)
        LIB_SCI = -L$(MKLPATH) -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm
     else
     ifeq ($(ESMA_FC), pgfortran)
        LIB_SCI = -L$(MKLPATH) -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl
     endif
     endif
  else

  ifdef MKLROOT
     ifeq ($(wildcard $(MKLROOT)/tools/mkl_link_tool),$(MKLROOT)/tools/mkl_link_tool)
        LIB_SCI = $(shell $(MKLROOT)/tools/mkl_link_tool -libs --os=$(MKL_OS) --compiler=$(MKL_COMPILER) --parallel=no -check_mkl_presence)
     endif
  else

  # This is a failover if MKL does not exist
  # We assume if on macOS, we have Accelerate...
  ifeq ($(ARCH),Darwin)
     LIB_SCI = -framework Accelerate
  # ...or we assume BLAS and LAPACK are installed...somewhere
  else
     LIB_SCI = -lblas -llapack

  endif # Failover
  endif # MKLPATH
  endif # MKLROOT
  endif # ifort
  endif # Darwin gfortran

