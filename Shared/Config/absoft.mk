  # ------------------------------
  # Absoft compiler - UNMAINTAINED
  # ------------------------------
  
  ifeq ($(ESMA_FC), absoft) 

     # Not sure for this compiler????
     MKL_COMPILER = gnu_f

     EXTENDED_SOURCE = -W 132
     FREE_SOURCE = -f free
     FIXED_SOURCE = -f fixed
     LIB_ESMF = $(BASELIB)/esmf/libesmf.a #$(BASELIB)/esmf/libnetcdf_stubs.a
     LIB_MPI = -L$(BASELIB)/mpi -lpmpich++ -lfmpich -lmpich -lmpichfsup
     LIB_SYS = -lstdc++ -lpthread -lU77 -lrt
     FREAL4 =
     FREAL8 = -N113
     M = -p
     FINCS += -I$(BASEINC)/mpi
     FDEFS += -DABSOFT -DNO_R16 
     XFLAGS += -YEXT_NAMES=LCS -YEXT_SFX=_ $(EXTENDED_SOURCE)
     FOPT   = -g -trap=INVALID,DIVBYZERO,OVERFLOW

  endif # absoft

