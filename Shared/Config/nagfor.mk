  # -----------
  # NAG Fortran
  # -----------

  ifeq ($(ESMA_FC), nagfor)

     # Not sure for this compiler????
     MKL_COMPILER = gnu_f

     EXTENDED_SOURCE := -132
     FREE_SOURCE = 
     FIXED_SOURCE = -fixed
     FREAL4   := 
     FREAL8   := -r8
     FINT8    := -i8

     # This is needed for m_fpe.F90
     #NO_RANGE_CHECK   := -fno-range-check 
     CPIC   := -fPIC
     FPIC   := -PIC

     # For some reason this does not work at the moment.
     #BIG_ENDIAN := -convert=BIG_ENDIAN
     #FPE = -ffpe-trap=zero,overflow -fbacktrace
     FPE := -dusty -mismatch_all

     #ALIGNCOM = -falign-commons
     #BYTERECLEN = -frecord-marker=4

     OMPFLAG = -openmp
     PP = -fpp

     ifeq ("$(BOPT)","g")
        FOPT = -O0 -g
     else
     ifeq ("$(BOPT)","Og")
        FOPT = -O3 -g
     else
        FOPT = $(FOPT3)
     endif
     endif

     # Suggested by Tom
     #FOPT += -dusty

  endif # nagfor

