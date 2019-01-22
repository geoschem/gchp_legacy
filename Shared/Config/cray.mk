  # ---------------------------
  # Cray Fortran - UNMAINTAINED
  # ---------------------------

  ifeq ($(ESMA_FC),ftn)

    LIB_ESMF = $(BASELIB)/libesmf.a
    INC_MPI  = $(MPICH_DIR)/include
    LIB_MPI  = -L$(MPICH_DIR)/lib -lmpichf90

    LIB_SYS = -lstd -lrt -lC
    EXTENDED_SOURCE = -Mextend
    TARGET   = -target=linux
    FREAL4   = -r4
    FREAL8   = -r8
    FPE      = -Ktrap=divz,inv,ovf
    PP       = -Mpreprocess

    ifeq ("$(BOPT)","g")
#      FOPT   = $(FOPTG) -Ktrap=fp -Mbounds -Mchkptr
#      FOPT   = $(FOPTG) -O0 -Ktrap=fp -Mbounds
       FOPT   = $(FOPTG) -O0 -Ktrap=fp
    else
       FOPT   = -fast -Kieee
#      FOPT   = -fast -Mvect=nosse -Kieee
    endif
    BIG_ENDIAN =
    fFLAGS += $(EXTENDED_SOURCE) $(FPE) $(TARGET)
    FFLAGS += $(EXTENDED_SOURCE) $(FPE) $(TARGET)
    f90FLAGS += $(FPE) $(TARGET)
    F90FLAGS += $(FPE) $(TARGET)
    CFLAGS   += -DpgiFortran
    CXXFLAGS +=
    CC  := cc
    CXX := CC

  endif # Cray

