#
# Earth System Modeling Applications (ESMA) base makefile fragment.
# This fragment costumizes ESMF_base.mk for each each architecture. 
#
# REVISION HISTORY:
# 20 Nov 2014 - R. Yantosca - Remove hardwiring for netCDF, MPI
# 20 Nov 2014 - R. Yantosca - Simplify the IF statement for FOPT3
#
#--------------------------------------------------------------------------

#                             ---------------------
#                             User defined defaults
#                             ---------------------
  ifdef ESMA_FC
	FC := $(ESMA_FC)
  endif

#                               -----
#                               Linux
#                               -----

ifeq ($(ARCH),Linux)

# Linux default compilers
# -----------------------
  ifndef ESMA_FC
     FC := ifort
  endif
  CC  = cc
  CXX = c++
  CPP = cpp

#
#                    Linux Compiler Specific
#                    -----------------------

# ------------------------------------------
  ifeq ($(word 1,$(shell $(FC) --version)), ifort)

#   Determine compiler version
#   --------------------------
    IFORT_VER := $(subst ., ,$(word 3,$(shell ifort --version)))
    IFORT_MAJOR := $(word 1,$(IFORT_VER))
    IFORT_MINOR := $(word 2,$(IFORT_VER))
    FPIC := -fPIC
    EXTENDED_SOURCE := -extend_source
    FREE_SOURCE := -free
    FIXED_SOURCE := -fixed
    MPFLAG  := #-mp
    OMPFLAG  := -openmp
    BIG_ENDIAN := -convert big_endian
    BYTERECLEN := -assume byterecl
    FPE = -fpe0
    ALIGNCOM = -align dcommons
    FREAL4 =
    FREAL8 = -r8
    FOPT2 += 
###############################################################################
# %%%%% COMMENTED OUT BY BOB Y. (11/20/14) %%%%%
#    ifeq ("$(BOPT)","g")
#       FOPT = $(FOPTG) -O0 -ftz -traceback -debug -nolib-inline -check bounds -check uninit -fp-stack-check -ftrapuv
#    else
#       ifeq ($(IFORT_MAJOR), 8)
#          FOPT = $(FOPT3)
#       else
#       ifeq ($(IFORT_MAJOR), 9)
#          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias
##          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias -fp-model precise
#       else
#       ifeq ($(IFORT_MAJOR),10)
#          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias
##          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias -fp-model precise -assume protect_parens
##          FOPT = $(FOPT3) -vec-report0 -align all -fno-alias -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -no-ftz 
#       else
#       ifeq ($(IFORT_MAJOR),11)
#          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias
##          FOPT = $(FOPT3) -xSSE4.1 -vec-report0 -ftz -align all -fno-alias
#       else
#          FOPT = $(FOPT3)
#       endif
#       endif
#       endif
#       endif
#    endif
###############################################################################
#------------------------------------------------------------------------------
# %%%%% ADDED BY BOB Y. (11/20/14) %%%%%
#
# Radically Simplify the IF statement.  All of the IFORT versions used 
# the same flags, so we really don't need all of these if/else statements.
    ifeq ("$(BOPT)","g")
       FOPT = $(FOPTG) -O0 -ftz -traceback -debug -nolib-inline -check bounds -check uninit -fp-stack-check -ftrapuv
    else
       FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias
    endif
#------------------------------------------------------------------------------

#    LIB_ESMF = $(BASELIB)/libesmf.so

    CC  = icc
    CXX = g++

###############################################################################
# %%%%% COMMENTED OUT BY BOB Y. (11/20/14) %%%%%
#
# Remove hardwiring; replace with portable declaration from OpenMPI.
##   Default MPI on i686
##   -------------------
#    ifeq ($(MACH), i686)
#      FC := mpif90
#      INC_MPI := $(dir $(shell which mpif90))../include
#      LIB_MPI := -L$(dir $(shell which mpif90))../lib -lmpi -lmpi_cxx -lmpi_f77
#    endif
#
##   Handle MPI on x86_64
##   --------------------
#    ifdef I_MPI_ROOT
#        FC := mpiifort
#        ifeq ($(MACH), x86_64) 
#          INC_MPI := $(I_MPI_ROOT)/include64
#          LIB_MPI := -L$(I_MPI_ROOT)/lib64  -lmpi -lmpiif # Intel MPI
#        else
#          INC_MPI := $(I_MPI_ROOT)/include
#          LIB_MPI := -L$(I_MPI_ROOT)/lib  -lmpi -lmpiif # Intel MPI
#        endif
#    else
#    ifdef M_MPI_ROOT
#        FC := mpiifort
#        INC_MPI := $(M_MPI_ROOT)/include
#        LIB_MPI := -L$(M_MPI_ROOT)/lib  -lmpich
#    else
#    ifdef MPI_HOME
#        FC := mpif90
#        INC_MPI := $(MPI_HOME)/include
#        LIB_MPI := -L$(MPI_HOME)/lib  -lmpi -lmpi_cxx -lmpi_f77 #-lmpich
#    else
#    ifdef MVAPICH2
#        FC := mpif90
#        INC_MPI := $(MVAPICH2)/include
#        LIB_MPI := -L$(MVAPICH2)/lib  -lmpich
#    else
#    ifdef FPATH
#        FPATHS := $(subst :, ,$(FPATH))
#        ifeq ($(MACH), x86_64) 
#          INC_MPI := $(filter /nasa/sgi/mpt%,$(FPATHS)) \
#                     $(filter /opt/scali%,$(FPATHS))
#          INC_MPI := $(word 1,$(INC_MPI))
#          LIB_MPI := -L$(subst include,lib64,$(INC_MPI)) -lmpi -lmpi++
#        endif
#        ifeq ($(MACH), ia64)
#          INC_MPI := $(filter /opt/sgi/mpt%,$(FPATHS)) \
#                     $(filter /nasa/sgi/mpt%,$(FPATHS)) 
#          INC_MPI := $(word 1,$(INC_MPI))
#          LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpi -lmpi++
#         endif
#    else 
#    endif
#    endif
#    endif
#    endif
#    endif
###############################################################################
#------------------------------------------------------------------------------
# %%%%% ADDED BY BOB Y. (11/20/14) %%%%%
#
# Now query the proper MPI settings
FC      :=mpif90
    ifdef MPT_VERSION
        FC := mpif90
        INC_MPI := $(MPI_ROOT)/include
        LIB_MPI := -L$(MPI_ROOT)/lib  -lmpi -lmpi++
    endif	
#INC_MPI := $(shell mpif90 --showme:incdirs)
#LIB_MPI := $(shell mpif90 --showme:link)
#LIB_MPI += $(shell mpicxx --showme:link)
#------------------------------------------------------------------------------

#   Define LIB_SYS
#   --------------
    LIB_SCI := 
    LIB_SYS := -ldl -lc -lpthread -lrt 

    ifeq ($(IFORT_MAJOR), 10)
          LIB_SYS := -lirc -lguide $(LIB_SYS)
          ifneq ($(MACH), i686)
              FPE := -fp-model precise
              MPFLAG :=# -mp is incompatible with the -fp-model option
          endif
    else
    ifeq ($(IFORT_MAJOR), 11)
          LIB_SYS := -lirc -lguide $(LIB_SYS)
          ifneq ($(MACH), i686)
              FPE += -fp-model precise
              MPFLAG :=# -mp is incompatible with the -fp-model option
          endif
    else
#------------------------------------------------------------------------------
# %%%%% ADDED BY BOB Y. (11/20/14) %%%%%
#
# Extend for IFORT 12 and IFORT 13 
    ifeq ($(IFORT_MAJOR), 12)
          LIB_SYS := -lirc -liomp5 $(LIB_SYS)
          ifneq ($(MACH), i686)
              FPE += -fp-model precise
              MPFLAG :=# -mp is incompatible with the -fp-model option
          endif
    else
    ifeq ($(IFORT_MAJOR), 13)
          LIB_SYS := -lirc -liomp5 $(LIB_SYS)
          ifneq ($(MACH), i686)
              FPE += -fp-model precise
              MPFLAG :=# -mp is incompatible with the -fp-model option
          endif
#------------------------------------------------------------------------------
    else
    ifeq ($(IFORT_MAJOR),15)
          FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias
#alt: cprts library conflicts with ESMF4
     LIB_SYS := -lirc -ldl -lc -lpthread -lrt 
#         LIB_SYS +=  -lunwind #-lcprts
    endif
    endif
    endif
#------------------------------------------------------------------------------
# %%%%% ADDED BY BOB Y. (11/20/14) %%%%%
#
# Need to add 2 more endif's to balance out the extra ifeq's above.
    endif
    endif
#------------------------------------------------------------------------------


#   MKL math library
#   ----------------
#    ifeq ($(wildcard $(ESMABIN)/mklpath.pl),$(ESMABIN)/mklpath.pl)
#       MKLPATH = $(shell $(ESMABIN)/mklpath.pl)
#    endif
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

#   Customize for each MACH
#   -----------------------
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

##############################################################################
# %%%%% COMMENTED OUT BY BOB Y. (11/20/14) %%%%%
#
# Comment this section out, it's not necessary, as we define
# both INC_MPI and LIB_MPI above.  
##   Some safeguards
##   ---------------
#    ifeq ($(INC_MPI),)
#      FC := mpif90
#      INC_MPI := $(dir $(shell which mpif90))../include
#      LIB_MPI := -L$(dir $(shell which mpif90))../lib -lmpi -lmpi_cxx -lmpi_f77
#    endif
##############################################################################

  endif

# GNU Fortran Compiler
# --------------------
  ifeq ($(word 1,$(shell $(FC) --version)), GNU)

      CC = gcc

      #LIB_ESMF = $(BASELIB)/libesmf.a

      EXTENDED_SOURCE := -ffixed-line-length-132
      FREE_SOURCE = 
      FIXED_SOURCE = -ffixed-form
      FREAL4   := 
      FREAL8   := -fdefault-real-8 -fdefault-double-8

      OMPFLAG = 

      FPIC := -fPIC

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
      #INC_MPI = /usr/include
      #LIB_MPI = -lmpi

#      LIB_SCI  = -llapackmt -lblasmt 
      LIB_SYS = -ldl -lc -lpthread -lrt -lstdc++

  endif

endif  #    Linux
