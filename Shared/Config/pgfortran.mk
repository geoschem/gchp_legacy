  # -----------
  # PGI Fortran
  # -----------

  ifeq ($(ESMA_FC), pgfortran)
     MKL_COMPILER = pgi_f

     # Determine compiler version
     # --------------------------
     PGI_VER := $(subst -, ,$(subst ., ,$(word 2,$(shell pgfortran --version | sed 1d))))
     PGI_MAJOR := $(word 1,$(PGI_VER))
     PGI_MINOR := $(word 2,$(PGI_VER))

     FREE_SOURCE = -Mfree
     FIXED_SOURCE = -Mfixed
     EXTENDED_SOURCE = -Mextend
     LIB_ESMF = $(BASELIB)/libesmf.a
     FREAL4 = 
     FREAL8 = -r8
     FPE = -Ktrap=fp #Equiv to -Ktrap=divz,inv,ovf
     FPIC = -fpic
     BACKSLASH_STRING = -Mbackslash
     BIG_ENDIAN = -Mbyteswapio

     OMPFLAG  := -mp

     INC_PGI := $(dir $(shell which pgfortran))../include

     ifeq ("$(BOPT)","g")
        GPU_TARGET :=
        FOPT = -O0 -g -Kieee -Minfo=all -Mbounds -traceback -Mchkfpstk -Mchkstk -Mdepchk $(GPU_TARGET)
     else
     ifeq ("$(BOPT)","GPU")

        # Note here we specifically compile to the target hosting the GPU. GPUs cannot support unified
        # binaries, so do not even try.

        ifeq ($(PGI_MAJOR),15)
           GPU_CUDA_VER := 6.5
        endif
        ifeq ($(PGI_MAJOR),16)
           GPU_CUDA_VER := 7.0
        endif
        ifeq ($(PGI_MAJOR),17)
           GPU_CUDA_VER := 7.5
        endif
        ifeq ($(PGI_MAJOR),18)
           GPU_CUDA_VER := 9.0
        endif

        ifndef GPU_CC_REV
           GPU_CC_REV := cc35
        endif

        GPU_TARGET := -Mcuda=nofma,ptxinfo,$(GPU_CUDA_VER),$(GPU_CC_REV),maxregcount:72 -acc -ta=nvidia:wait,nofma,$(GPU_CUDA_VER),$(GPU_CC_REV),maxregcount:72 -Minfo=accel,ccff

        FOPT = -fast -Kieee $(GPU_TARGET)
        USER_FDEFS += $(D)GPU_MAXLEVS=72        # Select max level for GPU Code (could save space with this)
        USER_FDEFS += $(D)_CUDA                 # Set this always so the GEOS-5 dependency builder can use it.
     else
        GPU_TARGET :=
        FOPT = -fast -Kieee -g
     endif
     endif

     # PGI autoselects the optimization target to the be host currently compiling
     # thus if you compile on a Sandy Bridge (=sandybridge-64) node, you will
     # generate code that cannot run on a Westmere (=nehalem). For safety's sake, 
     # we default to a generic target processor (=px-64). This allows for good layout
     # regression as well as reproducible results. Note: speed does not seem to be
     # affected by this, but a specific target can always be selected.
     #
     # Note: We append this to FPE since not every file obeys FOPT. But nearly all
     #       obey FPE
     FPE += -tp=px-64

     ifeq ("$(BOPT)","Og")
        FOPT += -g -traceback
     endif

     fFLAGS   += $(BACKSLASH_STRING)
     FFLAGS   += $(BACKSLASH_STRING)
     f90FLAGS += $(BACKSLASH_STRING)
     F90FLAGS += $(BACKSLASH_STRING)

     CFLAGS += $(FPIC) -DpgiFortran
     PP = -Mpreprocess

     ifeq ($(PGI_MAJOR),15)
        LDFLAGS += -pgcpplibs -tp=px-64
        LIB_SYS = -ldl -lstd -lC $(GPU_TARGET)
     else
        LDFLAGS += -pgc++libs -tp=px-64
        LIB_SYS = -ldl $(GPU_TARGET)
     endif

     ifeq ($(ARCH),Linux) 
        LIB_SYS += -lrt
     endif

  endif # pgfortran

