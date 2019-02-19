  # -------------
  # Intel Fortran
  # -------------

  ifeq ($(ESMA_FC), ifort)

     ifdef ESMA_CC
        CC := $(ESMA_CC)
     endif

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
     OMPFLAG  := -qopenmp
     PP  := -fpp
     BIG_ENDIAN := -convert big_endian
     BYTERECLEN := -assume byterecl
     HEAPARRAYS = -heap-arrays 32
     FPE = -fpe0 -fp-model source $(HEAPARRAYS) -assume noold_maxminloc
     ALIGNCOM = -align dcommons
     MCMODEL = -mcmodel medium  -shared-intel
     FREAL4 =
     FREAL8 = -r8

     ifeq ("$(BOPT)","g")
        FOPT = $(FOPTG) -O0 -ftz -align all -fno-alias -traceback -debug -nolib-inline -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -check bounds -check uninit -fp-stack-check -warn unused 
        ifneq ($(IFORT_MAJOR),15)
           FOPT += -init=snan,arrays
        endif
     else
     ifeq ("$(BOPT)","fast")
        -include $(ESMACFG)/vectorize.mk
     else
        FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias

        ifneq ($(IFORT_MAJOR),15)
           # Intel 17 has a new fp-model
           #FPE = -fpe0 -fp-model consistent -heap-arrays 32

           # Intel 16 seems to require -fimf-arch-consistency=true to allow zero-diff on Haswell and Sandy
           FPE += -fimf-arch-consistency=true
        endif
     endif # BOPT=fast
     endif # BOPT=g

     # Always add traceback to FOPT on Intel
     FOPT += -traceback

     # F2003 compliance for ifort 16
     ifneq ($(IFORT_MAJOR),15)
        FOPT += -assume realloc_lhs
     endif

     ifeq ("$(BOPT)","Og")
        FOPT += -g
     endif

     OVERRIDE_LIMITS =
     LOOP_VECT =
     FDEFS += $(D)HAVE_SHMEM

     # Define LIB_SYS
     # --------------
     LIB_SYS := -lirc -ldl -lc -lpthread 
     ifeq ($(ARCH),Linux)
        LIB_SYS += -lrt
     endif

     USING_CLANG := $(word 1,$(shell $(CC) --version))
     ifeq ("$(USING_CLANG)","Apple")
        LIB_SYS += -lc++
     else
        GCC_DIR := $(shell dirname `gcc --print-libgcc-file-name`)
        GXX_DIR := $(shell dirname `gcc -print-file-name=libstdc++.a`)

        #LIB_SYS += -L$(GCC_DIR) -L$(GXX_DIR) -lstdc++
        LIB_SYS += -L$(GCC_DIR) -L$(GXX_DIR) -lstdc++ -lgcc_eh
     endif

  endif # ifort
