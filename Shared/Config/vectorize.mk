#
# System dependent vectorization flags
#


  ifneq ("$(BOPT)", "g")

  ifeq ($(ESMA_FC), ifort)

     # ----
     # FOPT
     # ----

     # For vectorizing on Sandybridge and higher processors with AVX and AVX2 instructions
     # NOTE: No guarantee of zero-diff between *bridge and *well/*lake processors
     #FOPT := $(FOPT3) -axAVX,CORE-AVX2 -fma -qopt-report0 -ftz -align all -fno-alias -align array32byte

     # For vectorizing on Haswell and higher processors with AVX2 instructions
     FOPT := $(FOPT3) -xCORE-AVX2 -fma -qopt-report0 -ftz -align all -fno-alias -align array32byte

     # For vectorizing on Skylake and higher processors with CORE-AVX512 instructions
     #FOPT := $(FOPT3) -xCORE-AVX512 -qopt-zmm-usage=high -fma -qopt-report0 -ftz -align all -fno-alias -align array64byte

     # Add common FOPT flags
     FOPT += -traceback -assume realloc_lhs

     # ---
     # FPE
     # ---

     # For lower precision, but (usually) better performance from AVX instructions, enable this
     # Allows for MPI layout regression in testing
     FPE := -fpe3 -fp-model consistent -g -assume noold_maxminloc

     # For lower precision, but better performance from AVX instructions, enable this
     #FPE := -fpe3 -fp-model fast=2 -no-prec-div -g -assume noold_maxminloc

  endif

  ifeq ($(ESMA_FC),gfortran)

     # ----
     # FOPT
     # ----
     
     # NOTE: gfortran does get a benefit from vectorization, but the resulting code
     #       does not layout regress. Options kept here for testing purposes

     # Options per Jerry DeLisle on GCC Fortran List
     # These are the best found so far
     #FOPT := $(FOPT2) -march=native -ffast-math -ftree-vectorize -funroll-loops --param max-unroll-times=4 -mprefer-avx128 -mno-fma

     # Use SVML per list
     ##FOPT := $(FOPT2) -march=native -ffast-math -ftree-vectorize -funroll-loops --param max-unroll-times=4 -mprefer-avx128 -mno-fma -mveclibabi=svml
     ##LDFLAGS += -L/usr/local/intel/2018/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin -lsvml -mveclibabi=svml

     # ---
     # FPE
     # ---

     #FPE  := -g -fbacktrace

  endif

  endif
