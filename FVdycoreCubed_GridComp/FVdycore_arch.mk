#
# System dependent FLAGS for FVdycore.
#


ifeq ($(ARCH),Linux)

  ifeq ($(FC), ifort)

#       USER_FFLAGS = -mp -stack_temps -fno-alias -ftz -auto
        USER_FFLAGS = -fp-model source

  endif

  ifeq ($(ESMA_FC), gfortran)

        USER_FFLAGS = -DNO_R16 -fcray-pointer

  endif

  ifeq ($(FC), pgfortran)
        USER_FFLAGS = -DNO_R16
  endif

endif
