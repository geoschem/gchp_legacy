#
# System dependent FLAGS for FVdycoreCubed.
#

ifneq ($(ESMA_FC), pgfortran)
  FCNAME = $(word 1,$(shell $(FC) --version))
endif

# Intel Fortran Compiler
# ----------------------
ifeq ($(FCNAME),ifort)

  USER_FFLAGS = -stack_temps -safe_cray_ptr -assume byterecl \
                -fp-model source -ftz -align all -fno-alias -align dcommons
  
  # NOTE: This used to have -i_dynamic which would make a shared library. 
  #       But it has been obsoleted since Intel 13. We do not use shared
  #       libraries. 

endif

ifeq ($(FCNAME), GNU)  # gfortran

        USER_FFLAGS = -DNO_R16 -DNO_CRAY_POINTERS

endif

ifeq ($(ESMA_FC), gfortran)  # gfortran

        USER_FFLAGS = -DNO_R16 -fcray-pointer

endif

ifeq ($(ESMA_FC), ftn)
      USER_FFLAGS = -DNO_R16
endif

ifeq ($(ESMA_FC), pgfortran)
      USER_FFLAGS = -DNO_R16
endif
