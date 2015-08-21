#
# System dependent FLAGS for FVdycoreCubed.
#

ifneq ($(ESMA_FC), pgfortran)
  FCNAME = $(word 1,$(shell $(FC) --version))
endif

# Intel Fortran Compiler
# ----------------------
ifeq ($(FCNAME),ifort)

  USER_FFLAGS = -safe_cray_ptr -assume byterecl -fp-model source

endif

ifeq ($(ESMA_FC), gfortran)  # gfortran

        USER_FFLAGS = -DNO_R16 -fcray-pointer -DNO_QUAD_PRECISION

endif

ifeq ($(ESMA_FC), ftn)
      USER_FFLAGS = -DNO_R16 -DNO_QUAD_PRECISION
endif

ifeq ($(ESMA_FC), pgfortran)
      USER_FFLAGS = -DNO_R16 -DNO_QUAD_PRECISION
endif
