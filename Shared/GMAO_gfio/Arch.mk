#
# Architecture dependencies for MFHDF3
#

ifeq ($(ARCH),Linux)
	CC := gcc # does not work with icc
#	CDEFS += -DNAGf90Fortran
	CDEFS += -DpgiFortran $(I)$(INC_NETCDF)
endif

ifeq ($(ARCH),Darwin)
ifeq ($(ESMA_FC),g95)
	CDEFS += -Df2cFortran $(INC_NETCDF)
else
	CDEFS += -DpgiFortran $(INC_NETCDF)
endif
endif
