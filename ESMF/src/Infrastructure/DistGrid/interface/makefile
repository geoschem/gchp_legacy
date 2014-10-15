# $Id: makefile,v 1.1.5.1 2013-01-11 20:23:44 mathomp4 Exp $

ALL: build_here

SOURCEC	  = ESMCI_DistGrid_F.C ESMC_DistGrid.C
SOURCEF	  = ESMF_DistGridConnection.F90 ESMF_DistGrid.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = ESMC_DistGrid.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/DistGrid/interface

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 
