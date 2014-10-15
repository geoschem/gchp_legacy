# $Id: makefile,v 1.1.5.1 2013-01-11 20:23:43 mathomp4 Exp $

ALL: build_here

SOURCEC	  = ESMCI_ArraySpec.C ESMC_ArraySpec.C
SOURCEF	  =  ESMF_ArraySpec_C.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  =  ESMCI_ArraySpec.h ESMC_ArraySpec.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/ArraySpec/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 