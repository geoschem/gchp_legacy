# $Id: makefile,v 1.1.5.1 2013-01-11 20:23:43 mathomp4 Exp $

ALL: build_here

SOURCEC	  = ESMCI_ArrayBundle_F.C
SOURCEF	  = ESMF_ArrayBundle.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/ArrayBundle/interface

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 
