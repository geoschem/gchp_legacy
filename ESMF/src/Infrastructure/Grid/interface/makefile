# $Id: makefile,v 1.1.5.1 2013-01-11 20:23:44 mathomp4 Exp $ 

ALL: build_here 

SOURCEC	  = ESMCI_Grid_F.C
SOURCEF	  = ESMF_StaggerLoc.F90 ESMF_Grid.F90 
SOURCEH	  = 

# List all .h files which should be copied to common include dir 
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Grid/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 


