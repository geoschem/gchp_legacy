# $Id: makefile,v 1.1.5.1 2013-01-11 20:23:44 mathomp4 Exp $

ALL: build_here 

SOURCEC	  =  ESMCI_LocalArray.C 
SOURCEF	  =  

# if you have .h include files in this dir, list them all on the line below.
STOREH	  = ESMCI_LocalArray.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/LocalArray/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

