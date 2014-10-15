# $Id: makefile,v 1.1.5.1 2013-01-11 20:23:44 mathomp4 Exp $

ALL: build_here 

SOURCEC	  = ESMCI_State.C ESMC_State.C ESMCI_State_F.C 
SOURCEF	  =  ESMF_State_C.F90 
SOURCEH	  = 

# List .h files to be copied to higher level common include directory
STOREH	  = ESMCI_State.h  ESMC_State.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/State/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
