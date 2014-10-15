# $Id: makefile,v 1.1.5.1 2013-01-11 20:23:43 mathomp4 Exp $

ALL: build_here 

# on the Cray X1 default template instantiation is a pain
ifeq ($(ESMF_OS),Unicos)
ifeq ($(ESMF_COMPILER),default)
ESMF_CXXCOMPILEOPTS += -h simple_templates
endif
endif

SOURCEC	  = ESMCI_Array.C
SOURCEF	  = 
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = ESMCI_Array.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Array/src

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 

