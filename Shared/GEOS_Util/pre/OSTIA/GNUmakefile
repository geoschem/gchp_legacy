SHELL=/bin/sh

#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  08Jan2013  jstassi  Created with gmm script.
#
#.......................................................................

#---------------------------------------------------
# Get GNUmakefile name
# note: MAKEFILE_LIST defined for gmake versions >= 3.80
#---------------------------------------------------
ifdef MAKEFILE_LIST
   GNUMAKEFILE := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
else
   GNUMAKEFILE := GNUmakefile
endif

#------------------------------
# Make sure ESMADIR is defined
# -----------------------------
ifndef ESMADIR
   ESMADIR := $(PWD)/../../..
endif

#-------------------------------
# Compilation rules, flags, etc
#-------------------------------
include $(ESMADIR)/Config/ESMA_base.mk   # Generic stuff
include $(ESMADIR)/Config/ESMA_arch.mk   # System dependencies
include $(ESMADIR)/Config/GMAO_base.mk   # System dependencies

########################################################################

INC_LIST = $(INC_NETCDF)
LIB_LIST = $(LIB_NETCDF)

########################################################################

BINS = make_ostia_bcs.x

UT = proc_SST.x proc_SST_ICE.x

########################################################################

INCFILES = $(wildcard *.[Hh]) $(wildcard *.[Mm][Oo][Dd])

NMLFILES =

PERLFILES = $(wildcard *.pl) $(wildcard *.pm)

RCFILES =

SHELLSCRIPTS = $(wildcard *.csh)

SEDCMD = s^\@DASPERL^\$(PERL)^;s^\@DASSED^\$(SED)^;s^\@DASHELL^\$(SHELL)^


########################################################################

ALLDIRS =

SUBDIRS = $(wildcard $(ALLDIRS))

########################################################################

THIS = $(shell basename `pwd`)
ALLSRCS = proc_OSTIA.F90 read_ncFile.F90 fillUp_Land.F90 bin2bin.F90
SRCS = $(wildcard $(ALLSRCS))

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

ALLDEPS := $(DEPS) $(addsuffix .d, $(basename $(BINS)))

ifneq ("$(SRCS)", "")
  LIB = lib$(THIS).a
else
  LIB = 
endif

########################################################################

#-----------------------
# Standard ESMA Targets
#-----------------------
TARGETS = esma_install esma_clean esma_distclean esma_doc \
          install clean distclean doc         

$(TARGETS):
	@echo
	@if [ "$(SUBDIRS)" != "" ]; then ( \
	   SUBTARGET=$@; export SUBTARGET; $(MAKE) -f $(GNUMAKEFILE) subdirs \
	) fi
	$(MAKE) -f $(GNUMAKEFILE) local_$@

local_esma_install local_install:
	$(MKDIR) $(ESMABIN) $(ESMAETC)
	$(MAKE) -f $(GNUMAKEFILE) bins
	$(MAKE) -f $(GNUMAKEFILE) csh

local_esma_clean local_clean:
	$(RM) *~ $(BINS) *.[aox] *.[Mm][Oo][Dd]

local_esma_distclean local_distclean:
	$(RM) *~ $(BINS) *.[aoxd] *.[Mm][Oo][Dd]

local_esma_doc local_doc:
	@echo No documentation here

esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% gmake install       (builds and install under ESMADIR)"
	@echo "% gmake clean         (removes deliverables: *.[aox], etc)"
	@echo "% gmake distclean     (leaves in the same state as cvs co)"
	@echo "% gmake esma_doc      (generates PDF, installs under ESMADIR)"
	@echo "% gmake help          (this message)"
	@echo
	@echo "Subtargets:"
	@echo "% gmake bins          (compiles and installs binary files)"
	@echo "% gmake csh           (installs shell scripts)"
	@echo "% gmake etc           (installs rc and nml files)"
	@echo "% gmake inc           (installs include files)"
	@echo "% gmake library       (builds and installs library files)"
	@echo "% gmake nml           (installs nml files)"
	@echo "% gmake perl          (installs perl scripts and packages)"
	@echo "% gmake rc            (installs rc files)"
	@echo "% gmake scripts       (installs shell scripts and perl files)"
	@echo "% gmake subdirs       (builds subdirectories)"
	@echo "% gmake ut            (compiles utility programs)"
	@echo
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"

list:
	@echo $(SRCS)
	@echo USER_FFLAGS = $(USER_FFLAGS)
	@echo USER_CFLAGS = $(USER_CFLAGS)

########################################################################

bins: $(BINS)
	@if [ "$(BINS)" != "" ]; then ( \
	   echo; \
	   echo "================================="; \
	   for file in $(BINS); do \
	      ( echo "copy to \$$(ESMABIN): $$file"; \
	        $(CP) $$file $(ESMABIN) ) \
	   done; \
	   echo "\$$(ESMABIN) = $(ESMABIN)"; \
	   echo "================================="; \
	   echo; \
	) fi

csh:
	@if [ "$(SHELLSCRIPTS)" != "" ]; then ( \
	   echo; \
	   echo "================================="; \
	   for scr in $(SHELLSCRIPTS); do \
	      ( $(SED) -e "$(SEDCMD)"< $$scr > $(ESMABIN)/$$scr; \
	        echo "script to \$$(ESMABIN): $$scr"; \
	        chmod 755 $(ESMABIN)/$$scr ) \
	   done; \
	   echo "\$$(ESMABIN) = $(ESMABIN)"; \
	   echo "================================="; \
	   echo; \
	) fi

########################################################################

#----------------------
# User Defined Targets
#----------------------

vpath % $(INC_LIST) /usr/include

FOPT  = 
FREAL = $(FREAL4) 
FPE   =

USER_FDEFS  = $(_D)
USER_FFLAGS = $(BIG_ENDIAN) $(BYTERECLEN)
USER_FFLAGS =
USER_CFLAGS =

USER_FMODS  = $(foreach dir,$(INC_LIST),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(INC_LIST),$(I)$(dir)) 

$(LIB) lib : $(DEPS) $(OBJS)
	@if [ "$(OBJS)" != "" ]; then ( \
	   $(RM) $(LIB); \
	   $(AR) $(AR_FLAGS) $(LIB) $(OBJS) \
	) fi

make_ostia_bcs.x: $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(LIB_NETCDF)

#----------------------------------------------------
# Hack to prevent remaking dep files during cleaning
#----------------------------------------------------
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
ifneq ($(findstring clean,$(SUBTARGET)),clean)
   -include $(ALLDEPS)
endif
endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
