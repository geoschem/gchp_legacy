#
# recursive makefile for ESMA.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/..
endif


# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

esma_help :
	@echo "Standard ESMA targets:"
	@echo "% make esma_install    (builds and install under ESMADIR)"
	@echo "% make esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% make esma_distclean  (leaves in the same state as cvs co)"
	@echo "% make esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% make esma_help       (this message)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"

THIS := $(shell basename `pwd`)

#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

ALLDIRS = grads_util

BINS = zonal_${ARCH}.x

LIB = zonal.o

SUBDIRS = $(wildcard $(ALLDIRS))

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          help install clean distclean doc

export ESMADIR BASEDIR ARCH SITE

$(TARGETS): 
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			 \
	    ( cd $$d				;\
	      echo ""; echo Making $$t in `pwd`          ;\
	      $(MAKE) -e $$t ) \
	  done
	$(MAKE) local_$@

local_esma_install local_install: $(BINS)
	@echo ""

local_esma_clean local_clean:
	$(RM) $(BINS) $(LIB)

local_esma_distclean local_distclean:
	$(RM) $(BINS) $(LIB)

local_esma_doc local_doc:
	@echo "Target $@ not implemented yet in `pwd`"

#                  --------------------
#                  User Defined Targets
#                  --------------------

zonal_${ARCH}.x: zonal.o
	$(FC) $(FOPT2) $(BYTERECLEN) $(EXTENDED_SOURCE) $(PP) -I. -I$(INC_MAPL_BASE) -Dgrads $^ -o $@

zonal.o: zonal.f
	$(FC) $(FOPT2) $(BYTERECLEN) $(EXTENDED_SOURCE) $(PP) -I. -I$(INC_MAPL_BASE) -Dgrads -c $<

#.


  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

