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


#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

THIS := $(shell basename `pwd`)

BINS = regrid2_${ARCH}.x

LIB = regrid2.o

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          help install clean distclean doc

export ESMADIR BASEDIR ARCH SITE

esma_install install: $(BINS)

esma_clean clean esma_distclean distclean:
	$(RM) $(BINS) $(LIB)

esma_doc doc:
	@echo No documentation here

#                  --------------------
#                  User Defined Targets
#                  --------------------

regrid2_${ARCH}.x: regrid2.o
	$(FC) $^ -o $@

regrid2.o: regrid2.f
	$(FC) -c $<

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
