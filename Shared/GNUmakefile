#
# recursive makefile for ESMA.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(shell pwd)/
endif


# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies

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
	@echo "     DIR_ESMF = $(DIR_ESMF)"


#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

ALLDIRS = arpack pnagpack \
	  GFDL_fms \
	  GMAO_gfio \
	  GMAO_hermes \
	  GMAO_mpeu \
	  GMAO_pilgrim \
          MAPL_cfio \
          MAPL_Base \
	  GEOS_Shared \
	  GEOS_Util

SUBDIRS = $(wildcard $(ALLDIRS) )

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          help install doc clean distclean

export ESMADIR BASEDIR ARCH SITE

$(TARGETS): 
	@echo "BPREC $(BPREC)"  
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			 \
	    ( cd $$d				;\
	      echo ""; echo Making $$t in `pwd`          ;\
	      $(MAKE) $$t ) \
	  done


local_esma_install local_install: $(LIB)
	@echo No local install in here

#              ------------------------------------------
#              Package Dependencies for Parallel Install
#              ------------------------------------------

ifeq ($(wildcard MAPL_cfio), $(null))
   MAPL_CFIO =
else
   MAPL_CFIO = MAPL_cfio_install
endif


MAPL_Base_install   : $(MAPL_CFIO)
MAPL_cfio_install   : $(GMAO_MPEU)        \
                      $(GMAO_MFHDF3)


#.
