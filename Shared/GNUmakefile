#------------------------------------------------------------------------------
#                  GEOS-Chem Global Chemical Transport Model                  !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: GNUmakefile
#
# !DESCRIPTION: Makefile to build MAPL and related libraries. 
#\\
#\\
# !REMARKS:
#  Originally from the GEOS-5 build sequence. 
#
# !REVISION HISTORY: 
#  20 Nov 2014 - R. Yantosca - Build GMAO_mpeu before GMAO_gfio.
#  20 Nov 2014 - R. Yantosca - Add more visible separators to text to 
#                              indicate when we are building an new library
#EOP
#------------------------------------------------------------------------------
#BOC

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
	@echo "     MPI_INC  = $(MPI_INC)"


#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

###############################################################################
# %%%%% COMMENTED OUT BY BOB Y. (11/20/14) %%%%%
#ALLDIRS = arpack pnagpack \
#	  GFDL_fms        \
#	  GMAO_gfio       \
#	  GMAO_hermes     \
#	  GMAO_mpeu       \
#	  GMAO_pilgrim    \
#          MAPL_cfio       \
#          MAPL_Base       \
#	  GEOS_Shared     \
#	  GEOS_Util
###############################################################################
#------------------------------------------------------------------------------
# %%%%% ADDED BY BOB Y. (11/20/14) %%%%%
#
# Change order of Shared library builds.  Put GMAO_mpeu before GMAO_gfio.
ALLDIRS = arpack pnagpack \
	  GFDL_fms        \
	  GMAO_mpeu       \
	  GMAO_gfio       \
	  GMAO_hermes     \
	  GMAO_pilgrim    \
          MAPL_cfio       \
          MAPL_Base       \
	  GEOS_Shared     \
	  GEOS_Util
#------------------------------------------------------------------------------

SUBDIRS = $(wildcard $(ALLDIRS) )

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          help install doc clean distclean

export ESMADIR BASEDIR ARCH SITE

$(TARGETS): 
	@echo "BPREC $(BPREC)"  
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do \
	    ( cd $$d ;\
              echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%';\
	      echo "%%% Making $$t in ', `pwd`" ;\
              echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%';\
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
