#
# Makefile for ESMA build configuration system
#

# Generic Base/Arch + User Defined Fragments
# ------------------------------------------
  ESMA_BASE = ESMA_base.mk $(wildcard $(HOME)/.esma_base.mk) 
  ESMA_ARCH = ESMA_arch.mk $(wildcard $(HOME)/.esma_arch.mk) 
  ESMA_POST = ESMA_post.mk $(wildcard $(HOME)/.esma_post.mk) 

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMA_BASE)            # Generic stuff
  include $(ESMA_ARCH)            # System dependencies

  BINS    = # f90aib.x f90split.x
  SCRIPTS = mklpath.pl endiantest.pl OScheck.pl esma_timer.sh esma_tgraph.pl

  ARCHMKS = ifort.mk pgfortran.mk gfortran.mk nagfor.mk absoft.mk \
            cray.mk compiler.mk mpi.mk math.mk tau.mk linux.mk darwin.mk \
            vectorize.mk


# User can provide their own assert script
# ----------------------------------------
  ASSERT_MK = 
  ifdef ASSERT
        RC = $(shell $(ASSERT))
        ifeq ($(RC), 0)
              $(warning Enabling build environment check)
	      ASSERT_MK = Assert.mk
        else 
           ifeq ($(RC), -1)
             $(warning - Node not know, disabling build environment check)
           else 
             $(error Please correct your build environment and try again)
           endif
        endif
  endif

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

esma_help help:
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

makedirs : 
	$(MKDIR) $(ESMABIN) $(ESMALIB) $(ESMAINC) \
		 $(ESMAETC) $(ESMADOC) $(ESMACFG)

	@if [ "$(ESMACFG)" != "$(ESMADIR)/Config" ]; then (\
	   $(RM) -rf $(ESMADIR)/Config;  \
	   cd $(ESMADIR); $(LN) -s $(ARCH)/Config . \
	) fi

	$(CP) -p $(ARCHMKS) $(ESMACFG)

	$(CAT) $(ESMA_BASE) > $(ESMACFG)/ESMA_base.mk
	$(CAT) $(ESMA_ARCH) > $(ESMACFG)/ESMA_arch.mk
	$(CAT) $(ESMA_POST) > $(ESMACFG)/ESMA_post.mk
	$(CAT) GMAO_base.mk $(ASSERT_MK) > $(ESMACFG)/GMAO_base.mk
	echo "I = $(I)"              > $(ESMACFG)/NCEP_base.mk
	echo "LIB_SDF = $(LIB_SDF)" >> $(ESMACFG)/NCEP_base.mk
	$(CAT) NCEP_base.mk         >> $(ESMACFG)/NCEP_base.mk

	echo "BASEDIR: $(BASEDIR)" > $(ESMAETC)/BASEDIR.rc

esma_install install : makedirs fdp.o $(BINS)
	$(CP) -p protex $(MODINFO) $(BINS) $(SCRIPTS) $(ESMABIN)

ifdef ASSERT
	$(CP) $(ASSERT) $(ESMABIN)/Assert
endif
	./install_fdp ./fdp $(FDP)
	./CVSTAG_install.pl $(ESMAETC)
	./OScheck.pl -q -cp $(ESMAETC)

esma_clean clean:
	-$(RM) *~ *.[aox] *.[Mm][Oo][Dd]

esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"

# These are needed to solve a chicken-and-egg issue. All config files
# are referenced to are in $(ESMACFG) but FC isn't known until those are
# sourced. This directory needs to know what FC is to do the fdp.o

ifdef ESMA_FC
   FC := $(ESMA_FC)
endif

ifndef ESMA_FC
   ESMA_FC := ifort
endif

# This is mainly here to avoid weird PGI or other flags from 
# testing the simple fdp code.
FOPT =
FREAL = $(FREAL4)

# If ESMA_TIMER is enabled, fdp.o can't use the "standard"
# .f90.o rule since it gets compiled before esma_timer.sh is
# copied into ESMABIN. Easiest solution, just undefine ESMA_TIMER
# locally.

ESMA_TIMER =

%.x : %.F90
	$(FC) -o $@ $(F90FLAGS) $<

  include $(ESMA_POST)

#.

