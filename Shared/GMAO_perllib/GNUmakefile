#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 16Feb2004  Zaslavsky  Initial imlementation.
# 20Oct2004  da Silva   Standardization
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # System dependencies

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

esma_install install: Net Time XML
	$(MKDIR) $(ESMABIN)
	$(CP) -R ./Net  $(ESMABIN)
	$(CP) -R ./Time $(ESMABIN)
	$(CP) -R ./XML  $(ESMABIN)
	$(CP) -R ./ESMA $(ESMABIN)
	$(CP) -R ./GrADS $(ESMABIN)

esma_clean esma_distclean clean distclean:
	echo "Nothing to clean here."

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"


esma_help:
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

help:
	@ echo "Available targets:"
	@ echo "  make lib             (to create libods.a)"
	@ echo "  make lib MODOBJS=    (to omit f90 modules)"
	@ echo "  make test            (to test ODS library)"
	@ echo "  make doc             (to create documentation)"
	@ echo "  make all"  
	@ echo "  make export"  
	@ echo "  make clean"  
	@ echo "  make distclean"
	@ echo "  make depend"  

#                  --------------------
#                      Dependencies
#                  --------------------

#include Makefile.depend

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
