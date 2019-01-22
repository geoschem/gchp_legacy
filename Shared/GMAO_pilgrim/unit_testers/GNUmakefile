#-------------------------------------------------------------------------
#         NASA/GSFC, Global Modeling and Assimilation Office             !
#-------------------------------------------------------------------------
#
# !ROUTINE: GNUmakefile
#
# !DESCRIPTION:
#
#     Makefile for GMAO_pilgrim unit tests
#
# !SEE ALSO:
#
#     configure
#     Makefile.conf.*
#
#
# !REVISION HISTORY:
#
#  20050419  Sawyer    Creation
#
#-----------------------------------------------------------------------
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies

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

esma_install install: $(BIN)
	$(MKDIR) $(ESMABIN) $(ESMAETC)
#	$(CP) -p *.x    $(ESMABIN)
#	$(CP) -p *.rc   $(ESMAETC)

esma_clean clean:
	-$(RM) *~ *.[aox] *.[Mm][Oo][Dd]

esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"

#                  --------------------
#                  User Defined Targets
#                  --------------------

SGT = ScatterGatherTest
GT  = ghosttest

SRCS = ScatterGatherTest.F90 ghosttest.F90
OBJS = $(SRCS:.F90=.o)
BINS = $(SRCS:.F90=.x)

USER_FFLAGS = $(M)$(ESMAINC)/GMAO_pilgrim
LIB_COMP =  $(ESMALIB)/libGMAO_pilgrim.a

$(SGT).x : $(SGT).o
	echo "should include stubs $(LIB_ESMF)"
	$(LD) -o $(SGT).x $(LDFLAGS) $(SGT).o $(LIB_COMP)  \
		$(LIB_MPI) $(LIB_SCI) $(LIB_SYS) $(LIB_NETCDF)

$(GT).x : $(GT).o
	echo "should include stubs $(LIB_ESMF)"
	$(LD) -o $(GT).x $(LDFLAGS) $(GT).o $(LIB_COMP)  \
		$(LIB_MPI) $(LIB_SCI) $(LIB_SYS) $(LIB_NETCDF)

#                  --------------------
#                  Dependencies 
#                  --------------------
$(OBJS).o: $(LIB_COMP)

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
