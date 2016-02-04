#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 3mar2004  Zaslavsky  Initial imlementation.
# 20oct2004  da Silva  Standardization.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../..
endif

THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a
BINS = # Chem_SettlingCalculator.x

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # GMAO stuff

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------


esma_install install: $(DEPS) $(LIB) $(BINS)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(INC_CHEM_SHARED)
	$(CP) -p *.mod   $(INC_CHEM_SHARED)
	$(CP) -p *.a     $(ESMALIB)
#	$(CP) -p $(BINS) $(ESMABIN)

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd]

esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_doc doc:
	@$(PROTEX) $(PROTEX_FLAGS) *GridComp*.[fF]* > $(ESMADOC)/$(THIS).tex

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
	@echo "       GEOS-5 = $(DOING_GEOS5) "

#                  --------------------
#                  User Defined Targets
#                  --------------------

ifeq ($(DOING_GEOS5),TRUE)
      FVGCM_SRCS = mod_diag.F90 diaglist.F90
      USER_FFLAGS  = -DFVCHEM
      CHEM_UTIL = Chem_UtilMod.F90 Chem_StateMod.F90
else
      CHEM_UTIL = Chem_UtilMod_geos4.F90 
endif

SRCS = $(FVGCM_SRCS) Chem_ConstMod.F90 Chem_SettlingMod.F90 \
       DryDepositionMod.F90 DustEmissionMod.F90 \
       WetRemovalMod.F90 SeasaltEmissionMod.F90 \
       SulfateChemDriverMod.F90 ConvectionMod.F90 \
       SmokeEmissionMod.F90 $(CHEM_UTIL)
OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS))) \
        $(addsuffix .d, $(basename $(BINS)))
SPMD = $(D)SPMD
DEBUG = # $(D)DEBUG

ifeq ($(ESMA_REAL),$(FREAL8))
	THIS_GFIO = GMAO_gfio_r8
	THIS_CFIO = MAPL_cfio_r8
	FREAL = $(FREAL8)
else
	THIS_GFIO = GMAO_gfio_r4
	THIS_CFIO = MAPL_cfio_r4
	FREAL = $(FREAL4)
endif

INC_DIRS = . $(INC_MAPL_BASE) $(INC_ESMF) $(INC_SDF)
MOD_DIRS = . $(INC_ESMF) $(INC_CFIO) $(INC_MPEU) $(INC_CHEM_BASE) \
             $(INC_PILGRIM) $(INC_CHEM_SHARED) \
             $(INC_MAPL_BASE) $(INC_GEOS_SHARED)

USER_FFLAGS = $(BIG_ENDIAN) 
USER_FDEFS  = $(DEBUG) $(SPMD) $(D)FVCHEM 
USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) 
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 

# MAT This file takes two minutes to compile. As this code
#     only initializes and fills a derived data type with
#     strings, compiling with optimization is unnecessary.
#     Compiling with -O0 reduces compile time greatly with
#     zero difference.
ifneq ("$(BOPT)","g")
diaglist.o: FOPT := -O0
endif

vpath % $(MOD_DIRS)

#FOPT = $(FOPT3)

$(LIB) lib : $(OBJS)
	$(RM) $(LIB_CHEM_SHARED)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

%.x : $(LIB) %.o 
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIB) $(LIB_CHEM_BASE) $(LIB_MAPL_BASE) \
                                    $(LIB_CFIO) $(LIB_EU) $(LIB_GFIO) \
                                    $(LIB_MFHDF3) $(LIB_PILGRIM) $(LIB_SDF) \
                                    $(LIB_ESMF) $(LIB_SYS) $(LIB_MPI)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros
