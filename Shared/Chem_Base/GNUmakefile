#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 3mar2004  Zaslavsky  Initial imlementation.
# 20Oct2004  da Silva  Standardization
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := ../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # System dependencies


#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a

# Handle stubs for NCEP implementarion of GOCART Stand alone
# ----------------------------------------------------------
  ifeq ($(GOCART_MODE),stub)
     BINS = ut_Registry.x
     SCRP = 
     SRCS := Chem_RegistryMod---stub.F90
     LIBS = $(LIB)
     PY_MODS = 
     PY_EXTS = 
     PY_SCRP = 
  else
     BINS = Chem_Aod.x Chem_Aod3d.x ctl_crst.x gogo.x Chem_BundleToG5rs.x \
            reff_calculator.xx ext_calculator.xx
     SCRP = extinction_calculator.csh
     PY_MODS = mieobs.py
     PY_EXTS = MieObs_.$(F2PYEXT) 
     PY_SCRP = ext_calculator.py reff.py
     SRCS = Chem_RegistryMod.F90 Chem_ArrayMod.F90 \
            Chem_BundleMod.F90 Chem_Mod.F90 Chem_InitMod.F90 \
            Chem_MieTableMod.F90 Chem_MieMod.F90 Chem_AodMod.F90 \
            Chem_SimpleBundleMod.F90
     LIBS = $(LIB) $(LIB_MAPL_BASE) $(LIB_MAPL_BASE_STUBS) $(LIB_MAPL_BASE_STUBS2) \
            $(LIB_EU) $(LIB_CFIO) $(LIB_GFIO) \
            $(LIB_MPEU) $(LIB_MFHDF3) $(LIB_SDF) \
            $(LIB_ESMF) $(LIB_MPI) $(LIB_SYS)
     MPLIBS = $(LIB) $(LIB_MAPL_BASE) $(LIB_MAPL_BASE_STUBS) $(LIB_MAPL_BASE_STUBS2) \
            $(LIB_MPEU) $(LIB_CFIO) $(LIB_GFIO) \
            $(LIB_MPEU) $(LIB_MFHDF3) $(LIB_SDF) \
            $(LIB_ESMF) $(LIB_MPI) $(LIB_SYS)

     # f2py cannot handle a -Wl flag used by Open MPI. But that option is not
     # necessary for the code to compile or run. So if it is part of LIB_MPI
     # we just strip it from LIBS
     BADPATTERN := -Wl,--export-dynamic
     LIBS_F2PY1 = $(subst $(BADPATTERN),,$(LIBS))
     BADPATTERN2 := -pthread
     LIBS_F2PY = $(subst $(BADPATTERN2),,$(LIBS_F2PY1))
  endif

ifeq ( $(wildcard $(INC_MAPL_BASE)), $(null) ) 
      DGEOS5 =
else
      MOD_DIRS += $(INC_ESMF) $(INC_MAPL_BASE) $(INC_CFIO) $(INC_GMAO_MPEU) 
      DGEOS5 = $(D)GEOS5
endif

RCDIR = .

esma_install install: $(LIB) $(BINS) $(PY_EXTS)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAETC)/CMIP $(ESMAETC)/MERRA2 $(ESMAETC)/MERRA2-DD $(ESMAETC)/NR $(ESMAETC)/PIESA $(ESMAINC)/$(THIS) $(ESMABIN)
	$(CP) -p  *.a                    $(ESMALIB)
	$(CP) -p  *.rc                   $(ESMAETC)
	$(CP) -p  CMIP/*.rc              $(ESMAETC)/CMIP
	$(CP) -p  MERRA2/*.rc            $(ESMAETC)/MERRA2
	$(CP) -p  MERRA2-DD/*.rc         $(ESMAETC)/MERRA2-DD
	$(CP) -p  NR/*.rc                $(ESMAETC)/NR
	$(CP) -p  PIESA/*.rc             $(ESMAETC)/PIESA
	$(CP) -p  *.mod                  $(ESMAINC)/$(THIS)
	$(CP) -p  $(BINS) $(SCRP)        $(ESMABIN)
	$(CP) -p  $(PY_SCRP)             $(ESMABIN)
	$(CP) -p  $(PY_MODS) $(PY_EXTS)  $(ESMALIB)/Python

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd] *.xx *.so

esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] *.xx *.so

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"


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
	@echo "        FREAL = $(FREAL)"

#                  --------------------
#                  User Defined Targets
#                  --------------------
OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS))) \
        $(addsuffix .d, $(basename $(BINS))) 

BIG_ENDIAN =
THIS_GFIO = GMAO_gfio_r4
#FOPT = $(FOPT3)

ifeq ($(ESMA_REAL),$(FREAL8))
	THIS_GFIO = GMAO_gfio_r8
	THIS_CFIO = MAPL_cfio_r8
	FREAL = $(FREAL8)
else
	THIS_GFIO = GMAO_gfio_r4
	THIS_CFIO = MAPL_cfio_r4
	FREAL = $(FREAL4)
endif

MOD_DIRS += . $(INC_ESMF) $(INC_MAPL_BASE) $(INC_MPEU) $(INC_MFHDF3)

USER_FFLAGS = $(BIG_ENDIAN) $(DGEOS5)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(MOD_DIRS),$(I)$(dir)) 

# GPU_TARGET interferes with f2py. Turn it off
%_.$(F2PYEXT): GPU_TARGET := 

vpath % $(MOD_DIRS)

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

# Generic single PE apps
# ----------------------
%.x : $(LIB) %.o 
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIBS)

# Generic MPI apps
# ----------------
%.xx : $(LIB) %.o 
	$(LD) $(LDFLAGS) -o $@ $*.o $(MPLIBS)

# Python extensions
# -----------------
%_.$(F2PYEXT): %_py.F90 $(LIB)
	$(F2PY) -c -m $*_ $(M). $(USER_FINCS) $< $(LIBS_F2PY)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
