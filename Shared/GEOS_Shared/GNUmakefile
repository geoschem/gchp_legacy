#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 09Jun2003  da Silva  First crack.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # System dependencies

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

THIS = GEOS_Shared
LIB  = lib$(THIS).a

esma_install install: $(LIB)
	$(MKDIR) $(ESMALIB) $(ESMAINC)/$(THIS)
	$(CP) -p *.a            $(ESMALIB)
	$(CP) -p *.mod          $(ESMAINC)/$(THIS)

esma_clean clean:
	-$(RM) *~ *.[aox] *.mod GEOS_GenericUsersGuide.tex \
               *.log *.ps *.dvi *.aux *.toc *.lot *.lof

esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.mod GEOS_GenericUsersGuide.tex \
               *.log *.ps *.dvi *.aux *.toc *.lot *.lof

esma_doc doc:
	@echo no documentation here

#                  --------------------
#                  User Defined Targets
#                  --------------------


SRCS_F90 = $(wildcard GEOS_Utilities.F90 DragCoefficients.F90 GEOS.F90 \
                  tropovars.F90 surfacelayer.F90 GEOS_TopoGet.F90 \
                  GEOS_GmapMod.F90 GEOS_RemapMod.F90 G3_MPI_Util_Mod.F90 windfix.F90 OVP.F90 )
SRCS_F   = $(wildcard PWSSSP.F sphere.F calcdbz.F)

OBJS = $(SRCS_F90:.F90=.o) $(SRCS_F:.F=.o)
DEPS = $(SRCS_F90:.F90=.d) $(SRCS_F:.F=.d)

INC_DIRS = . $(INC_ESMF) $(INC_MPI) $(INC_GMAO_SHARED) 
MOD_DIRS = . $(INC_DIRS) 

USER_FINCS = $(foreach dir,$(INC_DIRS),$(I)$(dir))
USER_FMODS = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 

ifeq ($(ESMA_FC),ifort)
ifeq ("$(BOPT)","fast")
   windfix.o: FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias -g -traceback
   windfix.o: FPE = -fpe0 -fp-model source -assume noold_maxminloc -fimf-arch-consistency=true

   G3_MPI_Util_Mod.o: FOPT = $(FOPT3) -qopt-report0 -ftz -align all -fno-alias -g -traceback
   G3_MPI_Util_Mod.o: FPE = -fpe0 -fp-model source -assume noold_maxminloc -fimf-arch-consistency=true
endif
endif

vpath % $(INC_DIRS) $(MOD_DIRS)

FREAL = $(FREAL4) # for now, require 32 bit reals (R4)
THIS_CFIO = MAPL_cfio_r4

$(LIB) lib : $(DEPS) $(OBJS)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(RANLIB_FLAGS) $(LIB)

PWSSSP.o: PWSSSP.F
	$(FC) -c $(FFLAGS) $(FREAL8) -I. PWSSSP.F

sphere.o: sphere.F
	$(FC) -c $(FFLAGS) $(FREAL8) -I. sphere.F

GEOS_Utilities.o: qsatlqu.code qsatice.code esatlqu.code esatice.code
DragCoefficients.o: getcds.code

# Make sure dep files are not remade during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
