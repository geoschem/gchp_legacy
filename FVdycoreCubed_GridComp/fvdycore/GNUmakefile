#
# Makefile for ESMA components.
#
#
# REVISION HISTORY:
#
# 09Jun2003  da Silva  First crack.
# 07Aug2003  Sawyer    Modified for FVdycore
# 27Aug2003  Sawyer    Added a2d3d and d2a3d
# 22Sep2004  Sawyer    Modifications for merge with current code
# 06Oct2004  Sawyer    Removed spmd_dyn
# 17Feb2005  Sawyer    Added epvd and ppme
# 17May2005  Sawyer    Added FVdycore_wrapper
# 18oct2005  da Silva  Moved ALTIX specific flags to FVdycore.mk
# 18Jan2006  Putman    Added mfz_comp
# 17May2006  Sawyer    Added diag_module.F90
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
  include               FVdycore_arch.mk  # arch dependent flags 

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

THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a

esma_install install: $(LIB)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS)
	$(CP) -p *.a            $(ESMALIB)
	$(CP) -p *.[Mm][Oo][Dd] $(ESMAINC)/$(THIS)
#	$(CP) -p *.rc   $(ESMAETC)

esma_clean clean:
	-$(RM) *~ *.[aox] *.[Mm][Oo][Dd] 

esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_doc doc:
	@ echo "nothing to do here"
	#@$(PROTEX) $(PROTEX_FLAGS) *GridComp*.[fF]* > $(ESMADOC)/$(THIS).tex


#                  --------------------
#                  User Defined Targets
#                  --------------------

SRCS := $(shell cat path_names)
OBJS := $(notdir $(addsuffix .o, $(basename $(SRCS)))) 
DEPS := $(notdir $(addsuffix .d, $(basename $(SRCS))))

DEPS := $(filter-out nsclock.d threadloc.d, $(DEPS)) # filter these out

INC_DIRS = . $(INC_GFDL_FMS) \
            $(INC_GMAO_SHARED) $(INC_SDF) $(INC_ESMF) $(INC_MPI)
MOD_DIRS = . $(INC_DIRS) 
SRC_DIRS = $(sort $(dir $(SRCS)))

USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir))
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 

vpath % $(SRC_DIRS) $(INC_DIRS) $(MOD_DIRS) /usr/include

#USER_FFLAGS += $(BIG_ENDIAN) 
USER_FDEFS   = $(D)MAPL_MODE $(D)SKIP_NO_QUAD_PRECISION $(D)READ_GRID $(D)NO_GRID_G $(D)SPMD $(D)TIMING
USER_CDEFS   = $(USER_FDEFS)

# The FV_PRECISION variable is exported from the parent
ifeq ($(FV_PRECISION),R4)
# SINGLE PRECISION FV_CUBED
FREAL = 
USER_FDEFS += $(D)SINGLE_FV
else
# DOUBLE PRECISION FV_CUBED
FREAL = $(FREAL8)
endif

# The Following need to be REAL*8
#--------------------------------
fv_grid_utils.o : fv_grid_utils.F90
	$(FC) -c $(F90FLAGS) $(FREAL8) model/$*.F90

$(LIB) lib : $(DEPS) $(OBJS)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

#                  --------------------
#                      Dependencies
#                  --------------------

# Make sure dep files are not remade during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

#.

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros



