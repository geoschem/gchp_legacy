#
# Makefile for ESMA PILGRIM component
#
# REVISION HISTORY:
#
# 30Jul2003  Zaslavsky  Created from the prototype provided by A. da Silva.
# 22Sep2003  Sawyer     Added mod_irreg
# 02Mar2005  Sawyer     Added dependency software from Arlindo, timingModule
# 08Mar2005  Sawyer     Merged Arlindo's changes
# 19Apr2005  Sawyer     Added PILGRIM_xyMod
# 01Mar2006  Sawyer     Removed memstuff from compile list
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
  include ./GMAO_pilgrim_arch.mk          # Customization of this makefile

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

THIS = $(shell basename `pwd` )
LIB  = lib$(THIS).a

esma_install install: $(DEPS) $(LIB)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS)
	$(CP) -p *.a    $(ESMALIB)
	$(CP) -p *.mod  $(ESMAINC)/$(THIS)
###	$(CP) -p *.rc   $(ESMAETC)

esma_clean clean:
	$(RM) -f *~ *.[aox] *.[Mm][Oo][Dd]

esma_distclean distclean:
	$(RM) -f *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"


#                  --------------------`
#                  User Defined Targets
#                  --------------------

PILGRIM_SEQSRCS  = shr_kind_mod.F90 debugutilitiesmodule.F90 timingModule.F90 \
                   decompmodule.F90 ghostmodule.F90

PILGRIM_PUMSRCS  = mod_comm.F90 parutilitiesmodule.F90 PILGRIM_xyMod.F90 \
                   redistributemodule.F90 

PILGRIM_CSRCS    = mp_assign_to_cpu.c


PILGRIM_SEQOBJS         = ${PILGRIM_SEQSRCS:.F90=.o}
PILGRIM_PUMOBJS         = ${PILGRIM_PUMSRCS:.F90=.o} 
PILGRIM_COBJS           = ${PILGRIM_CSRCS:.c=.o} 

SRCS = $(PILGRIM_SEQSRCS) $(PILGRIM_PUMSRCS) ### $(PILGRIM_CSRCS)
OBJS = $(PILGRIM_SEQOBJS) $(PILGRIM_PUMOBJS) ### $(PILGRIM_COBJS)        
DEPS = $(OBJS:.o=.d) 

USER_FINCS += $(I). $(I)$(INC_MPI)  
USER_FDEFS += $(D)$(ARCH) $(D)SPMD $(D)STAND_ALONE $(D)MODCM_ALLOC ###$(D)SET_CPUS $(D)USE_MPI2

#USER_CFLAGS = $(D)$(ARCH) $(D)FORTRANUNDERSCORE $(D)SET_CPUS \
#                   $(D)PIN_CPUS $(D)USE_MPI2

vpath % $(INC_MPI)

AR_FLAGS = -clr

#F90FLAGS = -O3 -64 -cpp -mp -extend_source $(D)IRIX64 $(I). $(D)SPMD $(D)STAND_ALONE  -c
FREAL =   # Undefined any of the nasty -r4 or -r8 flags

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

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
