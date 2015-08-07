#! /bin/make

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(shell pwd)/../../
endif

THIS := $(shell basename `pwd`)

BINS := testhorz.x utCFIO_Bundle.x

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          install clean distclean doc 

export ESMADIR BASEDIR ARCH SITE FREAL

esma_install install: $(BINS)

esma_clean clean:
	-$(RM) *~ *.[aox] *.[Mm][Oo][Dd] *___.f90

esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] *___.f90

esma_doc doc:
	@echo "Target $@ not implemented in `pwd`"


.SUFFIXES: .F90


SRCS := testhorz.F90 tstsort.F90 utCFIO_Array.F90 utCFIO_Bundle.F90 utCFIO_Nbits.F90 utDistIO.F90 ut_ExtData.F90

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

INC_DIRS = . $(INC_MAPL_BASE) $(INC_ESMF) $(INC_CFIO) $(INC_MPEU) $(INC_MPI) $(INC_GEOS_FV1)
MOD_DIRS = . $(INC_DIRS) 

USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir))
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir))
         
FREAL = $(FREAL4)
THIS_CFIO = MAPL_cfio_r4

#                  --------------------
#                      Dependencies
#                  --------------------

LIBS_NC := $(shell $(GC_BIN)/nc-config --flibs)
LIBS_NC += $(shell $(GC_BIN)/nc-config --libs)

testhorz.x : testhorz.o
	$(FC) $(LDFLAGS) -o $@ $+ -L$(ESMALIB)/ -lFVdycoreCubed_GridComp -lfvdycore $(LIB_GFDL_FMS) $(LIB_MAPL_BASE) \
              $(LIB_ESMF) $(LIB_CFIO) $(LIB_MPEU) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS) $(LIBS_NC)

utCFIO_Array.x : utCFIO_Array.o
	$(FC) $(LDFLAGS) -o $@ $+ $(LIB_MAPL_BASE) $(LIB_MAPL_BASE_STUBS) \
              $(LIB_ESMF) $(LIB_CFIO) $(LIB_MPEU) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)

utCFIO_Bundle.x : utCFIO_Bundle.o
	$(FC) $(LDFLAGS) -o $@ $+ $(LIB_MAPL_BASE) $(LIB_MAPL_BASE_STUBS) \
              $(LIB_ESMF) $(LIB_CFIO) $(LIB_MPEU) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)

utCFIO_Nbits.x : utCFIO_Nbits.o
	$(FC) $(LDFLAGS) -o $@ $+ $(LIB_MAPL_BASE) $(LIB_MAPL_BASE_STUBS) \
              $(LIB_ESMF) $(LIB_CFIO) $(LIB_MPEU) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)


  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
