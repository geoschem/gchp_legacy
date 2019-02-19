#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# -------------------------------------------------------------------------


# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(shell pwd)/../../..
endif

THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a

BINS :=  pfio_collective_demo.x pfio_io_demo.x pfio_server_demo.x

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk

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
	@echo "         SITE = $(SITE) "
	@echo "         THIS = $(THIS) "

#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          install clean distclean doc 

export ESMADIR BASEDIR ARCH SITE FREAL

$(TARGETS): 
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			 \
	    ( cd $$d				;\
	      echo ""; echo Making $$t in `pwd`          ;\
	      $(MAKE) -e $$t ) \
	  done
	$(MAKE) local_$@

local_esma_install local_install: $(LIB) $(BINS)
	$(MKDIR) $(ESMALIB) $(ESMAINC)/$(THIS)
	$(CP) -pr *.a $(wildcard Python)   $(ESMALIB)
	$(CP) -p *.[Mm][Oo][Dd] $(IINCS)   $(ESMAINC)/$(THIS)

local_esma_clean local_clean:
	-$(RM) *~ *.[aox] *.[Mm][Oo][Dd] *___.f90

local_esma_distclean local_distclean:
	-$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] *___.f90

local_esma_doc local_doc:
	@echo "Target $@ not implemented in `pwd`"

#                  --------------------
#                  User Defined Targets
#                  --------------------

INCS :=  NetCDF4_get_var.H  NetCDF4_put_var.H  unused_dummy.H

SRCS :=  AbstractCollectiveDataMessage.F90 AbstractDataMessage.F90        AbstractDataReference.F90    AbstractDirectoryService.F90 \
         AbstractMessage.F90               AbstractRequestHandle.F90      AbstractServer.F90           AbstractSocket.F90           \
         AbstractSocketVector.F90          AddExtCollectionMessage.F90    AddHistCollectionMessage.F90 ArrayReference.F90           \
         Attribute.F90                     BaseServer.F90                 BaseThread.F90               ClientThread.F90  ClientThreadVector.F90           \
         CollectivePrefetchDataMessage.F90 CollectiveStageDataMessage.F90 CoordinateVariable.F90       DirectoryService.F90         \
         DoneMessage.F90                   DummyMessage.F90               ExtDataCollection.F90        FileMetadata.F90             \
         FileMetadataVector.F90            HistoryCollection.F90          IDMessage.F90                IntegerIntegerMap.F90        \
         IntegerMessageMap.F90             IntegerRequestMap.F90          IntegerSocketMap.F90         KeywordEnforcer.F90          \
         LocalMemReference.F90             MessageVector.F90              MessageVisitor.F90           MpiMutex.F90                 \
         MpiServer.F90                     MpiSocket.F90                  NetCDF4_FileFormatter.F90    \
         pFIO_Constants.F90                pFIO.F90                       pFIO_Utilities.F90           ProtocolParser.F90           \
         PrefetchDataMessage.F90           RDMAReference.F90 StageDataMessage.F90           ServerThread.F90             ServerThreadVector.F90       \
         ShmemReference.F90                SimpleSocket.F90             \
         StringIntegerMap.F90              StringInt64Map.F90       StringVector.F90             TerminateMessage.F90         \
         Throw.F90                         Variable.F90                   WaitRequestDataMessage.F90   pfio_base.F90                \
         pfio_collective_demo.F90          pfio_io_demo.F90               pfio_server_demo.F90 StringStringMap.F90 StringVariableMap.F90 \
         ModifyMetadataMessage.F90         UnlimitedEntity.F90 DownBit.F90 pFIO_ShaveMantissa.c pFIO_ErrorHandling.F90 
         #OpenMPServer.F90 # deactivated for now \

pfio_io_demo.o: | pfio_collective_demo.o
pfio_server_demo.o: | pfio_io_demo.o

#FOPT += $(OMPFLAG)

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

INC_DIRS = . $(INC_ESMF) $(INC_MPI) $(INC_SDF) $(INC_gFTL)
MOD_DIRS = $(INC_DIRS)

USER_FINCS = $(foreach dir,$(INC_DIRS),$(I)$(dir))
USER_FMODS = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FDEFS = $(D)MAPL $(D)DO_COMMAS $(D)TWO_SIDED_COMM $(D)__ifort_18

ifeq ($(IFORT_MAJOR),18)
  USER_FDEFS += $(D)__ifort_18
endif

FREAL = $(FREAL4) # for now, require 32 bit reals (R4)

USER_LDFLAGS += $(OMPFLAG)

vpath % $(MOD_DIRS) tests

$(LIB) lib : $(DEPS) $(OBJS)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(RANLIB_FLAGS) $(LIB)

%.x : $(LIB) %.o
	$(FC) $(LDFLAGS) -o $@ $*.o $(LIB) \
              $(LIB_ESMF) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
