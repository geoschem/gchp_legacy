SHELL=/bin/sh

#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  13Nov2007  stassi  Created with gmm script.
#  02Jun2008  Todling Removed eta2prs.F and fv2prs.F90
#  03Mar2009  Todling Add dyndot.x (needs libsci)
#
#.......................................................................

#---------------------------------------------------
# Get GNUmakefile name
# note: MAKEFILE_LIST defined for gmake versions >= 3.80
#---------------------------------------------------
ifdef MAKEFILE_LIST
   GNUMAKEFILE := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
else
   GNUMAKEFILE := GNUmakefile
endif

#------------------------------
# Make sure ESMADIR is defined
# -----------------------------
ifndef ESMADIR
   ESMADIR := $(PWD)/../..
endif

#-------------------------------
# Compilation rules, flags, etc
#-------------------------------
include $(ESMADIR)/Config/ESMA_base.mk   # Generic stuff
include $(ESMADIR)/Config/ESMA_arch.mk   # System dependencies
include $(ESMADIR)/Config/GMAO_base.mk   # System dependencies
include ./GMAO_hermes_arch.mk           # Customization of this makefile


########################################################################

ifeq ($(ESMA_SDF),hdf)
    LIB_SDFEOS = $(LIB_HDFEOS) $(LIB_GCTP)
    LCV2PRS_EOS = lcv2prs_eos.x
else
    LIB_SDFEOS = null
    LCV2PRS_EOS = 
endif

ifeq ($(wildcard $(LIB_GEOS_SHARED)),)
   BINS_MAPL = 
else
   BINS_MAPL = GFIO_remap.x GFIO_Shave.x lcv2prs.x $(LCV2PRS_EOS)
endif

BINS =  ana5sfc.x diag2dyn.x drs2dyn.x dyn2drs.x dyn2dyn.x \
	dyn2prs.x dyn2rs5.x dyn52dyn.x dyndiff.x dyndims.x dynp.x echorc.x \
	fvsubset.x getgfiodim.x ibc_upd.x maph.x maph_pert.x mapz.x \
	recalcsfc.x reset_time.x rs52dyn.x gcmbkg2ana.x dyndot.x \
        dyn_pert_remap.x dyn_recenter.x dyn_rankhist.x dyn2real_eta.x $(BINS_MAPL)

UT = ut_ana2dyn.x ut_fread.x ut_insitu.x ut_lp.x ut_ocean.x ut_sst.x \
	blendq.x geos2fv.x rout2prs.x 

MOD_DIRS = . $(INC_MPEU) $(INC_GFIO) $(INC_CFIO) $(INC_SDF) \
             $(INC_MAPL_BASE) $(INC_GEOS_SHARED)

PERLSCRIPTS = ana5sfc cnv2prs.pl
PERLPKGS = 
SHELLSCRIPTS = 

RCFILES = $(wildcard *.rc)
NMLFILES = 

SUBDIRS = 

########################################################################

SCRIPTS  = $(PERLSCRIPTS) $(PERLPKGS) $(SHELLSCRIPTS)
ETCFILES = $(RCFILES) $(NMLFILES)

scripts  = $(SCRIPTS)
perl     = $(PERLSCRIPTS)
csh      = $(SHELLSCRIPTS)
etc      = $(ETCFILES)
rc       = $(RCFILES)
nml      = $(NMLFILES)

CVSTAG    := $(shell cat $(ESMAETC)/CVSTAG)

ifeq ($(wildcard $(ESMAETC)/PUBLICTAG),)
      PUBLICTAG := unknown
else
      PUBLICTAG := $(shell cat $(ESMAETC)/PUBLICTAG)
endif

SEDCMD1 = s^\@DASPERL^\$(PERL)^;s^\@DASSED^\$(SED)^;s^\@DASHELL^\$(SHELL)^
SEDCMD2 = s^\@CVSTAG^\$(CVSTAG)^;s^\@PUBLICTAG^\$(PUBLICTAG)^

########################################################################

THIS = $(shell basename `pwd`)
LIB  = lib$(THIS).a

SRCS = m_ana2dyn.F m_const.f90 m_dyn.f90 m_dyn2dyn.f90 m_dynp.F90 \
	m_insitu.F m_interp.F90 m_lp.F m_maph.F90 m_maph_pert.F90 m_mapz.F \
	m_qsat.F m_remap.f90 m_set_eta.F90 m_vdc2vdc.F90 progsig.f90 m_topo_remap.F90 \
	m_interpack.F m_interpack_ad.F m_ec_set_eta.F90 m_ecdyn.f90 dyn_pert_remap.F90

########################################################################

#-----------------------
# Standard ESMA Targets
#-----------------------
esma_install install: $(LIB) $(BINS)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS) $(ESMABIN)
	-$(CP) -p *.a $(ESMALIB)
	-$(CP) -p *.x $(ESMABIN)
	-$(CP) -p *.[Mm][Oo][Dd] *.[Hh] $(ESMAINC)/$(THIS)

	@if [ "$(SCRIPTS)" != "  " ]; then (\
	   gmake -f \$(GNUMAKEFILE) scripts \
	) fi

	@if [ "$(ETCFILES)" != " " ]; then (\
	   gmake -f \$(GNUMAKEFILE) etc \
	) fi

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs\
	) fi

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd]
	$(RM) -r *.hdf *.nc4 *.grd *.[axol] *.mod \
              *~ *.log *.ps *.dvi *.aux *.toc *.lot *.lof rii_files/

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs\
	) fi

esma_distclean distclean:
	gmake -f $(GNUMAKEFILE) esma_clean
	$(RM) *.d

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs\
	) fi

esma_doc doc:
	$(PROTEX) ana5sfc.F90 > ana5sfc.tex
	$(LATEX) ana5sfc
	$(LATEX) ana5sfc

esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% gmake esma_install    (builds and install under ESMADIR)"
	@echo "% gmake esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% gmake esma_distclean  (leaves in the same state as cvs co)"
	@echo "% gmake esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% gmake esma_help       (this message)"
	@echo "% gmake scripts         (installs scripts only)"
	@echo "% gmake perl            (installs perl scripts only)"
	@echo "% gmake csh             (installs shell scripts only)"
	@echo "% gmake etc             (installs rc and nml files only)"
	@echo "% gmake rc              (installs rc files only)"
	@echo "% gmake nml             (installs nml files only)"
	@echo "% gmake ut              (compiles utility programs)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"
	@echo "   ESMA_SDF = $(ESMA_SDF)"
	@echo "    INC_SDF = $(INC_SDF)"

########################################################################

scripts perl csh:
	@echo ""
	@for scr in $($@); do  \
	   ( $(SED) -e "$(SEDCMD1)"< $$scr > $(ESMABIN)/$$scr; \
	     echo "script to \$$(ESMABIN): $$scr";  \
	     chmod 755 $(ESMABIN)/$$scr ) \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

etc rc nml:
	@echo ""
	@for file in $($@); do  \
	   ( $(SED) -e "$(SEDCMD2)"< $$file > $(ESMAETC)/$$file; \
	     echo "file to \$$(ESMAETC): $$file")  \
	done
	@echo ""
	@echo "\$$(ESMAETC) = $(ESMAETC)"
	@echo ""

subdirs:
	@ SUBTARGET=
	@ for subdir in $(SUBDIRS); do  \
	   ( cd $$subdir;  \
	     echo "$(MAKE) -e $(SUBTARGET) $$subdir";  \
	     $(MAKE) -e $(SUBTARGET) ) \
	done

ut:
	@echo ""
	@for prog in $(UT); do  \
	   ( echo ""; echo "making $$prog in local directory"; \
	     gmake -f $(GNUMAKEFILE) $$prog ) \
	done
	@echo ""

########################################################################

#----------------------
# User Defined Targets
#----------------------

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS))) \
        $(addsuffix .d, $(basename $(BINS)))

FOPT = $(FOPT3)
COPT = $(COPT3)

USER_FFLAGS = $(BIG_ENDIAN) $(MPFLAG) $(PP)
USER_FDEFS  = $(D)HERMES $(D)DEBUG_SHAVE $(D)unix $(D)__unix__
USER_CDEFS  = $(D)HERMES $(D)DEBUG_SHAVE $(D)unix $(D)__unix__
USER_CFLAGS = $(BIG_ENDIAN) 
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) 

vpath % $(MOD_DIRS) /usr/include

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)


# test and auxiliary programs
# ---------------------------
%.x : $(LIB) %.o 
	$(LD) $(LDFLAGS)  -o $@ $*.o $(LIB) \
	$(LIB_GFIO) $(LIB_EU) $(LIB_SDF) $(LIB_SCI) $(LIB_SYS)

dyndims.x : dyndims.o 
	$(LD) $(LDFLAGS) -o dyndims.x dyndims.o \
	      $(LIB_SDF) $(LIB_SYS)

GFIO_remap.x : $(LIB) GFIO_remap.o
	$(LD) $(LDFLAGS)  -o $@ GFIO_remap.o $(LIB) \
	$(LIB_GFIO) $(LIB_EU) $(LIB_CFIO) $(LIB_SDF) $(LIB_SCI) $(LIB_SYS)

GFIO_Shave.x : $(LIB) GFIO_Shave.o
	$(LD) $(LDFLAGS)  -o $@ GFIO_Shave.o $(LIB) \
	$(LIB_GFIO) $(LIB_EU) $(LIB_CFIO) $(LIB_SDF) $(LIB_SCI) $(LIB_SYS)

echorc.x :  echorc.o
	${FC} -o $@ $(LDFLAGS) echorc.o $(LIB_EU) $(LIB_SYS)

fvsubset.x : $(LIB) fvsubset.o
	${FC} $(LDFLAGS) -o $@ fvsubset.o $(LIB) \
		$(LIB_EU) $(LIB_GFIO) $(LIB_SDF) $(LIB_SYS) $(LIB_SDF)

lcv2prs.x : $(LIB) lcv2prs.o
	$(LD) $(LDFLAGS) -o $@ lcv2prs.o $(LIB) \
	      $(LIB_CFIO) $(LIB_EU) $(LIB_SDF) $(LIB_SYS) \
	      $(LIB_MAPL_BASE) $(LIB_GEOS_SHARED)

Chem_rst2aer_v.x : $(LIB) Chem_rst2aer_v.o
	$(LD) $(LDFLAGS) -o $@ Chem_rst2aer_v.o $(LIB) \
	      $(LIB_CFIO) $(LIB_EU) $(LIB_SDF) $(LIB_SYS) \
	      $(LIB_MAPL_BASE) $(LIB_GEOS_SHARED)

lcv2prs_eos.x : $(LIB) lcv2prs_eos.o
	$(LD) $(LDFLAGS) -o $@ lcv2prs_eos.o $(LIB) \
	      $(LIB_CFIO_EOS) $(LIB_EU) $(LIB_SDF) $(LIB_SYS) \
	      $(LIB_MAPL_BASE) $(LIB_GEOS_SHARED) $(LIB_SDFEOS)

# Special deps
# ------------
lcv2prs_eos.d : lcv2prs.d
	$(SED) "s/lcv2prs/lcv2prs_eos/" < lcv2prs.d > $@

# Special objs
# ------------
lcv2prs_eos.o: lcv2prs.F90
	$(ESMA_TIMER) $(FC) -o lcv2prs_eos.o -c -DHDFEOS $(F90FLAGS) lcv2prs.F90

dyn2rs5.o : dyn2rs5.f90
	@echo '---> Compiling $<'
	$(FC) -c $(patsubst $(BIG_ENDIAN),,$(f90FLAGS)) $<

rs52dyn.o : rs52dyn.f90
	@echo '---> Compiling $<'
	$(FC) -c $(patsubst $(BIG_ENDIAN),,$(f90FLAGS)) $<

dyn2drs.o : dyn2drs.f90
	@echo '---> Compiling $<'
	$(FC) -c $(patsubst $(BIG_ENDIAN),,$(f90FLAGS)) $<

#----------------------------------------------------
# Hack to prevent remaking dep files during cleaning
#----------------------------------------------------
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
   -include $(DEPS)
endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
