#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 19Jan2004  Zaslavsky  Initial imlementation.
# 20Oct2004  da Silva   Standardization
# 04Feb2009  Todling    Remove make_satinfo related files
# 05Nov2009  Todling    Remove lnlist and rc file
#

# Get GNUmakefile name (requires gmake versions >= 3.80)
#-------------------------------------------------------
ifdef MAKEFILE_LIST
   GNUMAKEFILE := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
else
   GNUMAKEFILE := GNUmakefile
endif

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

########################################################################

ALLPERLSCRIPT = $(wildcard *.pl) \
                $(wildcard *.py) \
	acquire        	         \
	acquire_obsys  	         \
	bacon          	         \
	esma_mpirun              \
	fvgetf         	         \
	g5_modules_perl_wrapper  \
	grads2grib     	         \
	jobIDfilter              \
	lsfpL                    \
	mkdir_remote             \
	n4zip                    \
	now                      \
	num_days_in_month        \
	pasta                    \
	pesto          	         \
	prund.pl       	         \
	r_dist         	         \
	require_obsys  	         \
	rget           	         \
        rndasfiles               \
	rput           	         \
	rst2rcyc       	         \
	rst4ncepf      	         \
	tick           	         \
	token_resolve            \
	vED                      \
        stripname

PERLSCRIPT = $(wildcard $(ALLPERLSCRIPT))

PERLPKG = $(wildcard *.pm)

#-------------------------------------------------------
# ALLSRC   = source files for binaries with no extension
# ALLSRCX  = source files for binaries with .x extension
# OTHERBIN = other files for ESMABIN directory
# ALLETC   = files for ESMAETC directory
# ALLGRADS = files for ESMALIB/grads directory
#-------------------------------------------------------
ALLSRC = argopt.c rst_date.f

ALLSRCX = fresolv.f90 gethdftinc.f mkdrstdate.f  \
          zeit_ci.f zeit_co.f zeit_pr.f

OTHERBIN = Functions.csh pyrob

ALLETC =  pesto.arc obsys*.rc *.tmpl \
          PUBLICTAG

ALLGRADS = lats4d.gs lats4d info.gs 


########################################################################

bin        = $(BINS)
etc        = $(ETCFILES)
grads      = $(GRADFILES)
perlpkg    = $(PERLPKG)
perlscript = $(PERLSCRIPT)
PERLFILES  = $(PERLSCRIPT) $(PERLPKG)

SEDCMD = s^\@DASPERL^\$(PERL)^;s^\@DASSED^\$(SED)^;s^\@DASHELL^\$(SHELL)^

########################################################################

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

THIS := $(shell basename $(PWD))

SRC = $(wildcard $(ALLSRC))
BIN = $(basename $(SRC))

SRCX = $(wildcard $(ALLSRCX))
BINX = $(addsuffix .x, $(basename $(SRCX)))

BINO = $(wildcard $(OTHERBIN))

# all files for bin, etc, and grads directories
#----------------------------------------------
BINS      = $(BIN) $(BINX) $(BINO)
ETCFILES  = $(wildcard $(ALLETC))
GRADFILES = $(wildcard $(ALLGRADS))


esma_install install: $(BINS)
	$(MKDIR) $(ESMALIB)/grads $(ESMAETC) $(ESMABIN) 

	@if [ "$(BINS)" != "  " ]; then (\
	   gmake -f \$(GNUMAKEFILE) bin \
	) fi

	@if [ "$(PERLFILES)" != " " ]; then (\
	   gmake -f \$(GNUMAKEFILE) perl \
	) fi

	@if [ "$(ETCFILES)" != "" ]; then (\
	   gmake -f \$(GNUMAKEFILE) etc \
	) fi

	@if [ "$(GRADFILES)" != "" ]; then (\
	   gmake -f \$(GNUMAKEFILE) grads \
	) fi


esma_clean clean:
	$(RM) -r $(BIN) *.[axol] *.[Mm][Oo][Dd] *~ Makefile*~ rii_files/ \
	         obs.ods *.hdf *.nc4 *.aux *.dvi *.ps  *.toc *.log
esma_distclean distclean:
	$(RM) -r $(BIN) *.[axodl] *.[Mm][Oo][Dd] *~ Makefile*~ rii_files/ \
	         obs.ods *.hdf *.nc4 *.aux *.dvi *.ps  *.toc *.log
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

########################################################################

bin:
	@echo ""
	@for file in $($@); do  \
	   ( echo "copy to \$$(ESMABIN): $$file"; \
	     $(CP) $$file $(ESMABIN))  \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

etc:
	@echo ""
	@for file in $($@); do  \
	   ( echo "copy to \$$(ESMAETC): $$file"; \
	     $(CP) $$file $(ESMAETC))  \
	done
	@echo ""
	@echo "\$$(ESMAETC) = $(ESMAETC)"
	@echo ""

grads:
	@echo ""
	@for file in $($@); do  \
	   ( echo "copy to \$$(ESMALIB)/grads: $$file"; \
	     $(CP) $$file $(ESMALIB)/grads )  \
	done
	@echo ""
	@echo "\$$(ESMALIB)/grads = $(ESMALIB)/grads"
	@echo ""

perlpkg:
	@echo ""
	@for pkg in $($@); do  \
	   ( echo "perl package to \$$(ESMABIN): $$pkg"; \
	     $(CP) $$pkg $(ESMABIN)/$$pkg )  \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

perlscript:
	@echo ""
	@for scr in $($@); do  \
	   ( $(SED) -e "$(SEDCMD)"< $$scr > $(ESMABIN)/$$scr; \
	     echo "perl script to \$$(ESMABIN): $$scr";  \
	     chmod 755 $(ESMABIN)/$$scr ) \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

perl:
	@if [ "$(PERLSCRIPT)" != "" ]; then (\
	   gmake -f \$(GNUMAKEFILE) perlscript \
	) fi

	@if [ "$(PERLPKG)" != "" ]; then (\
	   gmake -f \$(GNUMAKEFILE) perlpkg \
	) fi

#                  --------------------
#                  User Defined Targets
#                  --------------------

FOPT = $(FOPT3)
USER_FFLAGS = $(BIG_ENDIAN) $(M). $(M)$(INC_MPEU) $(I)$(INC_SDF)

argopt: argopt.c
	$(CC) -o argopt argopt.c

fresolv.x : fresolv.o
	$(LD) $(LDFLAGS) -o fresolv.x fresolv.o $(LIB_EU) $(LIB_SYS)

gethdftinc.x : gethdftinc.o
	$(LD) $(LDFLAGS) -o gethdftinc.x gethdftinc.o $(LIB_SDF) $(LIB_SYS)

zeit_ci.x: zeit_ci.o zeit_usage.o
	$(LD) $(LDFLAGS) -o zeit_ci.x zeit_ci.o zeit_usage.o $(LIB_SYS)

zeit_co.x: zeit_co.o zeit_usage.o
	$(LD) $(LDFLAGS) -o zeit_co.x zeit_co.o zeit_usage.o $(LIB_SYS)

zeit_pr.x: zeit_pr.o zeit_usage.o zeit_read.o zeit_jday.o 
	$(LD) $(LDFLAGS) -o zeit_pr.x zeit_pr.o zeit_usage.o \
                            zeit_read.o zeit_jday.o $(LIB_EU) $(LIB_SYS)

#
# Note: For consistency with the GEOS-5 restarts, these are compiled
#       without big_endian conversion.
#

mkdrstdate.x : mkdrstdate.f
	$(FC) -o $@ $(patsubst $(BIG_ENDIAN),,$(fFLAGS)) $< $(LIB_SYS)

rst_date : rst_date.f
	$(FC) -o $@ $(patsubst $(BIG_ENDIAN),,$(fFLAGS)) $< $(LIB_SYS)


  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
