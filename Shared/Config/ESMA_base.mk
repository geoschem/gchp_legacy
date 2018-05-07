#
# Earth System Modeling Applications (ESMA) base makefile fragment.
# This fragment defines somewhat universal macros and compilation
# rules which are then costumized for each architecture in ESMA_arch.mk.
#
# REVISION HISTORY:
#
# 06Jun2003  da Silva  First Crack
# 04apr2005  da Silva  Max patch 
# 26apr2005  da Silva  Introduced MACH
# 26apr2005  da Silva  MADE BOPT=O the default
# 08Jun2006  Stassi    Added Check Environment section
# 26Jun2006  da SIlva  Removed Assert.pl related staff; see Assert.mk instead
# 20 Nov 2014 - R. Yantosca - Remove hardwiring for netCDF, MPI
#--------------------------------------------------------------------------

#                       ----------------
#                           Preamble
#                       ----------------

  SHELL	= /bin/sh

ifndef ARCH             # Architecture, e.g., IRIX64 
  ARCH := $(shell uname -s)
endif
ifndef MACH             # Hardware type, e.g., ia64
  MACH := $(shell uname -m)
endif
ifndef SITE             # Site name, e.g., halem3
  SITE := $(shell uname -n)
endif
ifndef NODE             # same as SITE name, except sometimes SITE comes predefined
  NODE := $(shell uname -n)
endif
#ifndef BPREC
#  BPREC := 64#  Build with "-r8"
#endif
ifndef BPREC            #  Build with "-r4"
  BPREC := 32
endif

#                       ----------------
#                       Main Directories
#                       ----------------

# Installation Directories
# ------------------------
  ESMABIN = $(ESMADIR)/Config/bin
  ESMALIB = $(ESMADIR)/$(ARCH)/lib
  ESMAINC = $(ESMADIR)/$(ARCH)/include
  ESMAMOD = $(ESMADIR)/$(ARCH)/include
  ESMAETC = $(ESMADIR)/$(ARCH)/etc
  ESMADOC = $(ESMADIR)/$(ARCH)/doc
  ESMACFG = $(ESMADIR)/$(ARCH)/Config
  ESMATST = $(ESMAETC)/testsuites

# Base Libraries and utilities
# ----------------------------
  BASEBIN = $(ESMADIR)/$(ARCH)/bin
  BASELIB = $(ESMADIR)/$(ARCH)/lib
  BASEINC = $(ESMADIR)/$(ARCH)/include
  BASEMOD = $(ESMADIR)/$(ARCH)/include
  BASEETC = $(ESMADIR)/$(ARCH)/etc

#                       ----------
#                       Utilities
#                       ----------

AR          = ar
AR_FLAGS    = cr
AR_EXTRACT  = -x 
AWK         = /bin/awk
CP          = /bin/cp -f
CAT         = /bin/cat
LN          = /bin/ln
MAKE        = gmake
MKDIR       = /bin/mkdir -p
PERL        = /usr/bin/perl
RANLIB      = /usr/bin/ranlib
RM          = /bin/rm -f
SED         = /bin/sed                       
TAR         = /bin/tar
GZIP        = gzip -v
BOPT        = O
M4          = m4
FDP         = $(ESMABIN)/fdp
FDP_FLAGS   = -v
STUB        = $(ESMABIN)/mapl_stub.pl
ACG         = $(ESMABIN)/mapl_acg.pl 
ACG_FLAGS   = -v
F90SPLIT    = $(ESMABIN)/f90split.x  # split f90 file by procedure
F90AIB      = $(ESMABIN)/f90aib.x    # automatic interface block
F2PY        = f2py   # python fortran extension builder
DLLEXT      = so     # extension for shared libraries
F2PYEXT     = so     # extension for python extensions

#                     -----------------------
#                      Documentation Support
#                     ----------------------

PROTEX       = $(ESMABIN)/protex
PROTEX_FLAGS = -g -b -f
LATEX        = latex
PDFLATEX     = pdflatex
MKINDX       = makeindex
DVIPS        = dvips -Ppdf -G0 -f 
PS2PDF       = ps2pdf

#                     -----------------
#                      OPTIONAL TIMERS
#                     -----------------

ESMA_TIMER     = # command to time build steps (for compilation)
ESMA_TIMER_CI  = # command to start timer (for user to backet code segments)
ESMA_TIMER_CO  = # command to end   timer (for user to backet code segments)



#                     -----------------
#                         Libraries
#                     -----------------

LIB_SCI =
LIB_SYS =

###############################################################################
# %%%%% COMMENTED OUT BY BOB Y. (11/20/14) %%%%%
#
# We will get the netCDF include & lib paths from GC_INCLUDE and GC_LIB
# which are set in the GEOS-Chem Makefile_header.mk 
#DIR_HDF5 = $(GC_BIN)
#INC_HDF5 = $(DIR_HDF5)/../include
#LIB_HDF5 = $(wildcard $(foreach lib,hdf5_hl hdf5 z sz gpfs,\
#           $(BASELIB)/lib$(lib).a) )
#
#DIR_NETCDF = $(GC_BIN)
##./$(ARCH)
#INC_NETCDF = $(GC_INCLUDE)
##$(DIR_NETCDF)/../include
##ifeq ($(wildcard $(BASEBIN)/nc-config), )
##    LIB_NETCDF = $(BASELIB)/libnetcdf.a $(LIB_HDF5)
##else
#    LIB_NETCDF := $(shell $(GC_BIN)/nc-config --flibs)
##endif
#
##	LIB_NETCDF = -L$(GC_LIB)
##$(NCL)
###############################################################################
#------------------------------------------------------------------------------
# %%%%% ADDED BY BOB Y. (11/20/14) %%%%%
#
# Always assume we are using netCDF-4.  Take the include & link
# directory paths as computed from the Makefile_header.mk.
INC_NETCDF :=$(GC_INCLUDE)
ifdef GC_F_INCLUDE
   INC_NETCDF +=$(GC_F_INCLUDE)
endif
LIB_NETCDF :=$(NCL)
#------------------------------------------------------------------------------

###############################################################################
# %%%%% COMMENTED OUT BY BOB Y. (11/20/14) %%%%%
#
# COMMENTED OUT BY BOB Y. (11/20/14)
# We know that we are using netCDF for the data I/O, so we can skip
# all of this HDF stuff.
#DIR_HDF = $(GC_BIN)
##./$(ARCH)
#INC_HDF = $(DIR_HDF)/include/hdf
#LIB_HDF = $(wildcard $(foreach lib,mfhdf df hdfjpeg jpeg hdfz z sz,\
#          $(BASELIB)/lib$(lib).a) )
#
#ifeq ($(ESMA_SDF),hdf)
#   INC_SDF = $(INC_HDF)
#   LIB_SDF = $(LIB_HDF)
#else
#   INC_SDF = $(INC_NETCDF)
#   LIB_SDF = $(LIB_NETCDF)
#   ifneq ($(wildcard $(INC_SDF)/netcdf.inc), )
#     ifneq ($(shell grep -c netcdf4 $(INC_SDF)/netcdf.inc),0)
#        DEF_SDF += $(D)HAS_NETCDF4
#     endif
#     ifneq ($(shell grep -c 'netcdf version 3' $(INC_SDF)/netcdf.inc),0)
#        DEF_SDF += $(D)HAS_NETCDF3
#     endif
##     ifneq ($(shell grep -c 'define H5_HAVE_PARALLEL 1' $(INC_HDF5)/H5pubconf.h),0)
##        DEF_SDF += $(D)H5_HAVE_PARALLEL
##     endif
#   endif
#endif
###############################################################################
#------------------------------------------------------------------------------
# %%%%% ADDED BY BOB Y. (11/20/14) %%%%%
#
# Always assume we are using netCDF-4 (bmy, 11/20/14)
INC_SDF = $(INC_NETCDF)
LIB_SDF = $(LIB_NETCDF)
DEF_SDF += $(D)HAS_NETCDF4 
DEF_SDF += $(D)H5_HAVE_PARALLEL
#DEF_SDF += $(D)NETCDF_NEED_NF_MPIIO
#------------------------------------------------------------------------------

DIR_ESMF := $(ESMF_DIR)
INC_ESMF := $(DIR_ESMF)/$(ARCH)/include/ 
MOD_ESMF := $(DIR_ESMF)/$(ARCH)/mod/
LIB_ESMF := $(DIR_ESMF)/$(ARCH)/lib/libesmf.so

###############################################################################
# %%%%% COMMENTED OUT BY BOB Y. (11/20/14) %%%%%
#
# Don't rely on hardwired MPI paths & libraries.
#INC_MPI = /usr/include
#LIB_MPI = -lmpi
###############################################################################
#------------------------------------------------------------------------------
# %%%%% ADDED BY BOB Y. (11/20/14) %%%%%
#
# Now query for the correct MPI info (bmy, 11/20/14)
	FC := mpif90
        ifeq ($(ESMF_COMM),mvapich2)
           INC_MPI := $(MPI_ROOT)/include
           LIB_MPI := -L$(MPI_ROOT)/lib  -lmpich
        else ifeq ($(ESMF_COMM),mpich)
           INC_MPI := $(MPI_ROOT)/include
           LIB_MPI := -L$(MPI_ROOT)/lib  -lmpich
        else ifeq ($(ESMF_COMM),mpich2)
           INC_MPI := $(MPI_ROOT)/include
           LIB_MPI := -L$(MPI_ROOT)/lib  -lmpich
        else ifeq ($(ESMF_COMM),openmpi)
           INC_MPI := $(shell mpif90 --showme:incdirs)
           LIB_MPI := $(shell mpif90 --showme:link)
           LIB_MPI += $(shell mpicxx --showme:link)
        else ifeq ($(ESMF_COMM),mpi)
           # Generic MPI
           INC_MPI := $(MPI_ROOT)/include
           LIB_MPI := -L$(MPI_ROOT)/lib  -lmpi -lmpi++
        else
           $(error Bad ESMF_COMM in ESMA_base.mk)
        endif
#------------------------------------------------------------------------------


DIR_THIS := $(shell basename `pwd`)
INC_THIS = $(ESMAINC)/$(DIR_THIS)
LIB_THIS = $(ESMALIB)/lib$(DIR_THIS).a

#                     -----------------------
#                     C Compiler/Loader Flags
#                     -----------------------

CDEFS     = -Dsys$(ARCH) -DESMA$(BPREC) $(USER_CDEFS)
CINCS     = $(foreach dir,$(INC_ESMF), $(I)$(dir)) $(USER_CINCS)

COPT0 = -O0
COPT1 = -O1
COPT2 = -O2
COPT3 = -O3
COPT4 = -O4
COPT5 = -O5
ifeq ("$(BOPT)","g")
   COPT   = -g
else
   COPT   = -O
endif

CC        = gcc
CXX       = g++
CPP       = cpp
PP        = -$(CPP)

CFLAGS    = $(CDEFS) $(CINCS) $(COPT) $(USER_CFLAGS)
CXXFLAGS  = $(CDEFS) $(CINCS) $(COPT) $(USER_CFLAGS)


#                     -------------------------
#                     f90 Compiler/Loader Flags
#                     -------------------------

I = -I# f90 compiler option for include file path
M = -I# f90 compiler option for module  file path
D = -D# f90 compiler option for cpp defines
DC = $(D)

FOPTG = -g
FOPT0 = -O0
FOPT1 = -O1
FOPT2 = -O2
FOPT3 = -O3
FOPT4 = -O4
FOPT5 = -O5
ifeq ("$(BOPT)","g")
   FOPT   = $(FOPTG)
else
   FOPT   = $(FOPT3)
endif

BIG_ENDIAN  =
BYTERECLEN  =
OMPFLAG     =
FREAL4      = 
ifeq ("$(ESMF_COMPILER)","intel")
  FREAL8      = -r8
  FREE        =
  CPPANSIX    = -ansi -DANSI_CPP 
else ifeq ("$(ESMF_COMPILER)","gfortran")
  FREAL8      = -fdefault-real-8 -fdefault-double-8
  FREE        = -ffree-form -ffree-line-length-none -Wno-line-truncation -fno-range-check
  CPPANSIX    = -std=gnu11 -nostdinc -C
else
  FREAL8      =
  FREE        =
  CPPANSIX    = -ansi -DANSI_CPP 
endif
ifeq ( "$(BPREC)","32" )
      FREAL = $(FREAL4)
else
      FREAL = $(FREAL8)
endif
FINT4       = 
FINT8       = -i8
FINT        = $(FINT4)

FDEFS     = $(D)sys$(ARCH) $(D)ESMA$(BPREC) $(DEF_SDF) $(USER_FDEFS)
FINCS     = $(foreach dir,$(INC_ESMF), $(I)$(dir)) $(USER_FINCS)
FMODS     = $(foreach dir,$(MOD_ESMF), $(M)$(dir)) $(USER_FMODS)
XFLAGS    = 

#FC        = f90
fFLAGS    = $(FDEFS) $(FINCS) $(FMODS) $(FOPT) $(FREAL) $(FINT) $(XFLAGS) $(USER_FFLAGS)
f90FLAGS  = $(FDEFS) $(FINCS) $(FMODS) $(FOPT) $(FREAL) $(FINT) $(XFLAGS) $(USER_FFLAGS) $(FREE)
FFLAGS    = $(FDEFS) $(FINCS) $(FMODS) $(FOPT) $(FREAL) $(FINT) $(XFLAGS) $(USER_FFLAGS)
F90FLAGS  = $(FDEFS) $(FINCS) $(FMODS) $(FOPT) $(FREAL) $(FINT) $(XFLAGS) $(USER_FFLAGS) $(FREE)

FPP = /lib/cpp 
FPPFLAGS = -P $(DC)sys$(ARCH) $(FDEFS) $(FINCS) $(foreach dir,$(INC_MPI), $(I)$(dir))

LD = $(FC)
LDPATH  = -L$(BASELIB) -L$(ESMALIB)
LDFLAGS = $(LDPATH) $(USER_LDFLAGS)

#                     -----------------
#                     Compilation Rules
#                     -----------------

.SUFFIXES:
.SUFFIXES: .P90 .m4 .F90 .f90 .F .f .c .o .H .h .d .tex .dvi .pdf 

.c.o:
	$(ESMA_TIMER) $(CC) -c $(CFLAGS) $<

.C.o:
	$(ESMA_TIMER) $(CXX) -c $(CXXFLAGS) $<

.f.o:
	$(ESMA_TIMER) $(FC) -c $(fFLAGS) $<

.F.o:
	$(ESMA_TIMER) $(FC) -c $(FFLAGS) $<

.f90.o:
	$(ESMA_TIMER) $(FC) -c $(f90FLAGS) $<

.F90.o:
	$(ESMA_TIMER) $(FC) -c $(F90FLAGS) $<

.P90.o:
	@sed -e "/\!.*'/s/'//g" $< | $(CPP) $(CPPANSIX) $(FPPFLAGS) > $*___.f90
	$(ESMA_TIMER) $(FC) -c $(f90FLAGS) -o $*.o $*___.f90
	@$(RM) $*___.f90

.H.h:
	$(FPP) $(FPPFLAGS) $*.H > $*.h

.m4.o:
	$(M4) $(M4FLAGS) $*.m4 > $*.F90
	$(FC) -c $(F90FLAGS) $*.F90
	$(RM) $*.F90

.c.d:
	@$(PERL) $(FDP) $(FDP_FLAGS) -c $<

.f.d:
	@$(PERL) $(FDP) $(FDP_FLAGS) -c $<

.F.d:
	-@$(CPP) $(FPPFLAGS) $< > $*___.f
	@$(PERL) $(FDP) -i $< $(FDP_FLAGS) -c $*___.f
	@$(RM) $*___.f

.f90.d:
	@$(PERL) $(FDP) $(FDP_FLAGS) -c $<

.F90.d:
	-@$(CPP) $(FPPFLAGS) $< > $*___.f90
	@$(PERL) $(FDP) -i $< $(FDP_FLAGS) -c $*___.f90
	@$(RM) $*___.f90

.P90.d:
	@$(PERL) $(FDP) -i $< $(FDP_FLAGS) -c $<

.m4.d:
	$(M4) $(M4FLAGS) $*.m4 > $*___.F90
	-@$(FPP) $(FPPFLAGS) $*___.F90 > $*___.f90
	@$(PERL) $(FDP) -i $< $(FDP_FLAGS) -c $*___.f90
	@$(RM) $*___.f90 $*___.F90


%___.lst : %.F90
	$(F90SPLIT) < $< > $*___.lst  

%Interfaces___.h : %.F90
	$(F90AIB) < $< | $(SED) -e "s/$*_L2/$*_L1/1" > $@


%.tex : %.f
	$(PROTEX) $(PROTEX_FLAGS) -f $< > $*.tex

%.tex : %.f90
	$(PROTEX) $(PROTEX_FLAGS) -f $< > $*.tex

%.tex : %.F
	$(CPP) -D__PROTEX__ $(FPPFLAGS) $(I)$(INC_ESMF) $< | \
        $(PROTEX) $(PROTEX_FLAGS) -f > $*.tex

%.tex : %.F90
	$(CPP) -D__PROTEX__ $(FPPFLAGS) $(I)$(INC_ESMF) $< | \
        $(PROTEX) $(PROTEX_FLAGS) -f > $*.tex

%.tex: %.h
	$(CPP) -D__PROTEX__ $(FPPFLAGS) $(I)$(INC_ESMF) $< | \
        $(PROTEX) $(PROTEX_FLAGS) -f > $*.tex

%.dvi: %.tex
	$(LATEX) $*
	$(LATEX) $*

%.pdf: %.dvi
	$(DVIPS) $*.dvi -o $*.ps # going thru ps tend to produce searchable PDF
	$(PS2PDF) $*.ps
	$(RM) -rf $*.ps

