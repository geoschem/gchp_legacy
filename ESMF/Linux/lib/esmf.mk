# ESMF application makefile fragment
#
# Use the following ESMF_ variables to compile and link
# your ESMF application against this ESMF build.
#
# !!! VERY IMPORTANT: If the location of this ESMF build is   !!!
# !!! changed, e.g. libesmf.a is copied to another directory, !!!
# !!! this file - esmf.mk - must be edited to adjust to the   !!!
# !!! correct new path                                        !!!
#
# Please see end of file for options used on this ESMF build
#


ESMF_VERSION_STRING=5.2.0rp2

ESMF_VERSION_MAJOR=5
ESMF_VERSION_MINOR=2
ESMF_VERSION_REVISION=0
ESMF_VERSION_PATCHLEVEL=2
ESMF_VERSION_PUBLIC='T'
ESMF_VERSION_BETASNAPSHOT='F'


ESMF_APPSDIR=/home/mlong/CfA/GIGC/ESMF/Linux/bin/binO/Linux.intel.64.openmpi.default


ESMF_F90COMPILER=mpif90
ESMF_F90LINKER=mpif90

ESMF_F90COMPILEOPTS=-align all -fPIC -traceback  -O -fPIC -m64 -mcmodel=small -threads
ESMF_F90COMPILEPATHS=-I/home/mlong/CfA/GIGC/ESMF/Linux/mod -I/home/mlong/CfA/GIGC/ESMF/Linux/include 
ESMF_F90COMPILECPPFLAGS=-DESMF_NO_INTEGER_1_BYTE -DESMF_NO_INTEGER_2_BYTE -DESMF_MPIIO -DESMF_NO_OPENMP -DSx86_64_small=1 -DESMF_OS_Linux=1
ESMF_F90COMPILEFREECPP=
ESMF_F90COMPILEFREENOCPP=
ESMF_F90COMPILEFIXCPP=
ESMF_F90COMPILEFIXNOCPP=

ESMF_F90LINKOPTS= -m64 -mcmodel=small -threads
ESMF_F90LINKPATHS=-L/home/mlong/CfA/GIGC/ESMF/Linux/lib  
ESMF_F90LINKRPATHS=-Wl,-rpath,/home/mlong/CfA/GIGC/ESMF/Linux/lib 
ESMF_F90LINKLIBS= -lmpi_cxx  -lrt -ldl
ESMF_F90ESMFLINKLIBS=-lesmf  -lmpi_cxx  -lrt -ldl

ESMF_CXXCOMPILER=mpicxx
ESMF_CXXLINKER=mpicxx

ESMF_CXXCOMPILEOPTS=-fPIC -O -DNDEBUG -fPIC -m64 -mcmodel=small -pthread
ESMF_CXXCOMPILEPATHS= -I/home/mlong/CfA/GIGC/ESMF/Linux/include  
ESMF_CXXCOMPILECPPFLAGS=-DESMF_NO_INTEGER_1_BYTE -DESMF_NO_INTEGER_2_BYTE -DESMF_MPIIO -DESMF_NO_OPENMP -DSx86_64_small=1 -DESMF_OS_Linux=1 -D__SDIR__='' -DESMF_NO_SIGUSR2

ESMF_CXXLINKOPTS= -m64 -mcmodel=small -pthread
ESMF_CXXLINKPATHS=-L/home/mlong/CfA/GIGC/ESMF/Linux/lib  -L/opt/intel/Compiler/11.1/069/lib/intel64/
ESMF_CXXLINKRPATHS=-Wl,-rpath,/home/mlong/CfA/GIGC/ESMF/Linux/lib  -Wl,-rpath,/opt/intel/Compiler/11.1/069/lib/intel64/
ESMF_CXXLINKLIBS= -lmpi_f77 -lopen-rte -lopen-pal -ldl -lnsl -lutil -limf -lm -ldl -lifport -lifcoremt -limf -lsvml -lm -lipgo -lirc -lpthread -ldl -lgcc_s -lgcc -lirc -lirc_s -ldl -lrt -ldl
ESMF_CXXESMFLINKLIBS=-lesmf  -lmpi_f77 -lopen-rte -lopen-pal -ldl -lnsl -lutil -limf -lm -ldl -lifport -lifcoremt -limf -lsvml -lm -lipgo -lirc -lpthread -ldl -lgcc_s -lgcc -lirc -lirc_s -ldl -lrt -ldl

ESMF_SO_F90COMPILEOPTS=-fPIC
ESMF_SO_F90LINKOPTS=-shared
ESMF_SO_F90LINKOPTSEXE=-Wl,-export-dynamic
ESMF_SO_CXXCOMPILEOPTS=-fPIC
ESMF_SO_CXXLINKOPTS=-shared
ESMF_SO_CXXLINKOPTSEXE=-Wl,-export-dynamic

ESMF_OPENMP_F90COMPILEOPTS= -openmp
ESMF_OPENMP_F90LINKOPTS= -openmp
ESMF_OPENMP_CXXCOMPILEOPTS= -openmp
ESMF_OPENMP_CXXLINKOPTS= -openmp

# Internal ESMF variables, do NOT depend on these!

ESMF_INTERNAL_DIR=/home/mlong/CfA/GIGC/ESMF

#
# !!! The following options were used on this ESMF build !!!
#
# ESMF_DIR: /home/mlong/CfA/GIGC/ESMF
# ESMF_OS: Linux
# ESMF_MACHINE: x86_64
# ESMF_ABI: 64
# ESMF_COMPILER: intel
# ESMF_BOPT: O
# ESMF_COMM: openmpi
# ESMF_SITE: default
# ESMF_PTHREADS: ON
# ESMF_OPENMP: OFF
# ESMF_ARRAY_LITE: FALSE
# ESMF_NO_INTEGER_1_BYTE: TRUE
# ESMF_NO_INTEGER_2_BYTE: TRUE
# ESMF_FORTRANSYMBOLS: default
# ESMF_DEFER_LIB_BUILD:   ON
# 
# ESMF environment variables pointing to 3rd party software:
