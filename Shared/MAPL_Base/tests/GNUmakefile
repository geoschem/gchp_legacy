#! /bin/make

# Machine/Location specific part

# f90 compiler
FC = ifort
LD = $(FC)
# include module
M = -I
ARCH = Linux

DIR_MAPL = ../../../../$(ARCH)
INC_MAPL = $(DIR_MAPL)/include/MAPL
LIB_MAPL = $(DIR_MAPL)/lib

DIR_NETCDF = /usr/local/pkg/netcdf/netcdf-3.6.1/icc
INC_NETCDF = $(DIR_NETCDF)/include
LIB_NETCDF = -L$(DIR_NETCDF)/lib -lnetcdf

DIR_ESMF   = /home/cnh/src/esmf_builds/ESMF_2_2_2r/esmf/
INC_ESMF   = $(DIR_ESMF)/mod/modg/Linux.intel.32.mpich.default
MOD_ESMF   = $(INC_ESMF)
LIB_ESMF   = $(DIR_ESMF)/lib/libg/Linux.intel.32.mpich.default/libesmf.a


DIR_MPI    = /usr/local/pkg/mpich/mpich-intel
INC_MPI    = $(DIR_MPI)/include
LIB_MPI    = -L$(DIR_MPI)/lib -lpmpich -lpmpich++ -lmpich


LIB_SYS    =  -lpthread -lrt

.SUFFIXES: .F90


SRCS = utCFIO_Bundle.F90
OBJS = $(SRCS:.F90=.o) 
BIN = $(SRCS:.F90=.x) 

FINCS = -I$(INC_MAPL)
FFLAGS = $(M). $(M)$(INC_MAPL) $(M)$(MOD_ESMF)
            
LIB_COMP = -L$(LIB_MAPL) -lMAPL -lMAPL_cfio_r4


LIB_SDF = $(LIB_SDF)	   

$(BIN) bin: $(OBJS)
	$(LD) -o $(BIN) $(LDFLAGS) $(OBJS) $(LIB_COMP) $(LIB_ESMF) $(LIB_SDF) $(LIB_MPI) $(LIB_SCI) $(LIB_SYS)

#                  --------------------
#                      Dependencies
#                  --------------------
.F90.o:
	$(FC) -c $(FFLAGS) $(FINCS) $<

utCFIO_Bundle.o: utCFIO_Bundle.F90

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
