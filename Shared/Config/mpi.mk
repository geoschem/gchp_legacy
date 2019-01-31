  ######################
  #                    #
  #    MPI Defaults    #
  #                    #
  ######################

  ifdef I_MPI_ROOT
      FC := mpiifort
      INC_MPI := $(I_MPI_ROOT)/include64
      LIB_MPI := -L$(I_MPI_ROOT)/lib64 -lmpifort -lmpi # Intel MPI
      LIB_MPI_OMP := -L$(I_MPI_ROOT)/lib64 -lmpifort -lmpi_mt # Intel MPI
  else
  ifdef OPENMPI
      FC := mpifort
      INC_MPI := $(OPENMPI)/include
      OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link)
      OPENMPI_CXX_LINK_FLAGS := $(shell mpicxx -showme:link)
      LIB_MPI := -L$(OPENMPI)/lib $(OPENMPI_LINK_FLAGS) $(OPENMPI_CXX_LINK_FLAGS)
  else
  ifdef MVAPICH2
      FC := mpif90
      INC_MPI := $(MVAPICH2)/include
      LIB_MPI := -L$(MVAPICH2)/lib  -lmpich
  else
  ifdef MPICH
      FC := mpifort
      INC_MPI := $(MPICH)/include
      LIB_MPI := -L$(MPICH)/lib  -lmpifort -lmpi
  else
  ifdef M_MPI_ROOT
      FC := mpif90
      INC_MPI := $(M_MPI_ROOT)/include
      LIB_MPI := -L$(M_MPI_ROOT)/lib  -lmpich
  else
  ifdef MPI_HOME
      FC := mpif90
      INC_MPI := $(MPI_HOME)/include
      # Customize for GCHP (ewl, 1/31/19)
      #LIB_MPI := -L$(MPI_HOME)/lib  -lmpich
      LIB_MPI := -L$(MPI_HOME)/lib  -lmpi -lmpi++
  else
  # This detects the MPT setup at NCCS
  ifdef MPT_VERSION
      FC := mpif90
      INC_MPI := $(MPI_ROOT)/include
      LIB_MPI := -L$(MPI_ROOT)/lib  -lmpi -lmpi++
  else
  ifdef FPATH
      FC := mpif90 
      INC_MPI := $(dir $(shell which mpiexec_mpt))../include
      LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpi -lmpi++
  # All fails, assume Open MPI
  else
      FC := mpifort

      OPENMPI :=   $(shell ompi_info --parsable --path prefix | cut -d: -f3)
      INC_MPI :=   $(shell ompi_info --parsable --path incdir | cut -d: -f3)
      LIB_MPI := -L$(shell ompi_info --parsable --path libdir | cut -d: -f3)

      OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link)
      OPENMPI_CXX_LINK_FLAGS := $(shell mpicxx -showme:link)
      LIB_MPI += $(OPENMPI_LINK_FLAGS) $(OPENMPI_CXX_LINK_FLAGS)

  endif # FPATH
  endif # MPT_VERSION
  endif # MPI_HOME
  endif # M_MPI_ROOT
  endif # MPICH
  endif # MVAPICH2
  endif # OPENMPI
  endif # I_MPI_ROOT

