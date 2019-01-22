  ################################
  #                              #
  #    Arch Specific Defaults    #
  #                              #
  ################################

  # -----
  # Linux
  # -----

  ifeq ($(ARCH),Linux)
     MKL_OS = lnx

     # When building for profiling, use BOPT=Og
     # ----------------------------------------
     ifeq ("$(DOING_APROF)","yes")
        BOPT = Og
        LIB_APROF = -Wl,@$(BASELIB)/allinea-profiler.ld
     endif

     # MAT: I do not think these are needed on modern
     #      builds of the model. Carefully make sure 
     #      before deleting
     #
     # Add -lgpfs to LIB_HDF5 on borg/discover nodes
     # ---------------------------------------------
     ifeq ($(findstring borg,$(NODE)),borg)
        LIB_HDF5 += -lgpfs
     endif
     ifeq ($(findstring discover,$(NODE)),discover)
        LIB_HDF5 += -lgpfs
     endif

  endif

