  ######################
  #                    #
  #    TAU Profiler    #
  #                    #
  ######################

  # NOTE: TAU must be built per-compiler combination. This is here for convenience
  #       but TAUROOTDIR and PDTROOTDIR will need to change

  ifeq ($(ESMA_PROFILE),TAU)
     ORIGFC := $(ESMA_FC)
     export ORIGFC
     TAUROOTDIR   = /discover/swdev/mathomp4/tau-2.23/intel-14.0.1.106_mvapich2-2.0b
     PDTROOTDIR   = /discover/swdev/mathomp4/src/pdtoolkit-3.20/x86_64/intel14.0.1.106

     TAU_MAKEFILE =  $(TAUROOTDIR)/x86_64/lib/Makefile.tau-icpc-mpi-pdt

     #TAU_OPTIONS  = '-optPdtF90Parser="$(PDTROOTDIR)/bin/f95parse" -optPreProcess -optVerbose -optPDTInst -optTauSelectFile="$(ESMADIR)/src/Config/select.tau"'
     TAU_OPTIONS  = '-optPdtF90Parser="$(PDTROOTDIR)/bin/f95parse" -optPreProcess -optVerbose -optPDTInst -optRevert'
     #TAU_OPTIONS  = '-optPdtF90Parser="$(PDTROOTDIR)/bin/f95parse" -optPreProcess -optVerbose -optPDTInst -optRevert -optKeepFiles'
     export TAU_OPTIONS
     FC = $(TAUROOTDIR)/x86_64/bin/tau_f90.sh -tau_options=$(TAU_OPTIONS) -tau_makefile=$(TAU_MAKEFILE)
     F2PY = f2py --f77exec=$(ORIGFC) --f90exec=$(ORIGFC)
  endif


