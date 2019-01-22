#
# Earth System Modeling Applications (ESMA) base makefile fragment.
# This fragment customizes ESMF_base.mk for each each architecture. 
#
#--------------------------------------------------------------------------

  #######################
  #                     #
  # C Compiler Defaults #
  #                     #
  #######################
  
  CC  = gcc
  CXX = g++
  CPP = cpp

  #################################
  #                               #
  #    Compiler Specific Flags    #
  #                               #
  #################################

  ##################################
  # Supported and Tested Compilers #
  ##################################

  # -------------
  # Intel Fortran
  # -------------
  
  include $(ESMACFG)/ifort.mk

  # -----------
  # GNU Fortran
  # -----------

  include $(ESMACFG)/gfortran.mk

  # -----------
  # PGI Fortran
  # -----------

  include $(ESMACFG)/pgfortran.mk

  ##########################
  # Compilers Being Tested #
  ##########################

  # -----------
  # NAG Fortran
  # -----------

  include $(ESMACFG)/nagfor.mk

  ##########################
  # Unmaintained Compilers #
  ##########################

  #  Note: Unmaintained, but kept
  #        as a stub for possible
  #        future use.

  # ---------------
  # Absoft compiler
  # ---------------
  
  include $(ESMACFG)/absoft.mk

  # ------------
  # Cray Fortran
  # ------------

  include $(ESMACFG)/cray.mk

