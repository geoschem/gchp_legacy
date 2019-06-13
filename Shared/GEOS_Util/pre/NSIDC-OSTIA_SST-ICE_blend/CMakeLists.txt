esma_set_this ()

add_library (${this}
  bin2bin.F90
  check.F90
  fill_Land.F90
  hflip.F90
  interp_to_eight_deg.F90
  proc_SST_FRACI.F90
  proc_SST_FRACI_ostia_quart.F90
  read_input.F90
  read_input_quart.F90
  read_Ostia.F90
  read_Ostia_quart.F90
  read_Reynolds.F90
  )

include_directories (${include_MAPL_Base})
include_directories (${INC_ESMF})
include_directories (${INC_NETCDF})
target_link_libraries (${this} MAPL_Base ${NETCDF_LIBRARIES})


add_executable (regrid_forcing.x   regrid_forcing.F90)
target_link_libraries (regrid_forcing.x ${this})

add_executable (sst_sic_EIGTHdeg.x proc_SST_FRACI.F90)
target_link_libraries (sst_sic_EIGTHdeg.x ${this})

add_executable (sst_sic_QUARTdeg.x proc_SST_FRACI_ostia_quart.F90)
target_link_libraries (sst_sic_QUARTdeg.x ${this})
