get_filename_component(this "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
esma_set_include (${this})

add_library (${this}
  shr_kind_mod.F90
  debugutilitiesmodule.F90
  timingModule.F90
  decompmodule.F90
  ghostmodule.F90
  mod_comm.F90
  parutilitiesmodule.F90
  PILGRIM_xyMod.F90
  redistributemodule.F90
  )

target_compile_definitions (GMAO_pilgrim PRIVATE -DSPMD -DSTAND_ALONE -DMODCM_ALLOC)

target_link_libraries (GMAO_pilgrim ${MPI_Fortran_LIBRARIES})
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})


