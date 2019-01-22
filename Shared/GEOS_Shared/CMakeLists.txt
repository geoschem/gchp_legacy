get_filename_component(this "${CMAKE_CURRENT_SOURCE_DIR}" NAME)

add_library (${this}
  GEOS_Utilities.F90 DragCoefficients.F90 GEOS.F90
  tropovars.F90 surfacelayer.F90 GEOS_TopoGet.F90
  GEOS_GmapMod.F90 GEOS_RemapMod.F90 G3_MPI_Util_Mod.F90 windfix.F90
  PWSSSP.F sphere.F calcdbz.F
  )

target_link_libraries (${this} MAPL_Base)
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

set_property(SOURCE PWSSP.F APPEND_STRING PROPERTY COMPILE_FLAGS ${FREAL8})
set_property(SOURCE sphere.F APPEND_STRING PROPERTY COMPILE_FLAGS ${FREAL8})

include_directories(${INC_ESMF} ${INC_NETCDF})
include_directories(${include_MAPL_Base})
include_directories(${include_GEOS_Shared})


