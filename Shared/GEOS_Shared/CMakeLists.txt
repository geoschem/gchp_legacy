esma_set_this()

set (srcs
  GEOS_Utilities.F90 DragCoefficients.F90 GEOS.F90
  tropovars.F90 surfacelayer.F90 GEOS_TopoGet.F90
  GEOS_GmapMod.F90 GEOS_RemapMod.F90 G3_MPI_Util_Mod.F90 windfix.F90
  PWSSSP.F sphere.F calcdbz.F
  )

esma_add_library(${this} SRCS ${srcs} DEPENDENCIES MAPL_Base)

# other dependencies
target_include_directories(${this} PUBLIC ${INC_ESMF} ${INC_NETCDF})

# special cases
set_property(SOURCE PWSSP.F APPEND_STRING PROPERTY COMPILE_FLAGS ${FREAL8})
set_property(SOURCE sphere.F APPEND_STRING PROPERTY COMPILE_FLAGS ${FREAL8})



