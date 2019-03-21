# Add GEOSAGCM CMake modules
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# Find required packages
find_package(NetCDF REQUIRED COMPONENTS F90)
find_package(MPI REQUIRED)
find_package(OpenMP REQUIRED)
find_package(Threads REQUIRED)
find_package(ESMF REQUIRED)
find_package(GFTL REQUIRED)

# Import ESMA macros
include(esma_check_if_debug)
include(esma_set_this)
include(esma_add_subdirectories)
include(esma_add_library)
include(esma_generate_automatic_code)
include(esma_create_stub_component)
include(esma_fortran_generator_list)
include(${CMAKE_Fortran_COMPILER_ID})

# Miscellaneous configuration
set(F2PY_SUFFIX .so)
set(INC_NETCDF ${NETCDF_F90_INCLUDE_DIR})
set(INC_ESMF ${ESMF_INCLUDE_DIRS})
set(INC_gFTL ${GFTL_INCLUDE_DIRS})

# Build subcomponents 
esma_check_if_debug()

esma_add_subdirectories(
  GMAO_mpeu
  GMAO_pilgrim
  # GMAO_etc
  GMAO_pFIO 
  MAPL_Base 
  GEOS_Shared
  GMAO_hermes
  # GEOS_Util 
  # Chem_Base
  # Chem_Shared
  # LANL_Shared
  # GMAO_perllib
)

# if (PFUNIT)
#   add_subdirectory (MAPL_pFUnit EXCLUDE_FROM_ALL)
# endif ()

# Special cases - MAPL_cfio, GMAO_gfio, and GFDL_fms are built twice with two
# different precisions.
add_subdirectory (MAPL_cfio MAPL_cfio_r4)
add_subdirectory (MAPL_cfio MAPL_cfio_r8)

add_subdirectory (GMAO_gfio GMAO_gfio_r4)
add_subdirectory (GMAO_gfio GMAO_gfio_r8)
add_dependencies (GMAO_gfio_r4 GMAO_gfio_r8)

add_subdirectory (GFDL_fms GFDL_fms_r4)
add_subdirectory (GFDL_fms GFDL_fms_r8)
