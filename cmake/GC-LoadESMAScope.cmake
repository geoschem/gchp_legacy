include ("${CMAKE_Fortran_COMPILER_ID}")
# include (check_fortran_support)
include (esma_check_if_debug)
include (esma_set_this)
include (esma_add_subdirectories)
include (esma_add_library)
include (esma_generate_automatic_code)
include (esma_create_stub_component)
include (esma_fortran_generator_list)

# Dependencies: OpenMP, MPI, NetCDF, ESMF, and GFTL

# MPI
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})

# NetCDF
set(INC_NETCDF ${NETCDF_INCLUDE_DIRS})
set(NETCDF_LIBRARIES ${NETCDF_LIBRARIES})
add_definitions(-DHAS_NETCDF4)
add_definitions(-DH5_HAVE_PARALLEL)

# ESMF
set(INC_ESMF ${ESMF_INCLUDES_DIR} ${ESMF_HEADERS_DIR} ${ESMF_MOD_DIR})

# GFTL
set(INC_gFTL ${GFTL_INCLUDE_DIR})

# Misc 
set (FV_PRECISION R8)
add_definitions(-Dsys${CMAKE_SYSTEM_NAME})
set(ENV{USE_LATEX} NO)
set(PFUNIT FALSE)
