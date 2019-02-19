esma_set_this ()
string (REPLACE GFDL_fms_ "" precision ${this})

# The following logic is to avoid duplication in the supported GNUMake logic.
# Source files (and headers) are listed in a separate file, and we must
# process this into something that CMake can use for assembling a library.

# 1) Read the list in a variable
file (READ path_names srcs_and_includes)

# 2) Convert to a proper list without blank lines
string (REGEX REPLACE "\\.\\.\\/" "" srcs_and_includes "${srcs_and_includes}")
string (REGEX REPLACE ";" "\\\\;" srcs_and_includes "${srcs_and_includes}")
string (REGEX REPLACE " *\n" ";" srcs_and_includes "${srcs_and_includes}")

# 3) Filter out header files
set (srcs)
foreach (file ${srcs_and_includes})
  string (REGEX MATCH ".+\\.h" header ${file})
  string (REGEX MATCH ".+\\.inc" inc ${file})
  if (header OR inc)
  else ()
    list (APPEND srcs ${file})
  endif ()
endforeach ()

set (lib GFDL_fms_${precision})

esma_add_library (${lib} SRCS ${srcs})
target_link_libraries (${lib} ${NETCDF_LIBRARIES})

target_include_directories (${lib} PUBLIC shared/include shared/mpp/include shared/fft)
target_include_directories (${lib} PUBLIC ${INC_NETCDF})

file (COPY shared/include/fms_platform.h DESTINATION ${include_${this}})
file (COPY shared/include/file_version.h DESTINATION ${include_${this}})

# Various compiler options and CPP definitions ...
target_compile_definitions (${lib} PRIVATE OVERLOAD_R4 MAPL_MODE EIGHT_BYTE SPMD TIMING use_libMPI use_netCDF)

set (CMAKE_Fortran_FLAGS_RELEASE "${GEOS_Fortran_FLAGS_VECT}")

if (CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
  target_compile_options (${lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-kind=byte>)
endif ()
if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  target_compile_options (${lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fcray-pointer>)
endif ()
if (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  target_compile_options (${lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-diag-disable 6843,7712>) # warnings about dummy args
endif ()
if (CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
  target_compile_definitions (${lib} PRIVATE NO_QUAD_PRECISION)
endif ()

if (precision MATCHES "r8")
  esma_fortran_generator_list(${lib} ${FREAL8})
endif ()





  
