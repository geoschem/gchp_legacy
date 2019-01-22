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
message("${srcs_and_includes}")

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

add_library (${lib} STATIC ${srcs})

set_target_properties (${lib} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
target_link_libraries (${lib} ${NETCDF_LIBRARIES})

include_directories (shared/include)
include_directories (shared/mpp/include)
include_directories (shared/fft)
include_directories (${INC_NETCDF})

file (COPY shared/include/fms_platform.h DESTINATION ${include_${this}})

# Various compiler options and CPP definitions ...
add_definitions (-DOVERLOAD_R4 -DMAPL_MODE -DEIGHT_BYTE -DSPMD -DTIMING -Duse_libMPI -Duse_netCDF)

if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  target_compile_options (${lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fcray-pointer>)
endif ()
if (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  target_compile_options (${lib}  PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-stack_temps -safe_cray_ptr -assume byterecl>)
  target_compile_options (${lib}  PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fp-model source -ftz -align all,dcommons -fno-alias>)
  target_compile_options (${lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-diag-disable 6843,7712>) # warnings about dummy args
endif ()
if (CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
   add_definitions (-DNO_QUAD_PRECISION)
endif ()

if (precision MATCHES "r8")
  string (REPLACE " " ";" flags ${FREAL8})
  foreach (flag ${flags})
    target_compile_options (${lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
  endforeach ()
endif ()

target_compile_options (${lib} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${DISABLE_FIELD_WIDTH_WARNING}>)




  
