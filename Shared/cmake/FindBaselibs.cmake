set (BASEDIR /does-not-exist CACHE PATH "Path to installed baselibs _including_ OS subdirectory (Linux or Darwin).")

if (NOT EXISTS ${BASEDIR})
  message (FATAL_ERROR "ERROR: Must specify a value for BASEDIR with cmake ... -DBASEDIR=<path>.")
endif ()
if (ESMA_SDF)
  message (FATAL_ERROR "ERROR: -hdf option was thought to be obsolete when CMake was crafted.")
endif ()

link_directories (${BASEDIR}/lib)

#------------------------------------------------------------------
# netcdf
# The following command provides the list of libraries that netcdf
# uses.  Unfortunately it also includes the library path and "-l"
# prefixes, which CMake handles in a different manner. So we need so
# strip off that item from the list
execute_process (
  COMMAND ${BASEDIR}/bin/nf-config --flibs
  OUTPUT_VARIABLE LIB_NETCDF
  )

string(REGEX MATCHALL " -l[^ ]*" _full_libs "${LIB_NETCDF}")
set (NETCDF_LIBRARIES)
foreach (lib ${_full_libs})
  string (REPLACE "-l" "" _tmp ${lib})
  string (STRIP ${_tmp} _tmp)
  list (APPEND NETCDF_LIBRARIES ${_tmp})
endforeach()
#------------------------------------------------------------------

set (INC_HDF5 ${BASEDIR}/include/hdf5)
set (INC_NETCDF ${BASEDIR}/include/netcdf)
set (INC_HDF ${BASEDIR}/include/hdf)
set (INC_ESMF ${BASEDIR}/include/esmf)

set(gFTL "${BASEDIR}/gFTL" CACHE STRING "gFTL")
set (INC_gFTL ${gFTL}/include)
if (NOT EXISTS ${INC_gFTL})
    message (FATAL_ERROR  "gFTL directory, ${INC_gFTL} does not exist.")
endif ()

set (INC_FLAP ${BASEDIR}/include/FLAP)
set (LIB_FLAP ${BASEDIR}/lib/libflap.a)
if (NOT EXISTS ${INC_FLAP})
  message (FATAL_ERROR  "FLAP directory, ${INC_FLAP} does not exist.")
endif ()

if (EXISTS ${INC_NETCDF})
  execute_process (COMMAND grep -c netcdf4 ${INC_NETCDF}/netcdf.inc OUTPUT_VARIABLE _val)
  if (_val MATCHES "0")
    add_definitions(-DHAS_NETCDF4)
  endif ()
  execute_process (COMMAND grep -c "netcdf4 version 3" ${INC_NETCDF}/netcdf.inc OUTPUT_VARIABLE _val)
  if (_val MATCHES "0")
    add_definitions(-DHAS_NETCDF3)
  endif ()
  execute_process (COMMAND grep -c "define H5_HAVE_PARALLEL 1" ${INC_HDF5}/H5pubconf.h OUTPUT_VARIABLE _val)
  if (_val MATCHES "0")
    add_definitions(-DH5_HAVE_PARALLEL)
  endif ()
  if (EXISTS ${INC_NETCDF}/netcdf_par.h)
    add_definitions(-DNETCDF_NEED_NF_MPIIO)
  endif ()
endif ()

add_definitions(-DHAS_NETCDF4)
add_definitions(-DHAS_NETCDF3)
add_definitions(-DH5_HAVE_PARALLEL)
add_definitions(-DNETCDF_NEED_NF_MPIIO)
add_definitions (-DHAS_NETCDF3)


if (APPLE)
  if (NOT "${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
    execute_process (COMMAND ${CMAKE_C_COMPILER} --print-file-name=libgcc.a OUTPUT_VARIABLE libgcc OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif ()
  execute_process (COMMAND ${CMAKE_CXX_COMPILER} --print-file-name=libstdc++.dylib OUTPUT_VARIABLE stdcxx OUTPUT_STRIP_TRAILING_WHITESPACE)
else ()
  execute_process (COMMAND ${CMAKE_CXX_COMPILER} --print-file-name=libstdc++.so OUTPUT_VARIABLE stdcxx OUTPUT_STRIP_TRAILING_WHITESPACE)
endif ()

# For OS X - use the gcc stdc++ library - not clang
#find_library (STDCxx
#  stdc++
#  HINTS "/opt/local/lib/gcc5"
#  )

# We must statically link ESMF on Apple due mainly to an issue with how Baselibs is built.
# Namely, the esmf dylib libraries end up with the full *build* path on Darwin (which is in 
# src/esmf/lib/libO...) But we copy the dylib to $BASEDIR/lib. Thus, DYLD_LIBRARY_PATH gets
# hosed. yay.

if (APPLE)
   set (ESMF_LIBRARY ${BASEDIR}/lib/libesmf.a)
else ()
   set (ESMF_LIBRARY esmf_fullylinked)
endif ()
set (ESMF_LIBRARIES ${ESMF_LIBRARY} ${NETCDF_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${MPI_CXX_LIBRARIES} ${stdcxx} ${libgcc})


if (PFUNIT)
  set (PFUNIT_PATH ${BASEDIR}/pFUnit/pFUnit-mpi)
  set (PFUNIT_LIBRARY_DIRS ${PFUNIT_PATH}/lib)
  set (PFUNIT_LIBRARIES ${PFUNIT_PATH}/lib/libpfunit.a)
  set (PFUNIT_INCLUDE_DIRS ${PFUNIT_PATH}/mod ${PFUNIT_PATH}/include)
endif ()

