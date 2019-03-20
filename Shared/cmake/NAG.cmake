if (CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 6.0)
  message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} version must be at least 6.0!")
endif()

set (FREAL8 "-r8")
set (FINT8 "-i8")
set (PP    "-fpp")
set (DUSTY "-dusty")
set (MISMATCH "-mismatch_all")
set (DISABLE_FIELD_WIDTH_WARNING)
set (CRAY_POINTER "")
set (EXTENDED_SOURCE "-132")

####################################################

set (common_Fortran_flags)
set (common_Fortran_fpe_flags "${DUSTY}")

set (GEOS_Fortran_Debug_Flags "-O0 -g")# -C=all") # -C=undefined")

set (GEOS_Fortran_Release_Flags "-O3 -g")

# Until good options can be found, make vectorize equal common flags
set (GEOS_Fortran_Vect_Flags ${GEOS_Fortran_Release_Flags})
set (GEOS_Fortran_Vect_FPE_Flags ${common_Fortran_fpe_flags})

set (GEOS_Fortran_FLAGS_DEBUG   "${GEOS_Fortran_Debug_Flags} ${common_Fortran_flags} ${common_Fortran_fpe_flags} ${ALIGNCOM}")
set (GEOS_Fortran_FLAGS_RELEASE "${GEOS_Fortran_Release_Flags} ${common_Fortran_flags} ${common_Fortran_fpe_flags} ${ALIGNCOM}")
set (GEOS_Fortran_FLAGS_VECT    "${GEOS_Fortran_Vect_Flags} ${common_Fortran_flags} ${GEOS_Fortran_Vect_FPE_Flags} ${ALIGNCOM}")

set (CMAKE_Fortran_FLAGS_DEBUG   "${GEOS_Fortran_FLAGS_DEBUG}")
set (CMAKE_Fortran_FLAGS_RELEASE "${GEOS_Fortran_FLAGS_RELEASE}")

