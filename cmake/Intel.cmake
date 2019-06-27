if (CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 15.1)
  message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} version must be at least 15.1!")
endif()

set (FREAL8 "-r8")
set (FINT8 "-i8")
set (PP    "-fpp")
set (MISMATCH "")
set (BIG_ENDIAN "-convert big_endian")
set (EXTENDED_SOURCE "-extend_source")
set (FIXED_SOURCE "-fixed")
set (DISABLE_FIELD_WIDTH_WARNING "-diag-disable 8291")
set (CRAY_POINTER "")
set (MCMODEL "-mcmodel medium -shared-intel")
set (HEAPARRAYS "-heap-arrays 32")
set (BYTERECLEN "-assume byterecl")
set (ALIGNCOM "-align dcommons")
set (TRACEBACK "-traceback")

add_definitions(-DHAVE_SHMEM)

####################################################

# Common Fortran Flags
# --------------------
set (common_Fortran_flags "${TRACEBACK} -assume realloc_lhs")
set (common_Fortran_fpe_flags "-fpe0 -fp-model source ${HEAPARRAYS} -assume noold_maxminloc")

# GEOS Debug
# ----------
set (GEOS_Fortran_Debug_Flags "-g -O0 -ftz -align all -fno-alias -debug -nolib-inline -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -check bounds -check uninit -fp-stack-check -warn unused -init=snan,arrays -save-temps")
set (GEOS_Fortran_Debug_FPE_Flags "${common_Fortran_fpe_flags}")

# GEOS Release
# ------------
set (GEOS_Fortran_Release_Flags "-O3 -g -qopt-report0 -ftz -align all -fno-alias")
#set (GEOS_Fortran_Release_FPE_Flags "${common_Fortran_fpe_flags} -fimf-arch-consistency=true")

# GEOS Vectorize
# --------------
#set (GEOS_Fortran_Vect_Flags "-O3 -g -xCORE-AVX2 -fma -qopt-report0 -ftz -align all -fno-alias -align array32byte")
#set (GEOS_Fortran_Vect_FPE_Flags "-fpe3 -fp-model consistent -assume noold_maxminloc")

# Common variables for every compiler
include(GenericCompiler)
