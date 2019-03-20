include (try_fortran_compile)

try_fortran_compile(
  ${CMAKE_CURRENT_LIST_DIR}/assumed_type.F90
  FORTRAN_COMPILER_SUPPORTS_ASSUMED_TYPE
  )
