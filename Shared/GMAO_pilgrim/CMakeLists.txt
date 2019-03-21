esma_set_this()

set(srcs
  shr_kind_mod.F90
  debugutilitiesmodule.F90
  timingModule.F90
  decompmodule.F90
  ghostmodule.F90
  mod_comm.F90
  parutilitiesmodule.F90
  PILGRIM_xyMod.F90
  redistributemodule.F90
  )

set (CMAKE_Fortran_FLAGS_RELEASE "-O2 ${common_Fortran_fpe_flags} ${ALIGNCOM}")

esma_add_library(${this} SRCS ${srcs})
target_compile_definitions (${this} PRIVATE SPMD STAND_ALONE MODCM_ALLOC NO_TYPE_INIT)
target_include_directories(${this} PRIVATE ${MPI_Fortran_INCLUDE_PATH})
target_link_libraries (${this} PUBLIC ${MPI_Fortran_LIBRARIES})


