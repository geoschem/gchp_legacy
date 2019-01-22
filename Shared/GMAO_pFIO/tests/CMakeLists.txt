add_definitions(-DUSE_MPI)
set (TEST_SRCS
    Test_Attribute.F90
    Test_Variable.F90
    test_RequestDataMessage.F90
    Test_FileMetadata.F90
    Test_NetCDF4_FileFormatter.F90
    Test_pFIO_Utilities.F90
    test_SimpleSocket.F90
    test_MpiSocket.F90
    test_ServerThread.F90
    test_Client.F90
    test_ProtocolParser.F90
    test_DirectoryService.F90
  )

# SRCS are mostly mocks to facilitate tests
set (SRCS
  pFIO_Initialize.F90
  MockServerThread.F90
  MockClient.F90
#  MockServer.F90
  MockSocket.F90
  )

foreach(file ${TEST_SRCS})
   get_filename_component(basename ${file} NAME_WE)
   add_custom_command (
      OUTPUT ${file}
      COMMAND python
      ARGS ${PFUNIT_PATH}/bin/pFUnitParser.py ${CMAKE_CURRENT_SOURCE_DIR}/${basename}.pf ${file}
      DEPENDS ${basename}.pf
   )
endforeach()

add_library(pFIO_tests EXCLUDE_FROM_ALL ${TEST_SRCS} ${SRCS})
target_link_libraries(pFIO_tests GMAO_pFIO  MAPL_pFUnit ${PFUNIT_LIBRARIES})

include_directories(
   ${PFUNIT_INCLUDE_DIRS}
   ${CMAKE_CURRENT_SOURCE_DIR}
   ${CMAKE_BINARY_DIR}/src
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/..)
include_directories(${include_GMAO_pFIO})
include_directories(${include_MAPL_pFUnit})

set(EXE pfio_tests.x)
add_executable (${EXE} EXCLUDE_FROM_ALL ${PFUNIT_PATH}/include/driver.F90 testSuites.inc)
target_compile_definitions (${EXE} PRIVATE USE_MPI PFUNIT_EXTRA_INITIALIZE=pFIO_Initialize)
set_target_properties(${EXE} PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_link_libraries(${EXE} pFIO_tests esmf_fullylinked netcdff netcdf he5_hdfeos hdf5hl_fortran hdf5_hl mfhdf df hdf5 jpeg sz z MAPL_pFUnit ${MPI_CXX_LIBRARIES} ${STDCxx} ${libgcc})


set(MPI_NPES 8)

add_custom_target(tests
  COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPI_NPES} ${MPIEXEC_PREFLAGS} ./${EXE} ${MPIEXEC_POSTFLAGS}
  DEPENDS ${EXE} 
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Runnning tests ./${EXE} ..."
  )

add_test(pFIO_tests
  COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPI_NPES} ${MPIEXEC_PREFLAGS} ./${EXE} ${MPIEXEC_POSTFLAGS}
  )
