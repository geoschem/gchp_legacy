set (CMAKE_Fortran_FLAGS_RELEASE "-O3 ${common_Fortran_fpe_flags} ${ALIGNCOM}")

add_executable(argopt argopt.c)
add_executable(fresolv.x fresolv.f90)
add_executable(gethdftinc.x gethdftinc.f)
add_executable(zeit_ci.x zeit_ci.f zeit_usage.f)
add_executable(zeit_co.x zeit_co.f zeit_usage.f)
add_executable(zeit_pr.x zeit_pr.f zeit_usage.f zeit_read.f zeit_jday.f)

target_link_libraries(fresolv.x GMAO_mpeu)
target_link_libraries(gethdftinc.x GMAO_mpeu)
target_link_libraries(gethdftinc.x ${NETCDF_LIBRARIES})
target_link_libraries(gethdftinc.x Threads::Threads)
target_link_libraries(zeit_ci.x GMAO_mpeu)
target_link_libraries(zeit_co.x GMAO_mpeu)
target_link_libraries(zeit_pr.x GMAO_mpeu)

include_directories(${include_GMAO_mpeu})
include_directories(${INC_NETCDF})

set(execs
    Manipulate_time.pm
    tick
    stripname
    RmShmKeys_sshmpi.csh
    rmshmkeyhere.sh
    esma_mpirun
    GMAO_utils.pm
   )

foreach(exec ${execs})
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${exec} ${CMAKE_BINARY_DIR}/bin/${exec} COPYONLY)
endforeach()
