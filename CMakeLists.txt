add_static_target(GIGC
    SOURCES
        Chem_GridCompMod.F90
        GEOS_ctmEnvGridComp.F90
        gigc_chunk_mod.F90
        GIGC_GridCompMod.F90
        gigc_historyexports_mod.F90
        gigc_mpi_wrap.F90
        gigc_providerservices_mod.F90
        
    PUBLIC_LINK
        GeosCore
)

link_directories(${THIRD_PARTY}/lib)

add_executable(geos
    GEOSChem.F90
)

target_link_libraries(geos
    PUBLIC 
        GIGC 
	esmf 
        MAPL_Base MAPL_cfio GMAO_mpeu GMAO_pilgrim
        FVdycoreCubed_GridComp fvdycore GFDL_fms GEOS_Shared GMAO_hermes 
	${MPI_Fortran_LIBRARIES} ${NETCDF_LIBRARIES}
)

set_target_properties(geos
    PROPERTIES 
	INSTALL_RPATH "${THIRD_PARTY}/lib"
)

install(TARGETS geos
    RUNTIME DESTINATION ${RUNDIR}
)


