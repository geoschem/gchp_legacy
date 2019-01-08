# Add GCHP's CMake module directory
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMakeScripts)

# Configure GCHP build
include(GC-ConfigureGCHPBuild)

# Add GIGC library
add_library(GIGC STATIC
	Chem_GridCompMod.F90
	GEOS_ctmEnvGridComp.F90
	gigc_chunk_mod.F90
	GIGC_GridCompMod.F90
	gigc_historyexports_mod.F90
	gigc_mpi_wrap.F90
	gigc_providerservices_mod.F90
)    
target_link_libraries(GIGC
	PUBLIC GeosCore
)

add_executable(geos
    GEOSChem.F90
)

target_link_libraries(geos
	PUBLIC GIGC ${GC_EXTRA_TARGETS}
)

set_target_properties(geos
    PROPERTIES 
	INSTALL_RPATH "${THIRD_PARTY}/lib"
)

install(TARGETS geos
    RUNTIME DESTINATION ${RUNDIR}
)
