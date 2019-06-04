cmake_minimum_required (VERSION 3.5)
project(GCHP VERSION 12.4.0 LANGUAGES Fortran CXX C)
cmake_policy (SET CMP0053 NEW)
cmake_policy (SET CMP0054 NEW)

# Add module paths
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../CMakeScripts")
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/check_compiler_support")

# Find dependencies
find_package(OpenMP REQUIRED)
find_package(MPI REQUIRED)
find_package(NetCDF REQUIRED)
find_package(ESMF REQUIRED)
find_package(GFTL REQUIRED)

# Cleanup MPI link commands
string(STRIP ${MPI_Fortran_LINK_FLAGS} MPI_Fortran_LINK_FLAGS)

# Add ESMA but exclude from all to prevent all their targets from being added to our all target
add_subdirectory(Shared EXCLUDE_FROM_ALL)
add_subdirectory(FVdycoreCubed_GridComp EXCLUDE_FROM_ALL)

# Configure GEOS-Chem
set(GC_EXTERNAL_CONFIG      TRUE        PARENT_SCOPE)
set(RRTMG                   FALSE       PARENT_SCOPE)
set(GTMM                    FALSE       PARENT_SCOPE)
set(TOMAS                   FALSE       PARENT_SCOPE)
if("${GCHP}" MATCHES "standard")
    set(MECH                "Standard"  PARENT_SCOPE)
elseif("${GCHP}" MATCHES "tropchem")
    set(MECH                "Tropchem"  PARENT_SCOPE)
else()
    message(FATAL_ERROR "The chemistry mechanism couldn't be determined from your input.geos! The first line didn't match standard or tropchem.")
endif()
set(GCHP                    TRUE       PARENT_SCOPE)

target_compile_definitions(BaseTarget
    INTERFACE ESMF_ EXTERNAL_GRID USE_REAL8
)
target_link_libraries(BaseTarget INTERFACE 
    MAPL_Base MAPL_cfio_r4 GMAO_mpeu GMAO_pilgrim
    FVdycoreCubed_GridComp fvdycore GFDL_fms_r4 GEOS_Shared GMAO_hermes 
    NetCDF-F 
    ${MPI_Fortran_LINK_FLAGS} 
    ${MPI_Fortran_LIBRARIES} 
    ${OpenMP_Fortran_FLAGS} 
    ${ESMF_LIBRARIES} 
    rt
)

# Add the registry
set(MAPL_ACG ${CMAKE_CURRENT_SOURCE_DIR}/Shared/MAPL_Base/mapl_acg.pl)
add_subdirectory(Registry)

# Add GCHP targets
add_library(GIGC STATIC
	Chem_GridCompMod.F90
	GEOS_ctmEnvGridComp.F90
	gigc_chunk_mod.F90
	GIGC_GridCompMod.F90
	gigc_historyexports_mod.F90
	gigc_providerservices_mod.F90
)    
target_link_libraries(GIGC
	PUBLIC GeosCore
)
add_dependencies(GIGC registry)

add_executable(geos
    GEOSChem.F90
)
target_link_libraries(geos
	PUBLIC GIGC ${GC_EXTRA_TARGETS} MAPL_Base
)

set_target_properties(geos
    PROPERTIES 
	INSTALL_RPATH "${THIRD_PARTY}/lib"
)

install(TARGETS geos
    RUNTIME DESTINATION ${CMAKE_BINARY_DIR}
)

if("${CMAKE_BUILD_TYPE}" STREQUAL "Release")
    # Add warning suppressions to ESMA target's whose warning's we ignore anyways
    get_warning_suppression_flags(SUPPRESSED_FLAGS
        LANGUAGE Fortran
        INTEL
            8291 		# Recommended relationship between field width 'W' and the number of fractional digits 'D' in this edit descriptor is 'W>=D+7'.
            7920 7919 	# The value was too small when converting to REAL(KIND=4); the result is in the denormalized range.  (i.e. a value like 1e-40)
            5462 	 	# Global name too long, shortened from: xxxxxxxxxx to: xxxxx
            6536 		# All symbols from this module are already visible due to another USE; the ONLY clause will have no effect. Rename clauses, if any, will be honored.
            6843 		# A dummy argument with an explicit INTENT(OUT) declaration is not given an explicit value.
            6371 		# A jump into a block from outside the block may have occurred.
    )
    target_compile_options(GFDL_fms_r4 				PRIVATE ${SUPPRESSED_FLAGS})
    target_compile_options(GFDL_fms_r8 				PRIVATE ${SUPPRESSED_FLAGS})
    target_compile_options(GMAO_mpeu 				PRIVATE ${SUPPRESSED_FLAGS})
    target_compile_options(MAPL_Base 				PRIVATE ${SUPPRESSED_FLAGS})
    target_compile_options(GEOS_Shared 				PRIVATE ${SUPPRESSED_FLAGS})
    target_compile_options(fvdycore 				PRIVATE ${SUPPRESSED_FLAGS})
    target_compile_options(FVdycoreCubed_GridComp   PRIVATE ${SUPPRESSED_FLAGS})
    target_compile_options(GMAO_pilgrim 			PRIVATE ${SUPPRESSED_FLAGS})
endif()
