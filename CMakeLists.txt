# Add GCHP's CMake module directory
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMakeScripts)

# Add MAPL and FVdycore
add_subdirectory(Shared)
add_subdirectory(FVdycoreCubed_GridComp)

# Add GIGC library
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
    RUNTIME DESTINATION ${RUNDIR}
)

#[[ 	Configure GCHP build 												 ]]
include(GC-ConfigureGCHPBuild)

#[[ 	Warning suppressions on third-party components.						 ]]
message(STATUS "Suppressed compiler warnings for third-party components:")
set_dynamic_option(SUPPRESSED DEFAULT
    LOG SUPPRESSED_LOG
    SELECT_EXACTLY 1
    OPTIONS "DEFAULT" "NONE"
)
dump_log(SUPPRESSED_LOG)
if("${SUPPRESSED}" STREQUAL "DEFAULT")
	# Add suppression flags on MAPL and FVdycore
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
	target_compile_options(FVdycoreCubed_GridComp 	PRIVATE ${SUPPRESSED_FLAGS})
	target_compile_options(GMAO_pilgrim 			PRIVATE ${SUPPRESSED_FLAGS})

	target_compile_definitions(GFDL_fms_r4 			PRIVATE LRB_CLEANER_BUILD_LOG)
	target_compile_definitions(GFDL_fms_r8 			PRIVATE LRB_CLEANER_BUILD_LOG)
	target_compile_definitions(GMAO_mpeu 			PRIVATE LRB_CLEANER_BUILD_LOG)
	target_compile_definitions(GEOS_Shared 			PRIVATE LRB_CLEANER_BUILD_LOG)
	target_compile_definitions(fvdycore 			PRIVATE LRB_CLEANER_BUILD_LOG)
endif()