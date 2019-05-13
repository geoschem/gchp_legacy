#[[ GC-ConfigureGCHPBuild.cmake

This file configures BaseTarget for a GCHP build. This file does
three things:

    1) Finds dependencies needed by GCHP. Note that CMake scripts in 
       Shared/ also find_package's.

    2) Sets the appropriate preprocessor definitions for the run directory.

    3) Sets the default compiler flags.

]]

#[[--------------------------------------------------------------------------]]
#[[     Finding dependencies.                                                ]]
#[[--------------------------------------------------------------------------]]
find_package(OpenMP REQUIRED)
find_package(MPI REQUIRED)
find_package(NetCDF REQUIRED COMPONENTS F90)
find_package(GCHPThirdParty REQUIRED)
message(STATUS "Importing \"${EXPORTED_GCHP_THIRDPARTY}\"")

# Setup BaseTarget
string(STRIP "${MPI_Fortran_LINK_FLAGS}" MPI_Fortran_LINK_FLAGS)
target_compile_options(BaseTarget
    INTERFACE 
        ${MPI_Fortran_COMPILE_FLAGS}
)
target_include_directories(BaseTarget
    INTERFACE 
        ${NETCDF_F90_INCLUDE_DIR} 
        ${MPI_Fortran_INCLUDE_PATH}
)
get_target_property(ESMF_LIB ESMF INTERFACE_LINK_LIBRARIES)
target_link_libraries(BaseTarget 
	INTERFACE 
		MAPL_Base MAPL_cfio_r4 GMAO_mpeu GMAO_pilgrim
		FVdycoreCubed_GridComp fvdycore GFDL_fms_r4 GEOS_Shared GMAO_hermes 
		${NETCDF_LIBRARIES} 
        ${MPI_Fortran_LINK_FLAGS} 
        ${MPI_Fortran_LIBRARIES} 
        ${OpenMP_Fortran_FLAGS} 
        ${ESMF_LIB} rt
        #/usr/lib/x86_64-linux-gnu/libstdc++.so.6  TODO: not sure why this was needed before
)

# Print repository's last commit
get_cwd_last_commit_hash(GC_LAST_COMMIT   ${CMAKE_SOURCE_DIR})
get_cwd_last_commit_hash(GCHP_LAST_COMMIT ${CMAKE_SOURCE_DIR}/GCHP)
message(STATUS "GEOS-Chem repository: ${GC_LAST_COMMIT}")
message(STATUS "GCHP repository:      ${GCHP_LAST_COMMIT}")


#[[--------------------------------------------------------------------------]]
#[[     Setting preprocessor definitions.                                    ]]
#[[--------------------------------------------------------------------------]]


#[[     Get defaults for settings by inspecting the run directory.           ]]

# Inspect rundir name to guess the chemistry mechanism
if("${RUNDIR_NAME}" MATCHES ".*benchmark")
    set(RUNDIR_MECH "benchmark")
elseif("${RUNDIR_NAME}" MATCHES ".*RnPbBe")
    set(RUNDIR_MECH "RnPbBe")
else()
    set(RUNDIR_MECH "Standard")
endif()

#[[     Settings TUI with defaults from the run directory inspection.        ]]

# Chemistry mechanism
set_dynamic_option(MECH 
    DEFAULT "${RUNDIR_MECH}"
    LOG GENERAL_OPTIONS_LOG
    SELECT_EXACTLY 1
    OPTIONS "Standard" "RnPbBe" "benchmark"
)
set(MECH ${MECH} PARENT_SCOPE) # Make visible for use by ../KPP 

# Build RRTMG?
set_dynamic_option(RRTMG 
    DEFAULT "FALSE"
    LOG GENERAL_OPTIONS_LOG
    SELECT_EXACTLY 1
    OPTIONS "TRUE" "FALSE"
)
set(RRTMG ${RRTMG} PARENT_SCOPE)
if(${RRTMG})
    set_dynamic_default(GC_DEFINES DEFAULT "RRTMG")
endif()

message(STATUS "General settings:")
dump_log(GENERAL_OPTIONS_LOG)

# Get diagnostics
set_dynamic_default(DIAG 
#    "BPCH_DIAG" "BPCH_TIMESER" "BPCH_TPBC"
    DEFAULT "NC_DIAG" "NC_HAS_COMPRESSION"
    LOG EXTRA_DEFS_LOG
)
set_dynamic_default(GC_DEFINES DEFAULT ${DIAG})

# Get extra defines
set_dynamic_default(EXTRA 
    DEFAULT "ESMF_" "EXTERNAL_GRID" "GEOS_FP" "USE_REAL8" 
    
    LOG EXTRA_DEFS_LOG
)
set_dynamic_default(GC_DEFINES DEFAULT ${EXTRA})

message(STATUS "Additional definitions:")
dump_log(EXTRA_DEFS_LOG)

# Get resulting GC_DEFINES
string(REPLACE " " ";" GC_DEFINES "${GC_DEFINES}")
set_dynamic_default(GC_DEFINES LOG RESULTING_DEFINES_LOG)

#[[     Set resulting defintions on BaseTarget.                              ]]
target_compile_definitions(BaseTarget 
    INTERFACE 
        ${GC_DEFINES}
)
unset(GC_DEFINES)


#[[--------------------------------------------------------------------------]]
#[[     Setting default compiler options.                                    ]]
#[[--------------------------------------------------------------------------]]

# Get compiler options
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    set_dynamic_default(FC_OPTIONS
        DEFAULT 
            -fPIC 
            -cpp 
            -w 
            -auto 
            -noalign 
            "-convert big_endian" 
            -vec-report0 
            "-fp-model source" 
            -openmp 
            -mcmodel=medium 
            -shared-intel 
            -traceback
            -DLINUX_IFORT

        LOG RESULTING_DEFINES_LOG
    )
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
    set_dynamic_default(FC_OPTIONS
        DEFAULT 
            -cpp 
            -w 
            -std=legacy 
            -fautomatic 
            -fno-align-commons 
            -fconvert=native 
            -fno-range-check 
            -O3 
            -funroll-loops 
            -fopenmp 
            -mcmodel=medium 
            -fbacktrace -g 
            -DLINUX_GFORTRAN

        LOG RESULTING_DEFINES_LOG
    )
else()
    message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} Fortran compiler is not currently supported!")
endif()

message(STATUS "Resulting definitions/options:")
dump_log(RESULTING_DEFINES_LOG)

target_compile_options(BaseTarget 
    INTERFACE 
        ${FC_OPTIONS}

        # Debug flags
        $<$<AND:$<CXX_COMPILER_ID:Intel>,$<CONFIG:DEBUG>>:-g -O0>
        $<$<AND:$<CXX_COMPILER_ID:GNU>,  $<CONFIG:DEBUG>>:-g -Og>

        # Release flags
        $<$<AND:$<CXX_COMPILER_ID:Intel>,$<CONFIG:DEBUG>>:-O2>
        $<$<AND:$<CXX_COMPILER_ID:GNU>,  $<CONFIG:DEBUG>>:-O3>
)
unset(FC_OPTIONS)