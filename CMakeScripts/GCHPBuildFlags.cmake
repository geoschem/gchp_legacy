
# Inspect rundir name to guess the chemistry mechanism
if("${RUNDIR_NAME}" MATCHES ".*benchmark")
    set(RUNDIR_MECH "Benchmark")
elseif("${RUNDIR_NAME}" MATCHES ".*RnPbBe")
    set(RUNDIR_MECH "RnPbBe")
else()
    set(RUNDIR_MECH "Standard")
endif()

# Get thirdparty install directory 
message(STATUS "Third party libraries install:")

set(THIRD_PARTY_INSTALL_PROMPT "<path to third party installs>")
set_dynamic_default(THIRD_PARTY "${THIRD_PARTY_INSTALL_PROMPT}"
    LOG THIRD_PARTY_LOG 
)
dump_log(THIRD_PARTY_LOG)

# Chemistry mechanism
set_dynamic_option(MECH "${RUNDIR_MECH}"
    LOG GENERAL_OPTIONS_LOG
    SELECT_EXACTLY 1
    OPTIONS "Standard" "RnPbBe" "Benchmark"
)

# Build RRTMG?
set_dynamic_option(RRTMG "FALSE"
    LOG GENERAL_OPTIONS_LOG
    SELECT_EXACTLY 1
    OPTIONS "TRUE" "FALSE"
)
if(${RRTMG})
    set_dynamic_default(GC_DEFINES "RRTMG")
endif()

message(STATUS "General settings:")
dump_log(GENERAL_OPTIONS_LOG)

# Get diagnostics
set_dynamic_default(DIAG 
#    "BPCH_DIAG" "BPCH_TIMESER" "BPCH_TPBC"
    "NC_DIAG" "NC_HAS_COMPRESSION"
    LOG EXTRA_DEFS_LOG
)
set_dynamic_default(GC_DEFINES ${DIAG})

# Get extra defines
set_dynamic_default(EXTRA 
    "ESMF_" "EXTERNAL_GRID" "GEOS_FP" "USE_REAL8" 
    
    LOG EXTRA_DEFS_LOG
)
set_dynamic_default(GC_DEFINES ${EXTRA})

message(STATUS "Additional definitions:")
dump_log(EXTRA_DEFS_LOG)

# Get resulting GC_DEFINES
string(REPLACE " " ";" GC_DEFINES "${GC_DEFINES}")
set_dynamic_default(GC_DEFINES LOG RESULTING_DEFINES_LOG)

# Get compiler options
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    set_dynamic_default(FC_OPTIONS
        -fPIC -cpp -w -auto -noalign "-convert big_endian" -O2 -vec-report0 
        "-fp-model source" -openmp -mcmodel=medium -shared-intel -traceback
        -DLINUX_IFORT

        LOG RESULTING_DEFINES_LOG
    )
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
    set_dynamic_default(FC_OPTIONS
        -cpp -w -std=legacy -fautomatic -fno-align-commons -fconvert=native -fno-range-check -O3 
        -funroll-loops -fopenmp -mcmodel=medium -fbacktrace -g 
        
        -DLINUX_GFORTRAN

        LOG RESULTING_DEFINES_LOG
    )
else()
    message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} Fortran compiler is not currently supported!")
endif()

message(STATUS "Resulting definitions/options:")
dump_log(RESULTING_DEFINES_LOG)

# Replace ';' character (delimiting lists) with ' '
string(REPLACE ";" " " FC_OPTIONS "${FC_OPTIONS}")

