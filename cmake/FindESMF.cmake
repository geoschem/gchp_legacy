

find_path(ESMF_INCLUDES_DIR
	ESMF_ErrReturnCodes.inc
	HINTS 
		$ENV{ESMF_ROOT}
		$ENV{ESMF_ROOT}/DEFAULTINSTALLDIR
	DOC "The path to the directory containing \"ESMF_ErrReturnCodes.inc\"."
	PATH_SUFFIXES
		"include" 
		"mod"
)

find_path(ESMF_HEADERS_DIR
	ESMC.h
	HINTS 
		$ENV{ESMF_ROOT}
		$ENV{ESMF_ROOT}/DEFAULTINSTALLDIR
	DOC "The path to the directory containing \"ESMC.h\"."
	PATH_SUFFIXES "include"
)

file_glob_directories(GLOBBED_MODDIRS
	PATTERNS
		mod/mod*/*.*.*.*.*/esmf.mod
	PATHS
		${CMAKE_PREFIX_PATH}
		$ENV{ESMF_ROOT}
		$ENV{ESMF_ROOT}/DEFAULTINSTALLDIR
)
find_path(ESMF_MOD_DIR
	esmf.mod
	HINTS
		${GLOBBED_MODDIRS}
		$ENV{ESMF_ROOT}
		$ENV{ESMF_ROOT}/DEFAULTINSTALLDIR
	DOC "The path to the directory containing \"esmf.mod\"."
	PATH_SUFFIXES 
		"mod"
		"include"
)

file_glob_directories(GLOBBED_MODDIRS
	PATTERNS
		lib/lib*/*.*.*.*.*/libesmf.a
	PATHS
		${CMAKE_PREFIX_PATH}
		$ENV{ESMF_ROOT}
		$ENV{ESMF_ROOT}/DEFAULTINSTALLDIR
)
find_library(ESMF_LIBRARY
	libesmf.a
	HINTS
		${GLOBBED_MODDIRS}
		$ENV{ESMF_ROOT}
		$ENV{ESMF_ROOT}/DEFAULTINSTALLDIR
	DOC "The path to the directory containing \"libesmf.a\"."
	PATH_SUFFIXES
		"lib"
)

set(ESMF_ERRMSG "\nCounldn't find one or more of ESMF's files! The following files/directories weren't found:")
if(NOT ESMF_INCLUDES_DIR)
	set(ESMF_ERRMSG "${ESMF_ERRMSG}
    ESMF_INCLUDES_DIR: Directory with ESMF's \".inc\" files (e.g. \"ESMF_ErrReturnCodes.inc\")")
endif()
if(NOT ESMF_HEADERS_DIR)
	set(ESMF_ERRMSG "${ESMF_ERRMSG}
    ESMF_HEADERS_DIR:  Directory with ESMF's \".h\" files   (e.g. \"ESMC.h\")")
endif()
if(NOT ESMF_MOD_DIR)
	set(ESMF_ERRMSG "${ESMF_ERRMSG}
    ESMF_MOD_DIR:      Directory with ESMF's \".mod\" files (e.g. \"esmf.mod\")")
endif()
if(NOT ESMF_LIBRARY)
	set(ESMF_ERRMSG "${ESMF_ERRMSG}
    ESMF_LIBRARY:    Path to \"libesmf.a\"")
endif()
set(ESMF_ERRMSG "${ESMF_ERRMSG}\nFind the directories/files that are listed above. Specify the directories you want CMake to search with the CMAKE_PREFIX_PATH variable (or the ESMF_ROOT environment variable).\n")


find_package_handle_standard_args(ESMF 
	REQUIRED_VARS 
		ESMF_INCLUDES_DIR 
		ESMF_HEADERS_DIR 
		ESMF_MOD_DIR 
		ESMF_LIBRARY
	FAIL_MESSAGE "${ESMF_ERRMSG}"
)

# Specify the other libraries that need to be linked for ESMF
find_package(NetCDF REQUIRED)
find_package(MPI REQUIRED)
execute_process (COMMAND ${CMAKE_CXX_COMPILER} --print-file-name=libstdc++.so OUTPUT_VARIABLE stdcxx OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process (COMMAND ${CMAKE_CXX_COMPILER} --print-file-name=libgcc.a OUTPUT_VARIABLE libgcc OUTPUT_STRIP_TRAILING_WHITESPACE)

set(ESMF_LIBRARIES ${ESMF_LIBRARY} ${NETCDF_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${MPI_CXX_LIBRARIES} rt ${stdcxx} ${libgcc})
