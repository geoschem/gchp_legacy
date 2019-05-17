

find_path(ESMF_INCLUDES_DIR
	ESMF_ErrReturnCodes.inc
	DOC "The path to the directory containing \"ESMF_ErrReturnCodes.inc\"."
	PATH_SUFFIXES
		"include" 
		"mod"
)

find_path(ESMF_HEADERS_DIR
	ESMC.h
	DOC "The path to the directory containing \"ESMC.h\"."
	PATH_SUFFIXES "include"
)

file_glob_directories(GLOBBED_MODDIRS
	PATTERNS
		mod/mod*/*.*.*.*.*/esmf.mod
	PATHS
		${CMAKE_PREFIX_PATH}
)
find_path(ESMF_MOD_DIR
	esmf.mod
	HINTS
		${GLOBBED_MODDIRS}
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
)
find_library(ESMF_LIBRARIES
	libesmf.a
	HINTS
		${GLOBBED_MODDIRS}
	DOC "The path to the directory containing \"libesmf.a\"."
	PATH_SUFFIXES
		"lib"
)

set(ESMF_ERRMSG "\nCouldn't find the following directories/files:")
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
if(NOT ESMF_LIBRARIES)
	set(ESMF_ERRMSG "${ESMF_ERRMSG}
    ESMF_LIBRARIES:    Path to \"libesmf.a\"")
endif()
set(ESMF_ERRMSG "${ESMF_ERRMSG}\n\nLocate the directories/files mentioned above and then set CMAKE_PREFIX_PATH to a semicolon-separated list of directories which will be searched.\n\n")


find_package_handle_standard_args(ESMF 
	REQUIRED_VARS 
		ESMF_INCLUDES_DIR 
		ESMF_HEADERS_DIR 
		ESMF_MOD_DIR 
		ESMF_LIBRARIES
	FAIL_MESSAGE "${ESMF_ERRMSG}"
)