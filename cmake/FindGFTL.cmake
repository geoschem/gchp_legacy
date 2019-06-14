
find_path(GFTL_INCLUDE_DIR
	types/key_deferredLengthString.inc
	HINTS
		$ENV{GFTL_ROOT}
		$ENV{gFTL_ROOT}
	PATH_SUFFIXES "include"
)

set(GFTL_ERRMSG "\nCounldn't find one or more of GFTL's files! The following files/directories weren't found:")
if(NOT GFTL_INCLUDE_DIR)
	set(GFTL_ERRMSG "${GFTL_ERRMSG}
    GFTL_INCLUDE_DIR: Path to gFTL's \"include/\" directory")
endif()
set(GFTL_ERRMSG "${GFTL_ERRMSG}\nFind the directories/files that are listed above. Specify the directories you want CMake to search with the CMAKE_PREFIX_PATH variable (or the GFTL_ROOT environment variable).\n")

find_package_handle_standard_args(GFTL
	REQUIRED_VARS GFTL_INCLUDE_DIR
	FAIL_MESSAGE "${GFTL_ERRMSG}"
)

mark_as_advanced(GFTL_INSTALL)
