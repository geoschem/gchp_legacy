
find_path(GFTL_INCLUDE_DIR
	types/key_deferredLengthString.inc
	PATH_SUFFIXES "include"
)

set(GFTL_ERRMSG "\nCouldn't find the following directories/files:")
if(NOT GFTL_INCLUDE_DIR)
	set(GFTL_ERRMSG "${GFTL_ERRMSG}
    GFTL_INCLUDE_DIR: Path to gFTL's \"include/\" directory")
endif()
set(GFTL_ERRMSG "${GFTL_ERRMSG}\n\nLocate the directories/files mentioned above and then set CMAKE_PREFIX_PATH to a semicolon-separated list of directories which will be searched.\n\n")

find_package_handle_standard_args(GFTL
	REQUIRED_VARS GFTL_INCLUDE_DIR
	FAIL_MESSAGE "${GFTL_ERRMSG}"
)

mark_as_advanced(GFTL_INSTALL)
