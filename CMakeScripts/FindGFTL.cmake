
find_path(GFTL_INCLUDE_DIR
	types/key_deferredLengthString.inc
	HINTS 
		${GFTL_INSTALL}
		${CMAKE_BINARY_DIR}/${GFTL_INSTALL}
	PATH_SUFFIXES "include"
)

# Make a readable error message
set(GFTL_ERRMSG "\nFailed to find some file in your gFTL install that are needed. \
To resolve this you have two options:\n
  1) Set GFTL_INSTALL to the directory where gFTL was installed.")

if(IS_ABSOLUTE "${GFTL_INSTALL}" AND EXISTS "${GFTL_INSTALL}")
    set(GFTL_ERRMSG "${GFTL_ERRMSG}
	+ GFTL_INSTALL: ${GFTL_INSTALL}")
elseif(EXISTS "${CMAKE_BINARY_DIR}/${GFTL_INSTALL}")
    set(GFTL_ERRMSG "${GFTL_ERRMSG}
    + GFTL_INSTALL: ${CMAKE_BINARY_DIR}/${GFTL_INSTALL}")
else()
    set(GFTL_ERRMSG "${GFTL_ERRMSG}
    + GFTL_INSTALL: Path to where GFTL was installed")
endif()

set(GFTL_ERRMSG "${GFTL_ERRMSG}\n
  2) Manually set the following paths which are missing:")
if(NOT GFTL_INCLUDE_DIR)
	set(GFTL_ERRMSG "${GFTL_ERRMSG}
    + GFTL_INCLUDE_DIR: Path to gFTL's \"include/\" directory")
endif()
set(GFTL_ERRMSG "${GFTL_ERRMSG}\n\n")

find_package_handle_standard_args(GFTL
	REQUIRED_VARS GFTL_INCLUDE_DIR
	FAIL_MESSAGE "${GFTL_ERRMSG}"
)

mark_as_advanced(GFTL_INSTALL)
