
find_path(GFTL_INCLUDE
	types/key_deferredLengthString.inc
	PATHS "${GFTL_INSTALL}"
	PATH_SUFFIXES "include"
)

find_package_handle_standard_args(GFTL
	REQUIRED_VARS GFTL_INSTALL GFTL_INCLUDE
	FAIL_MESSAGE "Cannot find the gFTL include directory!\nSet GFTL_INSTALL\n"
)

mark_as_advanced(GFTL_INSTALL)
set(GFTL_INCLUDE_DIRS ${GFTL_INCLUDE})
