
find_path(ESMF_INCLUDES 
	ESMC.h
	PATHS "${ESMF_INSTALL}"
	PATH_SUFFIXES "include"
)
find_path(ESMF_MODULES
	esmf.mod
	PATHS "${ESMF_INSTALL}"
	PATH_SUFFIXES 
		"mod" 
		"mod/modO/Linux.intel.64.openmpi.default"
)

find_library(ESMF_LIBRARY_DIR
	esmf
	PATHS "${ESMF_INSTALL}"
	PATH_SUFFIXES
		"lib"
		"lib/libO/Linux.intel.64.openmpi.default"
)

find_package_handle_standard_args(ESMF 
	REQUIRED_VARS ESMF_INSTALL ESMF_INCLUDES ESMF_MODULES ESMF_LIBRARY_DIR
	FAIL_MESSAGE "Cannot find the ESMF includes and libraries!\nSet ESMF_INSTALL\n"
)

mark_as_advanced(ESMF_INSTALL)
set(ESMF_INCLUDE_DIRS ${ESMF_INCLUDES} ${ESMF_MODULES})
set(ESMF_LIBRARIES ${ESMF_LIBRARY_DIR})
