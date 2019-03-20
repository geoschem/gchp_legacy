
find_path(ESMF_INCLUDES 
	ESMC.h
	PATH_SUFFIXES include
	NO_DEFAULT_PATH
)
find_path(ESMF_MODULES
	esmf.mod
	PATH_SUFFIXES 
		mod 
		mod0/Linux.intel.64.openmpi.default
	NO_DEFAULT_PATH
)
find_library(ESMF_LIBRARY
	esmf
	PATH_SUFFIXES
		lib
		lib/libO/Linux.intel.64.openmpi.default
	NO_DEFAULT_PATH
)

find_package_handle_standard_args(ESMF 
	"Failed to find ESMF include directories and libraries!"
	ESMF_INCLUDES ESMF_MODULES ESMF_LIBRARY
)

mark_as_advanced(ESMF_INCLUDES ESMF_MODUL ESMF_LIBRARY)
set(ESMF_INCLUDE_DIRS ${ESMF_INCLUDES} ${ESMF_MODULES})
set(ESMF_LIBRARIES ${ESMF_LIBRARY})
