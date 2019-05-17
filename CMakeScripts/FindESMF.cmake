
#[[
find_path -- used to find the path to a directory containing the given file

find_path(VAR filename)

Creates a cache entry for VAR. If VAR is set, find_path will not run. If 
the file isn't found the the value will be VAR-NOT_FOUND.

HINTS is for dynamically generated guesses
PATHS is for hard-coded guesses
]]


find_path(ESMF_INCLUDES_DIR
	ESMF_ErrReturnCodes.inc 
	HINTS 
		${ESMF_INSTALL} 
		${CMAKE_BINARY_DIR}/${ESMF_INSTALL}
		$ENV{ESMF_INSTALL_PREFIX} 
		$ENV{ESMF_INSTALL_HEADERDIR}
		$ENV{ESMF_DIR}/DEFAULTINSTALLDIR
		$ENV{ESMF_DIR}/src
	DOC "The path to the directory containing \"ESMF_ErrReturnCodes.inc\"."
	PATH_SUFFIXES
		"include" 
		"mod"
)
find_path(ESMF_HEADERS_DIR
	ESMC.h
	HINTS 
		${ESMF_INSTALL} 
		${CMAKE_BINARY_DIR}/${ESMF_INSTALL}
		$ENV{ESMF_INSTALL_PREFIX} 
		$ENV{ESMF_INSTALL_HEADERDIR}
		$ENV{ESMF_DIR}/DEFAULTINSTALLDIR
	DOC "The path to the directory containing \"ESMC.h\"."
	PATH_SUFFIXES "include"
)


file(GLOB GLOB_MATCHES
	${ESMF_INSTALL}/mod/mod*/*.*.*.*.*/esmf.mod
	${CMAKE_BINARY_DIR}/${ESMF_INSTALL}/mod/mod*/*.*.*.*.*/esmf.mod
	$ENV{ESMF_INSTALL_PREFIX}/mod/mod*/*.*.*.*.*/esmf.mod
	$ENV{ESMF_DIR}/DEFAULTINSTALLDIR/mod/mod*/*.*.*.*.*/esmf.mod
)
set(ESMF_MODDIRS_GLOBBED "")
foreach(GLOBBED_FILE ${GLOB_MATCHES})
	get_filename_component(GLOBBED_DIR ${GLOBBED_FILE} DIRECTORY)
	list(APPEND ESMF_MODDIRS_GLOBBED ${GLOBBED_DIR})
endforeach()
list(REMOVE_DUPLICATES  ESMF_MODDIRS_GLOBBED)
find_path(ESMF_MOD_DIR
	esmf.mod
	HINTS
		${ESMF_MODDIRS_GLOBBED}
		${ESMF_INSTALL}
		${CMAKE_BINARY_DIR}/${ESMF_INSTALL}
		$ENV{ESMF_INSTALL_PREFIX}
		$ENV{ESMF_INSTALL_MODDIR}
	DOC "The path to the directory containing \"esmf.mod\"."
	PATH_SUFFIXES 
		"mod"
		"include"
)


file(GLOB GLOB_MATCHES
	${ESMF_INSTALL}/lib/lib*/*.*.*.*.*/libesmf.a
	${CMAKE_BINARY_DIR}/${ESMF_INSTALL}/lib/lib*/*.*.*.*.*/libesmf.a
	$ENV{ESMF_INSTALL_PREFIX}/lib/lib*/*.*.*.*.*/libesmf.a
	$ENV{ESMF_DIR}/DEFAULTINSTALLDIR/lib/lib*/*.*.*.*.*/libesmf.a
)
set(ESMF_LIBDIRS_GLOBBED "")
foreach(GLOBBED_FILE ${GLOB_MATCHES})
	get_filename_component(GLOBBED_DIR ${GLOBBED_FILE} DIRECTORY)
	list(APPEND ESMF_LIBDIRS_GLOBBED ${GLOBBED_DIR})
endforeach()
list(REMOVE_DUPLICATES  ESMF_LIBDIRS_GLOBBED)
find_library(ESMF_LIBRARIES
	esmf
	HINTS
		${ESMF_LIBDIRS_GLOBBED}
		${ESMF_INSTALL}
		${CMAKE_BINARY_DIR}/${ESMF_INSTALL}
		$ENV{ESMF_INSTALL_PREFIX}
		$ENV{ESMF_INSTALL_LIBDIR}
	DOC "The path to the directory containing \"libesmf.a\"."
	PATH_SUFFIXES
		"lib"
)


# Make a readable error message
set(ESMF_ERRMSG "\nFailed to find some files in your ESMF install that are needed. \
To resolve this you have two options:\n
  1) Set ESMF_INSTALL to the directory where ESMF was installed.")

if(IS_ABSOLUTE "${ESMF_INSTALL}" AND EXISTS "${ESMF_INSTALL}")
    set(ESMF_ERRMSG "${ESMF_ERRMSG}
	+ ESMF_INSTALL: ${ESMF_INSTALL}")
elseif(EXISTS "${CMAKE_BINARY_DIR}/${ESMF_INSTALL}")
    set(ESMF_ERRMSG "${ESMF_ERRMSG}
    + ESMF_INSTALL: ${CMAKE_BINARY_DIR}/${ESMF_INSTALL}")
else()
    set(ESMF_ERRMSG "${ESMF_ERRMSG}
    + ESMF_INSTALL: Path to where ESMF was installed")
endif()

set(ESMF_ERRMSG "${ESMF_ERRMSG}\n
  2) Manually set the following paths which are missing:")
if(NOT ESMF_INCLUDES_DIR)
	set(ESMF_ERRMSG "${ESMF_ERRMSG}
    + ESMF_INCLUDES_DIR: Path to directory with ESMF's \".inc\" files")
endif()
if(NOT ESMF_HEADERS_DIR)
	set(ESMF_ERRMSG "${ESMF_ERRMSG}
    + ESMF_HEADERS_DIR:  Path to directory with ESMF's \".h\" files")
endif()
if(NOT ESMF_MOD_DIR)
	set(ESMF_ERRMSG "${ESMF_ERRMSG}
    + ESMF_MOD_DIR:      Path to directory with ESMF's \".mod\" files")
endif()
if(NOT ESMF_LIBRARIES)
	set(ESMF_ERRMSG "${ESMF_ERRMSG}
    + ESMF_LIBRARIES:    Path to \"libesmf.a\"")
endif()
set(ESMF_ERRMSG "${ESMF_ERRMSG}\n\n")


find_package_handle_standard_args(ESMF 
	REQUIRED_VARS 
		ESMF_INCLUDES_DIR 
		ESMF_HEADERS_DIR 
		ESMF_MOD_DIR 
		ESMF_LIBRARIES
	FAIL_MESSAGE "${ESMF_ERRMSG}"
)

mark_as_advanced(ESMF_INSTALL)