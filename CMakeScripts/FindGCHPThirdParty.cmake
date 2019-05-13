find_path(GCHP_THIRDPARTY_TARGETS
    FindGCHPThirdParty-${CMAKE_Fortran_COMPILER_ID}.${CMAKE_Fortran_COMPILER_VERSION}.cmake
    PATHS 
        "${GCHP_THIRDPARTY_INSTALL}"                        # for absolute path
        "${CMAKE_BINARY_DIR}/${GCHP_THIRDPARTY_INSTALL}"    # for relativel path
	PATH_SUFFIXES "cmake"
)

find_package_handle_standard_args(GCHPThirdParty
	REQUIRED_VARS GCHP_THIRDPARTY_TARGETS
    FAIL_MESSAGE "Failed to find \"GCHPThirdParty-${CMAKE_Fortran_COMPILER_ID}.${CMAKE_Fortran_COMPILER_VERSION}.cmake\". This file is installed when you build GCHP's third party libraries.\nSet GCHP_THIRDPARTY_INSTALL to the directory containing this file.\n"
)

set(EXPORTED_GCHP_THIRDPARTY "${GCHP_THIRDPARTY_TARGETS}/FindGCHPThirdParty-${CMAKE_Fortran_COMPILER_ID}.${CMAKE_Fortran_COMPILER_VERSION}.cmake")
list(APPEND CMAKE_MODULE_PATH ${GCHP_THIRDPARTY_TARGETS})
find_package(GCHPThirdParty-${CMAKE_Fortran_COMPILER_ID}.${CMAKE_Fortran_COMPILER_VERSION} REQUIRED)
