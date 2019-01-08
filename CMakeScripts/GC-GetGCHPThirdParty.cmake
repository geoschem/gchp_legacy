# Get third party install path
set_dynamic_default(THIRD_PARTY "<path to third party install>"
    LOG THIRD_PARTY_LOG 
	IS_DIRECTORY
)
dump_log(THIRD_PARTY_LOG)
get_filename_component(THIRD_PARTY "${THIRD_PARTY}" ABSOLUTE)

# Add GCHP's third party libraries
set(THIRD_PARTY_LIBNAMES
	-L${THIRD_PARTY}/lib
	esmf 
    MAPL_Base MAPL_cfio GMAO_mpeu GMAO_pilgrim
    FVdycoreCubed_GridComp fvdycore GFDL_fms GEOS_Shared GMAO_hermes 
)

# Find NetCDF, MPI, and OpenMP and make them dependees
find_package(NetCDF REQUIRED COMPONENTS F90)
find_package(MPI REQUIRED)
find_package(OpenMP REQUIRED)
string(STRIP "${MPI_Fortran_LINK_FLAGS}" MPI_Fortran_LINK_FLAGS)
target_compile_options(BaseTarget
	INTERFACE ${MPI_Fortran_COMPILE_FLAGS}
)
target_include_directories(BaseTarget
	INTERFACE ${NETCDF_F90_INCLUDE_DIR} ${MPI_Fortran_INCLUDE_PATH} ${THIRD_PARTY}/include
)
target_link_libraries(BaseTarget 
	INTERFACE 
		${THIRD_PARTY_LIBNAMES} 
		${NETCDF_LIBRARIES} 
		${MPI_Fortran_LINK_FLAGS} ${MPI_Fortran_LIBRARIES} 
		${OpenMP_Fortran_FLAGS}
)
