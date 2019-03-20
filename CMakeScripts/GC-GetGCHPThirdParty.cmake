# Find required packages
find_package(OpenMP REQUIRED)
find_package(MPI REQUIRED)
find_package(ESMF REQUIRED)
find_package(NetCDF REQUIRED COMPONENTS F90)

string(STRIP "${MPI_Fortran_LINK_FLAGS}" MPI_Fortran_LINK_FLAGS)
target_compile_options(BaseTarget
	INTERFACE ${MPI_Fortran_COMPILE_FLAGS}
)
target_include_directories(BaseTarget
	INTERFACE ${NETCDF_F90_INCLUDE_DIR} ${MPI_Fortran_INCLUDE_PATH} ${THIRD_PARTY}/include
)
target_link_libraries(BaseTarget 
	INTERFACE 
		MAPL_Base MAPL_cfio_r4 GMAO_mpeu GMAO_pilgrim
		FVdycoreCubed_GridComp fvdycore GFDL_fms GEOS_Shared GMAO_hermes 
		${THIRD_PARTY_LIBNAMES} 
		${NETCDF_LIBRARIES} 
		${MPI_Fortran_LINK_FLAGS} ${MPI_Fortran_LIBRARIES} 
		${OpenMP_Fortran_FLAGS}
)

