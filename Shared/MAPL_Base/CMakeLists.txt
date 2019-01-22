esma_set_this ()

add_definitions(-DMAPL)
add_definitions(-DDO_COMMAS_SIDED_COMM)
add_definitions(-DTWO_SIDED_COMM)
add_definitions(-DHAVE_SHMEM)

add_library (${this}
  ESMFL_Mod.F90            MAPL_HeapMod.F90       MAPL_VarSpecMod.F90
  MAPL_Base.F90            MAPL_InterpMod.F90     hinterp.F
  MAPL_Throw.F90
  MAPL_Range.F90
  MAPL_Config.F90
  MAPL_CFIO.F90            MAPL_IO.F90
  MAPL_Comms.F90           MAPL_LocStreamMod.F90  MAPL_Mod.F90
  MAPL_Constants.F90       MAPL_Profiler.F90      sort.c
  MAPL_GenericCplComp.F90  MAPL_Sort.F90
  MAPL_Generic.F90         MAPL_sun_uc.F90        MAPL_SatVapor.F90
  MAPL_HistoryCollection.F90
  MAPL_HistoryGridComp.F90 MAPL_Cap.F90           hash.c
  MAPL_CapGridComp.F90
  MAPL_GridType.F90 
  MAPL_GridSpec.F90
  MAPL_VerticalInterpMod.F90

  MAPL_DirPath.F90
  MAPL_AbstractRegridder.F90
  MAPL_LatLonToLatLonRegridder.F90
  MAPL_TilingRegridder.F90
  Regrid_Functions_Mod.F90
  MAPL_ConservativeRegridder.F90
  MAPL_VotingRegridder.F90
  MAPL_FractionalRegridder.F90
  MAPL_IdentityRegridder.F90
  MAPL_CapGridComp.F90
  MAPL_KeywordEnforcer.F90
  MAPL_MinMax.F90  
  MAPL_AbstractGridFactory.F90 
  MAPL_IntegerVector.F90 
  MAPL_StringVector.F90 
  MAPL_StringIntegerMap.F90 
  MAPL_Integer64GridFactoryMap.F90 
  MAPL_StringGridFactoryMap.F90 
  MAPL_StringGridMap.F90
  MAPL_StringRouteHandleMap.F90 
  MAPL_GridManager.F90 
  MAPL_RegridderManager.F90 
  MAPL_RegridderSpec.F90 
  MAPL_RegridderVector.F90 
  MAPL_RegridderTypeSpecRegridderMap.F90
  MAPL_LatLonGridFactory.F90 
  MAPL_LatLonToLatLonRegridder.F90 
  MAPL_TripolarGridFactory.F90 
  c_mapl_locstream_F.c
  memuse.c                 MAPL_MemUtils.F90      MAPL_Hash.F90 
  MAPL_MaxMinMod.F90     MAPL_SimpleBundleMod.F90 
  MAPL_OrbGridCompMod.F90  MAPL_NominalOrbitsMod.F90 MAPL_ExtDataGridCompMod.F90 
  MAPL_NewArthParser.F90   MAPL_ShmemMod.F90      MAPL_LoadBalance.F90 MAPL_CFIOServer.F90
  CFIOCollection.F90 ESMF_CFIOPtrVectorMod.F90
  MAPL_TransposeRegridder.F90 MAPL_GetLatLonCoord.F90 MAPL_EsmfRegridder.F90
  )

target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${OpenMP_Fortran_FLAGS}>)
target_link_libraries (${this} MAPL_cfio_r4 ${ESMF_LIBRARIES})
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
# Kludge for OSX security and DYLD_LIBRARY_PATH ...
foreach(dir ${OSX_EXTRA_LIBRARY_PATH})
  target_link_libraries(${this} "-Xlinker -rpath -Xlinker ${dir}")
endforeach()


add_executable (cub2latlon.x cub2latlon_regridder.F90)
add_executable (Regrid_Util.x Regrid_Util.F90)
target_link_libraries (cub2latlon.x ${this} GMAO_pFIO ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
set_target_properties (cub2latlon.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}} LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_link_libraries (Regrid_Util.x ${this} GMAO_pFIO ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
set_target_properties (Regrid_Util.x PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}} LINK_FLAGS "${OpenMP_Fortran_FLAGS}")

#--------------------
include_directories (${INC_ESMF})
include_directories (${include_GMAO_pFIO})
include_directories (${BASEDIR}/gFTL/include)
include_directories (${INC_FTL})
include_directories (${INC_NETCDF})

include_directories(${include_MAPL_cfio_r4})
include_directories(${include_MAPL_cfio})

target_link_libraries (MAPL_Base MAPL_cfio_r4 ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES})

#--------------------
# Copy include files that are used by other libraries.
# We could leave these in the source directory, and just broaden the search path
# in the other libaries, but this make it explicit which aspects are externally
# used.
file (COPY unused_dummy.H DESTINATION ${include_${this}})
file (COPY MAPL_Generic.h DESTINATION ${include_${this}})
file (COPY MAPL_Exceptions.h DESTINATION ${include_${this}})
file (COPY MAPL_ErrLog.h DESTINATION ${include_${this}})
file (COPY mapl_acg.pl DESTINATION ${include_${this}})
file (COPY mapl_stub.pl DESTINATION ${include_${this}})

# Users guide 
if ($ENV{USE_LATEX} MATCHES NO)
else ()
  add_subdirectory (TeX)
endif ()


# Unit testing
if (PFUNIT)
  add_subdirectory(tests)
endif ()

