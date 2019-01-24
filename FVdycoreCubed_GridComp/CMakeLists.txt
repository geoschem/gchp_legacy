esma_set_this ()

set (alldirs fvdycore)

set (srcs
  sw.f90 jw.f90 testcases_3_4_5_6_stand_alone.f90
  GetWeightsC2C.F90
  GetWeights.F90
  CubeHalo.F90
  Cube2LatLon.F90 LatLon2Cube.F90 AppGridCreate.F90 FV_StateMod.F90
  AdvCore_GridCompMod.F90
  DynCore_GridCompMod.F90 CreateInterpWeights_GridCompMod.F90
  StandAlone_DynAdvCore_GridCompMod.F90
  CubedSphereGridFactory.F90
  CubeToLatLonRegridder.F90 
  LatLonToCubeRegridder.F90 
  CubeToCubeRegridder.F90
  CubeToLatLon.F90
  )

include_directories (${include_FVdycoreCubed_GridComp})
include_directories (${include_fvdycore})
include_directories (${include_MAPL_Base})
include_directories (${include_GMAO_hermes})
include_directories (${include_GMAO_mpeu})
include_directories (${include_GEOS_shared})
include_directories (${INC_ESMF})

add_library (${this} ${srcs})
if (CRAY_POINTER)
  set_target_properties (${this} PROPERTIES COMPILE_FLAGS ${CRAY_POINTER})
endif()
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

add_definitions (-DSPMD -DMAPL_MODE)

if (FV_PRECISION MATCHES R4)
   # Use default real
   add_definitions (-DSINGLE_FV -DOVERLOAD_R4)
   include_directories (${include_GFDL_fms_r4})
   target_link_libraries (${this} MAPL_Base GMAO_mpeu GMAO_hermes GEOS_Shared GFDL_fms_r4)
else ()
   string(REPLACE " " ";" tmp ${FREAL8})
   include_directories (${include_GFDL_fms_r8})
   target_link_libraries (${this} MAPL_Base GMAO_mpeu GMAO_hermes GEOS_Shared GFDL_fms_r8)
endif ()

foreach(flag ${tmp})
   target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
endforeach()


add_executable (StandAlone_FV3_Dycore.x StandAlone_FV3_Dycore.F90)
set_target_properties (StandAlone_FV3_Dycore.x PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_link_libraries(StandAlone_FV3_Dycore.x ${this} MAPL_Base GMAO_pFIO)

add_executable (rs_scale.x rs_scale.F90)
set_target_properties (rs_scale.x PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
target_link_libraries(rs_scale.x ${this} )

esma_config_subcomponents (${this} ${alldirs})


