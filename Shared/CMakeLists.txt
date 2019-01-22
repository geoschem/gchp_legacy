set (alldirs
  GMAO_mpeu GMAO_pilgrim GMAO_etc
  GMAO_pFIO 
  MAPL_Base 
  GEOS_Shared GMAO_hermes GEOS_Util 
  Chem_Base Chem_Shared
  LANL_Shared
  GMAO_perllib
  )

if (PFUNIT)
  add_subdirectory (MAPL_pFUnit EXCLUDE_FROM_ALL)
endif ()

esma_build_subcomponents (${alldirs})

# Special cases - MAPL_cfio, GMAO_gfio, and GFDL_fms are built twice with two
# different precisions.
add_subdirectory (MAPL_cfio MAPL_cfio_r4)
add_subdirectory (MAPL_cfio MAPL_cfio_r8)

add_subdirectory (GMAO_gfio GMAO_gfio_r4)
add_subdirectory (GMAO_gfio GMAO_gfio_r8)
add_dependencies (GMAO_gfio_r4 GMAO_gfio_r8)

add_subdirectory (GFDL_fms GFDL_fms_r4)
add_subdirectory (GFDL_fms GFDL_fms_r8)
