get_filename_component(this "${CMAKE_CURRENT_SOURCE_DIR}" NAME)

add_definitions (-DHERMES)
include_directories (${include_GMAO_mpeu} ${include_MAPL_Base} ${include_GEOS_Shared})
include_directories (${include_MAPL_cfio_r8} ${include_GMAO_gfio_r8})
include_directories (${include_${this}})

string (REPLACE " " ";" flags ${FREAL8})
add_compile_options(${flags})

add_library (${this}
  m_ana2dyn.F m_const.f90 m_dyn.f90 m_dyn2dyn.f90 m_dynp.F90
  m_insitu.F m_interp.F90 m_lp.F m_maph.F90 m_maph_pert.F90 m_mapz.F m_mapz_pert.F90
  m_qsat.F m_remap.f90 m_set_eta.F90 m_vdc2vdc.F90 progsig.f90 m_topo_remap.F90
  m_interpack.F m_interpack_ad.F m_ec_set_eta.F90 m_ecdyn.f90 m_tick.f90
  m_dyn_util.F90 m_spline.f90
  )

target_link_libraries (${this} GMAO_mpeu GMAO_gfio_r8)
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

set_source_files_properties (m_interpack.F PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
set_source_files_properties (m_interpack_ad.F PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
set_source_files_properties (m_ana2dyn.F PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
set_source_files_properties (m_insitu.F PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
set_source_files_properties (GFIO_Shave.f90 PROPERTIES COMPILE_FLAGS ${PP})
set_source_files_properties (GFIO_remap.f90 PROPERTIES COMPILE_FLAGS ${PP})

add_executable (dyndims.x dyndims.f)
add_executable (GFIO_remap.x GFIO_remap.f90)
add_executable (GFIO_Shave.x GFIO_Shave.f90)
add_executable (echorc.x echorc.f90)
add_executable (fvsubset.x fvsubset.F90)

target_link_libraries (dyndims.x ${this})
target_link_libraries (GFIO_remap.x GMAO_gfio_r8 ${this} MAPL_cfio_r8)
target_link_libraries (GFIO_Shave.x GMAO_gfio_r8 ${this} MAPL_cfio_r8)
target_link_libraries (echorc.x ${this})
target_link_libraries (fvsubset.x ${this} GMAO_gfio_r8)



