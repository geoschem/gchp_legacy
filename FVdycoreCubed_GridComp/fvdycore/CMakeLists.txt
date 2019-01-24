esma_set_this ()

set (srcs
   tools/fv_treat_da_inc.F90
   model/lin_cloud_microphys.F90
   model/fv_cmp.F90
   geos_utils/fill_corner.F90
   geos_utils/flow_proj.F90
   geos_utils/ghost_cubsph.F90
   geos_utils/grid_utils_mod.F90
   geos_utils/cub2latlon.F90
   geos_utils/cub2cub.F90
   model/fv_arrays.F90
   tools/fv_mp_mod.F90
   tools/fv_eta.F90
   tools/fv_timing.F90
   model/fv_grid_utils.F90
   tools/sorted_index.F90
   tools/fv_grid_tools.F90
   model/a2b_edge.F90
   model/fv_fill.F90
   tools/init_hydro.F90
   model/fv_mapz.F90
   tools/fv_surf_map.F90
   model/fv_update_phys.F90
   model/fv_sg.F90
   tools/test_cases.F90
   tools/fv_diagnostics.F90
   tools/external_ic.F90
   tools/fv_restart.F90
   model/fv_control.F90
   model/tp_core.F90
   model/sw_core.F90
   model/fastexp.c
   model/nh_utils.F90
   model/nh_core.F90
   model/dyn_core.F90
   model/fv_tracer2d.F90
   model/fv_dynamics.F90
   tools/fv_io.F90
   tools/sim_nc_mod.F90
   model/boundary.F90
   tools/external_sst.F90
   tools/fv_nudge.F90
   model/fv_nesting.F90
   geos_utils/fv_regrid_c2c.F90
   geos_utils/fv_regridding_utils.F90
   geos_utils/rs_scaleMod.F90
   )


include_directories (${include_GFDL_fms})
include_directories (${include_MAPL_Base})
include_directories (${INC_ESMF})
include_directories (${INC_NETCDF})

add_definitions (-DMAPL_MODE -DSKIP_NO_QUAD_PRECISION -DREAD_GRID -DNO_GRID_G -DSPMD -DTIMING)

add_library (${this} ${srcs})
set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if (FV_PRECISION MATCHES R4)
   # Use default real
   add_definitions (-DSINGLE_FV -DOVERLOAD_R4)
   include_directories (${include_GFDL_fms_r4})
   target_link_libraries (${this} MAPL_Base GFDL_fms_r4)
else ()
   string(REPLACE " " ";" tmp ${FREAL8})
   include_directories (${include_GFDL_fms_r8})
   target_link_libraries (${this} MAPL_Base GFDL_fms_r8)
endif ()

foreach(flag ${tmp})
   target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
endforeach()


