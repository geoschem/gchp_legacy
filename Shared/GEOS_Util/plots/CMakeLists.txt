add_executable(zonal.x zonal.f)
set_source_files_properties(zonal.f PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})

# Dependencies
include_directories(${include_MAPL_Base})
target_link_libraries(zonal.x MAPL_Base)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/quickplot ${CMAKE_CURRENT_BINARY_DIR}/quickplot COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/configure ${CMAKE_CURRENT_BINARY_DIR}/configure COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/landscape.script ${CMAKE_CURRENT_BINARY_DIR}/landscape.script COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/portrait.script ${CMAKE_CURRENT_BINARY_DIR}/portrait.script COPYONLY)
