# In most cases, GEOS follows a standard process to configure each
# component within the nested hierarchy.  The algorithm below codifies
# this standard process, thereby consiberably simplifying most
# CMakeLists.txt files in the code.

# Eventually the name of subcomponents and subdirectories will coincide.
# Waiting for git so that the rename is easy.


macro (esma_add_library this)

  if (CMAKE_DEBUG)
    message (STATUS "Generating build instructions for component: ${this}")
  endif ()
  set (options OPTIONAL EXCLUDE_FROM_ALL)
  set (multiValueArgs SRCS SUBCOMPONENTS SUBDIRS DEPENDENCIES INCLUDES NEVER_STUB)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  # Subdirs must exist and should be configured prior to subcomponents.
  foreach (subdir ${ARGS_SUBDIRS})
    add_subdirectory(${subdir})
  endforeach()

  # Configure subcomponents.  These can be stubbed and may have a
  # different name than the directory they reside in.  (Most
  # unfortunate.)
  set (non_stubbed)
  foreach (subdir ${ARGS_SUBCOMPONENTS})

    if (NOT rename_${subdir}) # usual case
      set (module_name ${subdir})
    else ()
      set(module_name ${rename_${subdir}})
    endif ()
    
    if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${subdir})
      if (CMAKE_DEBUG)
	message (STATUS  "  ... Creating stub component ${module_name}")
      endif()
      add_subdirectory (${subdir})
      list (APPEND non_stubbed ${subdir})
    else () # make stub and append to srcs (in ARGS_SRCS)
      if (CMAKE_DEBUG)
	message (STATUS  "  ... Creating stub component ${module_name}")
      endif()
      esma_create_stub_component (ARGS_SRCS ${module_name})
    endif ()

  endforeach ()

  if (NOT TARGET ${this})
    add_library (${this} "")
  endif ()
  target_sources(${this} PRIVATE ${ARGS_SRCS})
  set_target_properties(${this} PROPERTIES EXCLUDE_FROM_ALL ${ARGS_EXCLUDE_FROM_ALL})
  set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${esma_include}/${this})
  # Export target  include directories for other targets
  target_include_directories(${this} PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR} # stubs
    ${esma_include}/${this}) # modules and copied *.h, *.inc

  # This library depends on all DEPENDENCIES and _non-stubbed_ subcomponents.
  foreach (dependency ${ARGS_DEPENDENCIES} ${non_stubbed})
    target_link_libraries(${this} PUBLIC ${dependency})
  endforeach()

  if (ARGS_INCLUDES)
    target_include_directories(${this} PUBLIC ${ARGS_INCLUDES})
  endif ()

endmacro ()
