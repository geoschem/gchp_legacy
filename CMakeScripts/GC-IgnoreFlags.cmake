

function(get_warning_suppression_flags VARNAME)
    cmake_parse_arguments(DCW
        ""
        "LANGUAGE"
        "INTEL;GNU"
        ${ARGN}
    )

    if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
        string(REPLACE ";" "," INTEL_FLAGS "${DCW_INTEL}")
        set(FLAGS "-diag-disable ${INTEL_FLAGS}")
    elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
        string(REPLACE ";" " -Wno-" GNU_FLAGS "${DCW_GNU}")
        set(FLAGS "-Wno-${GNU_FLAGS}")
    else()
        message(WARNING "Unknown compiler ID.")
        return()
    endif()

    if(DEFINED DCW_LANGUAGE)
        set(${VARNAME} "$<$<COMPILE_LANGUAGE:${DCW_LANGUAGE}>:${FLAGS}>" PARENT_SCOPE)    
    else()
        set(${VARNAME} "${FLAGS}" PARENT_SCOPE)     
    endif()
endfunction()