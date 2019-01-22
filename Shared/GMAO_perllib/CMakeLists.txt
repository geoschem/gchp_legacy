set(perldirs
   ESMA
   GrADS
   Net
   Time
   XML
   )

foreach(perldir ${perldirs})
   file (COPY ${CMAKE_CURRENT_SOURCE_DIR}/${perldir} DESTINATION ${CMAKE_BINARY_DIR}/bin/
         PATTERN "CVS" EXCLUDE)
endforeach()
