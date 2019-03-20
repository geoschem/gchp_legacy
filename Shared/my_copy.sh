#!/bin/bash

AGCM=$1

for CMakeFile in $(find ${AGCM} -name "CMakeLists.txt" | sed -n "s|^${AGCM}/||p")
do
	[ -f $CMakeFile ] && [ -f ${AGCM}/$CMakeFile ] && echo "Match: $(ls $CMakeFile)" && cp ${AGCM}/$CMakeFile $CMakeFile
	[ -f ${AGCM}/$CMakeFile ] && [ ! -f $CMakeFile ] && echo "Skipping: $(ls ${AGCM}/$CMakeFile)"

done

