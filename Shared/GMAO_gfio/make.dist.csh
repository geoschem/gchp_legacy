#!/bin/csh
#
#  Makes
#

   if ( $#argv < 1 ) then
	echo "Usage:       make.dist.csh  gfio_version"
	echo "Example:     make.dist.csh  v1.0.8"
	exit 1
   else
	set version = $1
   endif

#   Fresh start
#   -----------
    make distclean
   ./configure

   set arch = `uname`

   if ( $arch == "IRIX64" ) then

      make lib \
           FFLAGS='-extend_source -64 -r4 -I/ford1/local/IRIX64/hdf/include'
      if ( $status ) exit 1
      mv libgfio.a libgfio${version}_r4.a

      make clean
      make lib \
           FFLAGS='-extend_source -64 -r8 -I/ford1/local/IRIX64/hdf/include'

      if ( $status ) exit 1
      mv libgfio.a libgfio${version}_r8.a

      echo " "
      echo 'Created libraries: ' `ls libgfio*.a`
      echo " "

    else
	echo "$arch not supported"
        exit 1
    endif

    exit 0