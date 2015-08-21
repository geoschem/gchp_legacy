#!/bin/csh

  set fvroot = $1
  set mydir  = $2

  setenv FVROOT $fvroot
  cd $mydir

# source $FVROOT/bin/g5_modules

  setenv GEOSUTIL $FVROOT/../src/GMAO_Shared/GEOS_Util
  $GEOSUTIL/plots/configure
  source .quickplotrc

# Convert Original ECMWF.nc File to GFIO-Readable nc4 File (Creates:  grads.fwrite.nc4)
# -------------------------------------------------------------------------------------
  /bin/rm -f                                               grads.commands
  touch                                                    grads.commands
  echo \'sdfopen ecmwf.data\'                           >> grads.commands
  echo \'run $GEOSUTIL/plots/grads_util/writegrads.gs\' >> grads.commands
  echo \'q ctlinfo\'                                    >> grads.commands
  echo   write \(ctlinfo,\result\)                      >> grads.commands
  echo \'quit\'                                         >> grads.commands
  # This call assumes that the grads environment includes GEOS_Util/post/grads_utils
  # in the GASCRP setting
  grads    -b -l -c "run grads.commands"

  set failed = 0
  if (! -e grads.fwrite ) set failed = 1
  if (! -e ctlinfo      ) set failed = 1

  /bin/rm sedfile grads.commands grads.txt
  if ($failed) then
     echo "something did not work in grads conversion, aborting ..."
     exit(1)
  endif
  exit(0)

