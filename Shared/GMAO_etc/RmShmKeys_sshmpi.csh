#!/bin/csh -f

setenv MYNAME RmShmKeys_sshmpi

                                    setenv FAILED 0
if( (! $?FVROOT ) & (! $?GEOSBIN) ) setenv FAILED 1

if ( $FAILED ) then
   env
   echo " ${MYNAME}: not all required env vars defined"
   exit 1
endif

if( $?FVROOT ) then
   set pathname = $FVROOT/bin
endif
if( $?GEOSBIN ) then
   set pathname = $GEOSBIN
endif

setenv SITE `$pathname/g5_modules site`

if( $?PBS_NODEFILE ) then
   sleep 10

   set nodes = `cat $PBS_NODEFILE | uniq`

   if ( $SITE == NCCS ) then

      echo "Found site ${SITE}: using sshmpi"
      foreach node ($nodes)
         echo sshmpi $node $pathname/rmshmkeyhere.sh
              sshmpi $node $pathname/rmshmkeyhere.sh &
      end

   else if ( $SITE == NAS ) then

      echo "Found site ${SITE}: using pdsh"
      pdsh -f 128 -aF $PBS_NODEFILE $pathname/rmshmkeyhere.sh

   else

      echo "SITE: $SITE not supported for SHMEM! Contact GEOS Support"
      exit 2

   endif

   wait
endif
