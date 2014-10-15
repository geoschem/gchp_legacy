#/bin/csh
# REVISION HISTORY:
# 16Jul2013  Akella      OSTIA for forward processing & RPIT, Reynolds 1/4 for MERRA2

setenv ESMADIR /discover/nobackup/sakella/GEOSadas/

#Reynolds data location
 set reynolds_data_path   = /gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/REYN/
#1982- 2002, 2011/10/05- present [ ~last 10 days file template:  avhrr-only-v2.YYYYMMDD__preliminary.nc --> I am ignoring these]
 set reynolds_file_suff1  = avhrr-only-v2.
#2002/06- 2011/10/04 
 set reynolds_file_suff2  = amsr-avhrr-v2.

#Ostia RAN file- suffix & prefix info
#1985- 2006.
 set ostiaran_data_path      = /archive/input/dao_ops/ops/flk/ukmet_sst/netcdf/OSTIA_RAN/
 set ostiaran_file_suff1     = -UKMO-L4HRfnd-GLOB-v01-fv02-OSTIARAN.nc.bz2
 set ostiaran_file_suff2     = -UKMO-L4HRfnd-GLOB-v01-fv02-OSTIARAN.nc

#Ostia file- suffix & prefix info
#04/2006- present.
 set ostia_data_path      = /archive/input/dao_ops/obs/flk/ukmet_sst/netcdf/OSTIA/
 set ostia_file_suff1     = -UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc.bz2
 set ostia_file_suff2     = -UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc
#-------------------------------------------------------------------------------------------
#output location
 set outdir               = /discover/nobackup/sakella/GEOSadas-5_11_0_p1/GEOSadas/src/GMAO_Shared/GEOS_Util/pre/NSIDC-OSTIA_SST-ICE_blend/new_code/merra2/data/
 set bz2Suff              = .bz2

# Ocean resolution
 set nlat                 = 1440
 set nlon                 = 2880

# whether to output for MERRA2 using Reynolds? [1:yes, 0:no]
  set iMerra = 0
#-------------------------------------------------------------------------------------------
 set process_data_start = 19850101
 set process_data_end   = 19860101
 set date_next          = `$ESMADIR/Linux/bin/tick $process_data_start `
#-------------------------------------------------------------------------------------------
while ($process_data_start != $process_data_end)

# date info
     set yyyy                = `echo $process_data_start | cut -c1-4`
     set mm                  = `echo $process_data_start | cut -c5-6`
     set dd                  = `echo $process_data_start | cut -c7-8`

# Ostia file info
     set ostia_filePath      = $ostiaran_data_path/Y$yyyy/M$mm/
     set fileName1           = $ostia_filePath/$process_data_start$ostiaran_file_suff1
     set fileName2           = $outdir/$process_data_start$ostiaran_file_suff1
     set Ostia_file          = $process_data_start$ostiaran_file_suff2

# Reynolds file info
     set reynolds_filePath   = $reynolds_data_path/$yyyy/
     set Reynolds_file       = $reynolds_filePath/${reynolds_file_suff1}${process_data_start}.nc

##   delete any old input file 
     if ( -e input_stuff.txt) rm -f input_stuff.txt

     if ( -e $fileName1 ) then
        dmget   $fileName1
        cp      $fileName1 .
        bunzip2 $fileName2

        /bin/rm -f input_stuff.txt
        echo ${process_data_start}                                                  > input_stuff.txt
        echo ${date_next}                                                          >> input_stuff.txt
        echo $Reynolds_file                                                        >> input_stuff.txt
        echo $Ostia_file                                                           >> input_stuff.txt
        echo $nlat                                                                 >> input_stuff.txt
        echo $nlon                                                                 >> input_stuff.txt
        echo $iMerra                                                               >> input_stuff.txt

        echo "PROCESSING data from ..." $process_data_start "to " \ $date_next

        cat input_stuff.txt

#       make SST & FRACI Boundary Condition files
	/discover/nobackup/sakella/GEOSadas-5_11_0_p1/GEOSadas/src/GMAO_Shared/GEOS_Util/pre/NSIDC-OSTIA_SST-ICE_blend/new_code/merra2/src/ostia_sst_ice_NO_NSIDC.x input_stuff.txt

#       done with ostia netcdf file, del it
        /bin/rm -f $Ostia_file
     endif
     # PROCESS next day file
     set process_data_start  = `$ESMADIR/Linux/bin/tick $process_data_start `
     set date_next           = `$ESMADIR/Linux/bin/tick $process_data_start `
end
#-------------------------------------------------------------------------------------------


