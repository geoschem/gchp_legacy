#/bin/csh

setenv ESMADIR /discover/nobackup/sakella/GEOSadas/

set  lats4d_path = /discover/nobackup/projects/gmao/share/dasilva/opengrads/Contents/

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
 set nlat                 =  720
 set nlon                 = 1440
#-------------------------------------------------------------------------------------------
 set process_data_start = 20120101
 set process_data_end   = 20130101
 set date_next          = `$ESMADIR/Linux/bin/tick $process_data_start `
#-------------------------------------------------------------------------------------------
while ($process_data_start != $process_data_end)

# date info
     set yyyy                = `echo $process_data_start | cut -c1-4`
     set mm                  = `echo $process_data_start | cut -c5-6`
     set dd                  = `echo $process_data_start | cut -c7-8`

# Ostia file info
     set ostia_filePath      = $ostia_data_path/Y$yyyy/M$mm/
     set fileName1           = $ostia_filePath/$process_data_start$ostia_file_suff1
     set fileName2           = $outdir/$process_data_start$ostia_file_suff1
     set Ostia_file          = $process_data_start$ostia_file_suff2
#
#    set ostia_filePath      = $ostiaran_data_path/Y$yyyy/M$mm/
#    set fileName1           = $ostia_filePath/$process_data_start$ostiaran_file_suff1
#    set fileName2           = $outdir/$process_data_start$ostiaran_file_suff1
#    set Ostia_file          = $process_data_start$ostiaran_file_suff2

# Reynolds file info
     set reynolds_filePath   = $reynolds_data_path/$yyyy/
     set Reynolds_file       = $reynolds_filePath/${reynolds_file_suff1}${process_data_start}.nc

     set lats4d_out1         = temp_ostia_quart_deg
     set lats4d_out2         = temp_ostia_quart_deg.nc
#    -------------------------------------------------------------------------------------------

#   delete any old input file 
     if ( -e input_quart.txt) rm -f input_quart.txt

     if ( -e $fileName1 ) then
        dmget   $fileName1
        cp      $fileName1 .
        bunzip2 $fileName2

#       run lats4d; get a temporary nc file
        $lats4d_path/lats4d.sh -v -i $Ostia_file \
                               -o $lats4d_out1 \
                               -format netcdf \
                               -de my_quart \
                               -func "re(@,1440,linear,-179.875,0.25,720,linear,-89.875,0.25,ba)"
 
        /bin/rm -f input_quart.txt
        echo ${process_data_start}                                                  > input_quart.txt
        echo ${date_next}                                                          >> input_quart.txt
        echo $Reynolds_file                                                        >> input_quart.txt
        echo $lats4d_out2                                                          >> input_quart.txt
        echo $nlat                                                                 >> input_quart.txt
        echo $nlon                                                                 >> input_quart.txt

        echo "PROCESSING data from ..." $process_data_start "to " \ $date_next

        cat input_quart.txt

#       make SST & FRACI Boundary Condition files
        /discover/nobackup/sakella/GEOSadas-5_11_0_p1/GEOSadas/src/GMAO_Shared/GEOS_Util/pre/NSIDC-OSTIA_SST-ICE_blend/new_code/merra2/src/quart_ostia_sst_ice.x input_quart.txt

#       done with ostia netcdf file, del it
        /bin/rm -f $Ostia_file
        /bin/rm -f $lats4d_out2
#       *****************  I DO NOT NEED PROCESSED FILES- now! BUT OPS WILL NEED. 
#       ***************** DEL FOLLOWING LINES FOR OPS. BECAUSE OPS SHOULD KEEP quart deg Reynolds data for ...!
       /bin/rm -f Reynolds_sst_*.bin
       /bin/rm -f Reynolds_ice_*.bin
     endif
     # PROCESS next day file
     set process_data_start  = `$ESMADIR/Linux/bin/tick $process_data_start `
     set date_next           = `$ESMADIR/Linux/bin/tick $process_data_start `
end
#-------------------------------------------------------------------------------------------

