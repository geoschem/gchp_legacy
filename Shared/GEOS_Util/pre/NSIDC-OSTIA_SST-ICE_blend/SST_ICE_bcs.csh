#/bin/csh
# REVISION HISTORY:
# 02Jan2013  Akella      SST Blend: OSTIA + Reynolds[lakes]; SEA-ICE: NSIDC + Reynolds

setenv ESMADIR /discover/nobackup/sakella/GSI-TMI/GEOSadas/

#set data dir locations
 set reynolds_data_path   = /discover/nobackup/dao_ops/intermediate/flk/stage/reynolds_daily/
 set ostia_data_path      = /archive/input/dao_ops/obs/flk/ukmet_sst/netcdf/OSTIA/

#NSIDC data location
 set nsidc_NH_data_path      = /gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/NSIDC/NRT/north/
 set nsidc_SH_data_path      = /gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/NSIDC/NRT/south/

#ostia specific file- suffix & prefix info
 set ostia_file_suff1     = -UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc.bz2
 set ostia_file_suff2     = -UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc

#output location
 set outdir               = /discover/nobackup/sakella/GSI-TMI/GEOSadas/src/GMAO_Shared/GEOS_Util/pre/NSIDC-OSTIA_SST-ICE_blend/OUT_2/
 set bz2Suff              = .bz2

# Ocean resolution
 set nlat                 = 1440
 set nlon                 = 2880

# whether to output: OSTIA-Reynolds Blend [1:yes, 0:no]
  set save_ostia_reynolds_ice = 0
#-------------------------------------------------------------------------------------------
 set process_data_start = 20120101
 set process_data_end   = 20120712
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

# NSIDC file info
     set NSIDC_NH_file       = $nsidc_NH_data_path/${yyyy}/nt_${yyyy}${mm}${dd}_f17_nrt_n.bin
     set NSIDC_SH_file       = $nsidc_SH_data_path/${yyyy}/nt_${yyyy}${mm}${dd}_f17_nrt_s.bin

# Reynolds file info
     set reynolds_filePath   = $reynolds_data_path
     set Reynolds_file       = $reynolds_filePath/avhrr-only-v2.${process_data_start}.nc

# NSIDC NH & SH files info


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
        echo $NSIDC_NH_file                                                        >> input_stuff.txt
        echo $NSIDC_SH_file                                                        >> input_stuff.txt
        echo $Ostia_file                                                           >> input_stuff.txt
        echo $nlat                                                                 >> input_stuff.txt
        echo $nlon                                                                 >> input_stuff.txt
        echo $save_ostia_reynolds_ice                                              >> input_stuff.txt
        
        echo "PROCESSING data from ..." $process_data_start "to " \ $date_next 

        cat input_stuff.txt

#       make SST & FRACI Boundary Condition files
        $ESMADIR/src/GMAO_Shared/GEOS_Util/pre/NSIDC-OSTIA_SST-ICE_blend/./make_ostia_nsidc_bcs.x input_stuff.txt

#       done with ostia netcdf file, del it
        /bin/rm -f $Ostia_file
     endif 

     # PROCESS next day file
     set process_data_start  = `$ESMADIR/Linux/bin/tick $process_data_start `
     set date_next           = `$ESMADIR/Linux/bin/tick $process_data_start `
end
#-------------------------------------------------------------------------------------------
# USAGE INFO.:
# Set process_data_start & process_data_end
# process_data_start: should be the starting date from which onwards you want to gather data.
#                     for e.g., if you want to process data from UKMO-OSTIA start date (Apr 1, 2006) then set process_data_start = 20060401.
#                     or else you want to process data- 20120101 onwards, then set process_data_start = 20120101.
#
# process_data_end:   should be the date up to which you want the data to be processed.
# 
# of course you also need to specify ESMADIR and outdir 
#
# output: sst & fraci files for each day will be saved in current processing location: sst_YYYYMMDD.bin & fraci_YYYYMMDD.bin
#
# MISSING DATA
# I assume that we do not miss data for consequetive days.
#-------------------------------------------------------------------------------------------
