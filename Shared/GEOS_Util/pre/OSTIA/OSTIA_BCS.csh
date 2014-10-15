#/bin/csh
# Many thanks to Joe Stassi

setenv ESMADIR /discover/nobackup/sakella/GEOSadas/

# OPS
 set ostia_data_path   = /archive/input/dao_ops/obs/flk/ukmet_sst/netcdf/OSTIA/

 set ostia_file_suff1  = -UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc.bz2
 set ostia_file_suff2  = -UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc
 set ostia_start_date  = 20060401
 set outdir            = /discover/nobackup/sakella/GEOSadas/src/GMAO_Shared/GEOS_Util/ostia_bcs
 set bz2Suff           = .bz2

 set real_proc         = 0

# Ocean resolution
 set nlat              = 1440
 set nlon              = 2880
#-------------------------------------------------------------------------------------------
 if ($real_proc == 1) then
#    get today's date
     set today              = `date "+%Y%m%d" `
     set process_data_start = $today
     set process_data_end   = `$ESMADIR/Linux/bin/tick $today `
 else
#    set process_data_start = $ostia_start_date
     set process_data_start = 20120101
     set process_data_end   = 20120601
     set date_next          = `$ESMADIR/Linux/bin/tick $process_data_start `
 endif
#-------------------------------------------------------------------------------------------
while ($process_data_start != $process_data_end)

     if ( $process_data_start == 20120101) then
     	set fileName4           = XXXX
     else
     	set fileName4           = $fileName3
     endif

     set yyyy             = `echo $process_data_start | cut -c1-4`
     set mm               = `echo $process_data_start | cut -c5-6`
     set dd               = `echo $process_data_start | cut -c7-8`

     set ops_filePath     = $ostia_data_path/Y$yyyy/M$mm/
     set fileName1        = $ops_filePath/$process_data_start$ostia_file_suff1
     set fileName2        = $outdir/$process_data_start$ostia_file_suff1
     set fileName3        = $process_data_start$ostia_file_suff2

##   delete any old input file 
     if ( -e input_stuff.txt) rm -f input_stuff.txt

     if ( -e $fileName1 ) then
 	dmget   $fileName1
 	cp      $fileName1 .
        bunzip2 $fileName2

        echo $process_data_start \ $date_next \ $fileName3 \ $nlat \ $nlon > input_stuff.txt
        echo "PROCESSING data from ..." $process_data_start "to " \ $date_next "using nc file: " \ $fileName3

##      read nc file, fill land pts, bin to desired resolution, dump sst & fraci bin files with header
        ./make_ostia_bcs.x

##      done with ostia netcdf file, del it
        rm -f $fileName3

     else

#       ostia netcdf file does not exist. 
        echo "UKMO- OSTIA FILE DOES NOT EXIST"
        echo $fileName1
        echo "Using following FILE FROM PREVIOUS DAY"
        echo $fileName4
        set  fileName4_1  = $fileName4$bz2Suff

 	dmget   $fileName4_1
 	cp      $fileName4_1 .
        bunzip2 $fileName4_1

        echo $process_data_start \ $date_next \ $fileName4 \ $nlat \ $nlon > input_stuff.txt
        echo "PROCESSING data from ..." $process_data_start "to " \ $date_next "using nc file: " \ $fileName4
##      read nc file, fill land pts, bin to desired resolution, dump sst & fraci bin files with header
        ./make_ostia_bcs.x
##      done with ostia netcdf file, del it
        rm -f $fileName4
     endif 

##   cat sst & fraci bin files
     if ( -e ostia_sst.bin ) then
           mv  ostia_sst.bin  sst_temp.bin
           cat sst_temp.bin   fort.91 > ostia_sst.bin
           rm -f fort.91 sst_temp.bin
     else
           mv fort.91 ostia_sst.bin
     endif 
     if ( -e ostia_fraci.bin ) then
           mv  ostia_fraci.bin fraci_temp.bin
           cat fraci_temp.bin  fort.92 > ostia_fraci.bin
           rm -f fort.92 fraci_temp.bin
     else
           mv fort.92 ostia_fraci.bin
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
#                     or else you want to process data- 20110101 onwards, then set process_data_start = 20110101.
#
# process_data_end:   should be the date up to which you want the data to be processed.
# 
# of course you also need to specify ESMADIR and outdir 
# unless you change names of sst & fraci files, they will be aggregated to ostia_sst.bin and ostia_fraci.bin respectively.
#
# MISSING DATA
# I assume that we do not miss data for consequetive days.
#-------------------------------------------------------------------------------------------
