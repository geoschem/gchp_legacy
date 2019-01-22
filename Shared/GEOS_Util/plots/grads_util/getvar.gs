function getvar (args)

EXPORT = subwrd(args,1)
GC     = subwrd(args,2)
SOURCE = subwrd(args,3)

* Ensure UpperCase EXPORT and GC
* ------------------------------
*'run uppercase 'EXPORT
*                EXPORT = result
*'run uppercase 'GC
*                GC     = result

* Get Info from HISTORY.rc File
* -----------------------------
'run getenv "GEOSUTIL"'
             geosutil = result

'run getenv "PLOTS_DIR"'
             PLOTS_DIR = result
'!./chckhist 'EXPORT' 'GC' 'SOURCE

expdsc = sublin( read(hist.txt),2 )
qname  = sublin( read(hist.txt),2 )
qfile  = sublin( read(hist.txt),2 )
scale  = sublin( read(hist.txt),2 )
format = sublin( read(hist.txt),2 )
base   = sublin( read(hist.txt),2 )
           rc = close(hist.txt)

if( qfile = "NULL" )
    say EXPORT' from 'GC' not found in HISTORY.rc!'
    return 'NULL NULL 1 NULL NULL NULL'
endif

'getfile 'qfile
           file = result

* Open New Experiment Dataset
* ---------------------------
say 'Opening: 'qfile
say '   Desc: 'expdsc
say '         '

if( file               = "NULL" )
if( substr(format,1,4) = "flat" ) ; '   open 'qfile ; endif
if( substr(format,1,4) = "CFIO" ) ; 'xdfopen 'qfile ; endif
if( substr(format,1,3) = "HDF"  ) ; 'sdfopen 'qfile ; endif

'getinfo numfiles'
         numfiles = result

* Set CASHE SIZE
* --------------
'getinfo file'
      curfile = result

'set dfile 'numfiles
'getinfo xdim'
         xdim = result
'getinfo ydim'
         ydim = result
         cash = xdim * ydim * 4

'run getenv CASHESZ'
            CASHESZ = result

if( CASHESZ = 'NULL' )
    say 'setting cachesf 'cash
   'set cachesf 'cash
   'run setenv "CASHESZ" 'cash
endif

if( CASHESZ != 'NULL' & cash > CASHESZ )
       say 'updating cachesf 'cash
      'set cachesf 'cash
      'run setenv "CASHESZ" 'cash
endif

'set dfile 'curfile

endif

return qname' 'numfiles' 'scale' 'expdsc' 'base
