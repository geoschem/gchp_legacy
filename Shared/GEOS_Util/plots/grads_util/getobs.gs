function getobs (args)

EXPORT = subwrd(args,1)
GC     = subwrd(args,2)
RCFILE = subwrd(args,3)

* Ensure UpperCase EXPORT and GC
* ------------------------------
*'run uppercase 'EXPORT
*                EXPORT = result
*'run uppercase 'GC
*                GC     = result

* Get Info from RCFILE
* --------------------
'run getenv "GEOSUTIL"'
             geosutil = result
'!'geosutil'/plots/chckrc 'EXPORT' 'GC' 'RCFILE

obsnam = sublin( read(hist.txt),2 )
obsdsc = sublin( read(hist.txt),2 )
fname  = sublin( read(hist.txt),2 )
ftype  = sublin( read(hist.txt),2 )
qname  = sublin( read(hist.txt),2 )
scale  = sublin( read(hist.txt),2 )
            rc =close(hist.txt)

if( ftype = "NULL" )
    say EXPORT' from 'GC' not found!'
    return 'NULL NULL 1.0  NULL NULL'
endif

'getfile 'fname
          file = result

* Open Observation Dataset
* ------------------------
    say 'Opening: 'fname
    say '   Desc: 'obsdsc
    say '         '

if( file  = "NULL" )
if( ftype = "BIN"  ) ; '   open 'fname ; endif
if( ftype = "HDF"  ) ; 'xdfopen 'fname ; endif
if( ftype = "SDF"  ) ; 'sdfopen 'fname ; endif


* Set Dimension Environment
* -------------------------
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

return qname' 'numfiles' 'scale' 'obsdsc' 'obsnam
