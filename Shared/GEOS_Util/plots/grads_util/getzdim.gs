function getzdim (args)

*******************************************************
****                 INPUT Variables               ****
*******************************************************

EXPORT = subwrd(args,1)
    GC = subwrd(args,2)

'run getenv "GEOSUTIL"'
             geosutil = result

* Initialize
* ----------
'reinit'

'run getvar 'EXPORT' 'GC
        qname = subwrd(result,1)
        qfile = subwrd(result,2)
        scale = subwrd(result,3)
       expdsc = subwrd(result,4)
    if( qfile = 'NULL' ) ; return ; endif


* Model Experiment
* ----------------
'set dfile 'qfile
'setlons'
'set lat -90 90'

'getlevs   'qname
   nlevs = result
'run setenv 'EXPORT':'GC':NLEVS 'nlevs
return

