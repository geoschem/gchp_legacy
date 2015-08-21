* file: info.gs 
*    A script to extract and print file information. Input files may consist of
*    any GrADS readable data set (GrADS IEEE, GSFC Phoenix, GRIB,NetCDF or 
*    HDF-SDS).
*
*    Usage: info [filename]
*
* REVISION HISTORY:
*
* 28jun2001   Ardizzone  first version.
*
*......................................................................
*
* main(args)  Main driver 
*
function main ( args )

  _myname = 'info'

* Parse command line
* ------------------

  fname=subwrd(args,1)
  t1=subwrd(args,2)
  t2=subwrd(args,3)

  if (fname = ''); return; endif;
  rc = info(fname,t1,t2)

  'quit'

return

*............................................................................

*
* info(fname,t1,t2)
*

function info ( fname,t1,t2 )

  rc=openf(fname,'')
  if ( rc != 0 ); return 1; endif;

  'set dfile 'numopen()

  'q file'

  tmp = sublin ( result, 3 )
  dset = subwrd(tmp,2)

  tmp = sublin ( result, 5 )
  xdef = subwrd(tmp,3)
  ydef = subwrd(tmp,6)
  zdef = subwrd(tmp,9)
  tdef = subwrd(tmp,12)

  tmp = sublin ( result, 6 )
  vars = subwrd(tmp,5)

  i = 1
  while (i <= vars)

    tmp = sublin ( result, 6+i )
    var = subwrd(tmp,1)

    k=4
    title = ''
    str = subwrd(tmp,k)

    while (str != '')  
      title = title ' ' str
      k = k + 1
      str = subwrd(tmp,k)
    endwhile

    say 'var:'var':'title

    i = i + 1

  endwhile

  undef = getundef()

  _tmax = tdef
  getfreq()

  'set x 1 ' xdef
  'set y 1 ' ydef
  getdims()

  loninc = (_lonmax - _lonmin) / (xdef - 1)
  latinc = (_latmax - _latmin) / (ydef - 1)

  say 'binary:'dset
  say 'undef:'undef
  say 'xdef:'xdef':'_lonmin':'loninc
  say 'ydef:'ydef':'_latmin':'latinc
  say 'zdef:'zdef
  say 'vars:'vars
  say 'tdef:'tdef':'_deltat':'_freq
  say 'format:'_format

  if (t1 = '')
    'set t '1
    time1 = subwrd(result,4)
    'q time'
    t1 = subwrd(result,3)
  endif

  if (t2 = '')
    'set t 'tdef
    time2 = subwrd(result,4)
    'q time'
    t2 = subwrd(result,3)
  endif

  'set time 't1
  'q dims'
  tmp=sublin(result,5)
  index = subwrd(tmp,9)
  say 't1:'t1':'index':'time1

  'set time 't2
  'q dims'
  tmp=sublin(result,5)
  index = subwrd(tmp,9)
  say 't2:'t2':'index':'time2

  'close 'numopen()

  return 0
*.......................................................................

*
* openf(fname,ftype)  Opens a file according to the file type (ftype).
*                     If ftype is not specified it attempts to determine it
*                     by a heuristic algorithm;
*                     ftype can be 'ctl', 'sdf' or 'xdf'
*

function openf(fname,ftype)

*  Determine file type
*  -------------------
   if ( ftype = '' | ftype ='ftype' )
*                                       fname may be a template...
*                                       filen is always a file name
      filen = subwrd(fname,1)
      buf = read(filen)
      rc = sublin(buf,1)
      iret = close(filen)
      if ( rc != 0 )
           buf = read(filen'.ctl')
           rc = sublin(buf,1)
           iret = close(filen)
      endif
      if ( rc != 0 )
           say _myname 'cannot read file ' filen ' or ' filen '.ctl'
           return rc
      endif
      rec = sublin(buf,2)
      tok = subwrd(rec,1)
      if ( tok = 'dset' | tok='DSET' )
         is_xdf = 0
         i = 1
         tok = substr(filen,i,4)
         while ( tok != '' )
           if ( tok='.ddf' | tok='.DDF' ); is_xdr = 1; endif
           i = i + 1
           tok = substr(filen,i,4)
         endwhile
         if ( is_xdr = 1 )
              ftype = 'xdf'
         else
              ftype = 'ctl'
         endif
      else
         ftype = 'sdf'
      endif
   endif   

   _result = ''
   _format = 'unknown'

*  Open according to file type
*  ---------------------------
   if ( ftype = 'ctl' )
        'open ' fname
        _result = result
        _format = 'stream'
        ret = rc
   endif
   if ( ftype = 'sdf' ) 
        'sdfopen ' fname
        _result = result
        _format = 'coards'
        ret = rc
   endif
   if ( ftype = 'xdf' ) 
        'xdfopen ' fname
        _result = result
        _format = 'grib'
        ret = rc
   endif

   if (_result = '')

     say _myname 'cannot handle file type "' ftype '"'
     return 1

   endif

   if (ret != 0)

     say _myname '(openf): unable to open "' fname '"'
     say _result
     return 1

   endif

return 0

*
* getfreq() Examines two subsequent times and determine
*           time increment. This is heuristic and not
*           guaranteed to always work.
*

function getfreq()

  'set time 00Z01JAN1900'

  'q dims'
  tmp = sublin(result,5)
  t   = subwrd(tmp,9)

  'set t 't
  t1 = subwrd(result,4)
  y1 = subword(t1,':',1)
  m1 = subword(t1,':',2)
  d1 = subword(t1,':',3)
  h1 = subword(t1,':',4)

  'set t 't + 1
  t2 = subwrd(result,4)
  y2 = subword(t2,':',1)
  m2 = subword(t2,':',2)
  d2 = subword(t2,':',3)
  h2 = subword(t2,':',4)

  if (h1 != h2)

     _freq = hr
     _deltat = h2 - h1 + (d2 - d1) * 24
     return 

  endif

  if (d1 != d2)

     _freq = dy
     _deltat = d2 - d1
     return

  endif

  if (m1 != m2)

     _freq = mo
     _deltat = m2 - m1
     return

  endif
    
  _freq = yr
  _deltat = y2 - y1

return

function getundef()

  'q file'
  tmp = sublin(result,7)
  var = subwrd(tmp,1)

  'set gxout stat'
  'set t 1'
  'd ' var
  str = sublin(result,6)
  undef = subwrd(str,4)

return undef

********************************
function subword(str,delim,word)
********************************

i=1
n=1

while (n<=word)

char = ''
len  = 0
ipos = i

while (1)

   char=substr(str,i,1)
   if (char=''); break; endif
   i = i + 1
   if (char=delim); break; endif
   len = len + 1

endwhile

n = n + 1

endwhile

if (len=0); return ''; endif
return substr(str,ipos,len)

function numopen()

  'query files'

  i = 1
  n = 0
  str = sublin(result,i)
  str = subwrd(str,1)

  while (str = 'File')

    n = n + 1
    i = i + 3
    str = sublin(result,i)
    str = subwrd(str,1)

  endwhile

return n

function getdims()

   'q dims'

   tmp = sublin(result,2)
   type = subwrd(tmp,3)
   if ( type = 'varying' )
      _lonmin = subwrd(tmp,6)
      _lonmax = subwrd(tmp,8)
   else
      _lonmin = subwrd(tmp,6)
      _lonmax = _lonmin
   endif

   tmp = sublin(result,3)
   type = subwrd(tmp,3)
   if ( type = 'varying' )
      _latmin = subwrd(tmp,6)
      _latmax = subwrd(tmp,8)
   else
      _latmin = subwrd(tmp,6)
      _latmax = _latmin
   endif

return
