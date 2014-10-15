function genpltz (args)

'numargs  'args
 numargs = result

qmax = NULL
qmin = NULL

        num = 0
while ( num < numargs )
        num = num + 1

if( subwrd(args,num) = '-EXPID'   ) ; expid    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-EXPORT'  ) ; EXPORT   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-GC'      ) ; GC       = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-ALIAS'   ) ; alias    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-QFILE'   ) ; qfile    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-OFILE'   ) ; ofile    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-ONAME'   ) ; obsnam   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-OBDATE'  ) ; begdateo = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-OEDATE'  ) ; enddateo = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-NMOD'    ) ; nmod     = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-NOBS'    ) ; nobs     = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-QDESC'   ) ; qdesc    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-ODESC'   ) ; odesc    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-OUTPUT'  ) ; output   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-SEASON'  ) ; season   = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-PTOP'    ) ; ptop     = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-MAX'     ) ; qmax     = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-MIN'     ) ; qmin     = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-ZLOG'    ) ; zlog     = subwrd(args,num+1) ; endif

endwhile


* Check for Contour Level Type
* ----------------------------
'run getenv "LEVTYPE"'
             LEVTYPE = result


say '-EXPID 'expid
say '-EXPORT 'EXPORT
say '-ALIAS  'alias
say '-QFILE 'qfile
say '-OFILE 'ofile
say '-ONAME 'obsnam
say '-OBDATE 'begdateo
say '-OEDATE 'enddateo
say '-NMOD 'nmod
say '-NOBS 'nobs
say '-QDESC 'qdesc
say '-ODESC 'odesc
say '-OUTPUT 'output
say '-SEASON 'season
say '-MAX    'qmax
say '-MIN    'qmin

* Get Dates for Plots
* -------------------
'run getenv "BEGDATE"'
             begdate  = result
'run getenv "ENDDATE"'
             enddate  = result
if( begdate = "NULL" )
   'set dfile 'qfile
   'set t    '1
   'getinfo date'
         begdate = result
endif
if( enddate = "NULL" )
   'set dfile 'qfile
   'getinfo tdim'
            tdim     = result
   'set t  'tdim
   'getinfo date'
         enddate = result
endif

'run getenv "CLIMATE"'
             climate = result
if( begdate = begdateo & enddate = enddateo )
         climate = 'Actual'
endif

'set gxout shaded'

* Get Plotting Values from Resource File
* --------------------------------------
'run getenv "GEOSUTIL"'
             geosutil = result
PLOTRC = geosutil'/plots/grads_util/plot.rc'
 
say ''
                        'getresource 'PLOTRC' 'EXPORT'_'GC'_TITLE'  ; title   = result
                        'getresource 'PLOTRC' 'EXPORT'_'GC'_FACTOR' ; fact    = result

                        'getresource 'PLOTRC' 'EXPORT'_'GC'_Z_CCOLS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'EXPORT'_'GC'_CCOLS' ; endif
                                                            ccols = result

                        'getresource 'PLOTRC' 'EXPORT'_'GC'_Z_DCOLS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'EXPORT'_'GC'_DCOLS' ; endif
                                                             dcols = result

                        'getresource 'PLOTRC' 'EXPORT'_'GC'_Z_CLEVS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'EXPORT'_'GC'_CLEVS' ; endif
                                                            clevs = result

                        'getresource 'PLOTRC' 'EXPORT'_'GC'_Z_'LEVTYPE'LEVS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'EXPORT'_'GC'_'LEVTYPE'LEVS' ; endif
                                                             dlevs = result

say ''
say 'CLEVS: 'clevs
say 'DLEVS: 'dlevs
say 'DCOLS: 'dcols

if( fact    = 'NULL' ) ; fact    = 1            ; endif
if( title   = 'NULL' )
  'getdesc 'EXPORT
     title  = result
endif

    oname = '/hdiag_'obsnam'_'EXPORT'.'GC'_z'

* Remove possible BLANKS from FACTOR
* ----------------------------------
DESC = ''
length = getlength(fact)
i = 1
while( i<=length )
  bit = substr(fact,i,1)
  if( bit != ' ' )
      if( DESC = '' )
          DESC = bit
      else
          DESC = DESC''bit
      endif
  endif
i = i+1
endwhile
fact = DESC





* Make Mean Plot
* --------------
'set vpage off'
'set parea off'
'set grid  off'
'set frame on'
'set xlopts 1 3 .11'
'set ylopts 1 3 .11'
'rgbset'

'set dfile 'qfile
'set lat -90 90'
'set lon 0'
'set lev 1000 'ptop
'set t 1'

'set vpage 0 8.5 0.0 11'
'set parea 1.5 7.0 7.70 10.50'
'set grads off'
'set gxout shaded'
'set zlog 'zlog
 if( zlog = ON ) ; 'setlevs' ; endif

if( ccols = NULL )
   'shades modz*'fact' 0'
else
   'set clevs 'clevs
   'set ccols 'ccols
endif

'd modz*'fact
'draw ylab Pressure (mb)'
'set gxout contour'
'set ccolor 1'
'set clevs 'clevs
'd modz*'fact
'set parea 0 8.5 7.0 11'
'cbarn -vert'
'set parea off'

'set dfile 'ofile
'set lat -90 90'
'set lon 0'
'set lev 1000 'ptop
'set t 1'
'set vpage 0 8.5 0.0 11'
'set parea 1.5 7.0 4.30 7.10'
'set grads off'
'set gxout shaded'
'set zlog 'zlog
 if( zlog = ON ) ; 'setlevs' ; endif

if( ccols = NULL )
   'shades modz*'fact' 0'
else
   'set clevs 'clevs
   'set ccols 'ccols
endif

'd obsz*'fact
'draw ylab Pressure (mb)'
'set gxout contour'
'set clevs 'clevs
'set ccolor 1'
'd obsz*'fact
'set parea off'

'set dfile 'qfile
'set lat -90 90'
'set lon 0'
'set lev 1000 'ptop
'set t 1'
'set vpage 0 8.5 0.0 11'
'set parea 1.5 7.0 0.90 3.70'
'set grads off'
'set gxout shaded'
'set zlog 'zlog
 if( zlog = ON ) ; 'setlevs' ; endif

        n = 0
if( dcols = NULL )
   'd abs('qmin'*'fact')'
           qmin = subwrd(result,4)
   'd abs('qmax'*'fact')'
           qmax = subwrd(result,4)
   if( qmin > qmax ) ; qmax = qmin ; endif
   if( qmax > 0 )
      'd log10('qmax')'
       n = subwrd(result,4)
   else
       n = 0
   endif
   say '    Log Factor: 'n
   if( n<0 ) ; n = n-2 ; endif
   'getint 'n
            n = result
   if( n>0 )
       if( n<=2 )
           n = 0
        else
           n = n+2
        endif
   endif
   say 'Scaling Factor: 'n
      'd 0.1*'qmax'/1e'n
       cint = subwrd(result,4)
      'shades 'cint
      'd qz*'fact'/1e'n
else
      'set clevs 'dlevs
      'set ccols 'dcols
      'd qz*'fact
endif

'draw ylab Pressure (mb)'
'set gxout contour'
'set ccolor 1'
if( dcols = NULL )
   'set clevs -'cint' 'cint
else
   'set clevs 'dlevs
endif
'd qz*'fact'/1e'n
'cbarn -snum 0.55'

'set vpage off'
'set string 1 c 6'
'set strsiz .13'
'draw string 4.25 10.85 'title

'set strsiz .11'
'draw string 4.25 10.635 EXPID: 'expid'  'qdesc' 'season' ('nmod')'
'draw string 4.25  7.235 'odesc' 'season' ('nobs') ('climate')'
if( n != 0 )
'draw string 4.25  3.850 Difference (Top-Middle) (x 10**'n')'
else
'draw string 4.25  3.850 Difference (Top-Middle)'
endif

                date = getdate (begdate)
bmnthm = subwrd(date,1)
byearm = subwrd(date,2)
                date = getdate (enddate)
emnthm = subwrd(date,1)
eyearm = subwrd(date,2)
                date = getdate (begdateo)
bmntho = subwrd(date,1)
byearo = subwrd(date,2)
                date = getdate (enddateo)
emntho = subwrd(date,1)
eyearo = subwrd(date,2)

'set string 1 l 4'
'set strsiz .08'
'draw string 0.10 10.37 Beg: 'bmnthm' 'byearm
'draw string 0.10 10.24 End: 'emnthm' 'eyearm
'draw string 0.10  6.97 Beg: 'bmntho' 'byearo
'draw string 0.10  6.84 End: 'emntho' 'eyearo
'set string 1 c 6'

'myprint -name 'output'/'oname'.'season

return

function getdate (date,month,year)
       num = 1
       bit = substr(date,num,1)
while( bit != '' )
       num = num+1
       bit = substr(date,num,1)
endwhile
       loc = num-7
     month = substr(date,loc  ,3)
      year = substr(date,loc+3,4)
return month' 'year

function getlength (string)
tb = ""
i = 1
while (i<=80)
blank = substr(string,i,1)
if( blank = tb )
length = i-1
i = 81
else
i = i + 1
endif
endwhile
return length

