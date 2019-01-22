function genpltz (args)

'numargs  'args
 numargs = result

USE_PLOTRC = FALSE

dqmax = NULL
dqmin = NULL

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
if( subwrd(args,num) = '-MAX'     ) ; dqmax    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-MIN'     ) ; dqmin    = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-ZLOG'    ) ; zlog     = subwrd(args,num+1) ; endif

endwhile

'run getlevs 'alias
     numlevs = result

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
say '-MAX    'dqmax
say '-MIN    'dqmin
say '-PTOP   'ptop
say '-ZLOG   'zlog

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

if( zlog = 'ON' & ptop < 10 )
                        'getresource 'PLOTRC' 'EXPORT'_'GC'_ZLOG_CLEVS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'EXPORT'_'GC'_Z_CLEVS' ; endif
else
                        'getresource 'PLOTRC' 'EXPORT'_'GC'_Z_CLEVS'
endif
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'EXPORT'_'GC'_CLEVS' ; endif
                                                            clevs = result

                        'getresource 'PLOTRC' 'EXPORT'_'GC'_Z_'LEVTYPE
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'EXPORT'_'GC'_'LEVTYPE ; endif
                                                             dlevs = result

                        'getresource 'PLOTRC' 'EXPORT'_'GC'_DIFFMAX'
                                                            diffmax = result
                        'getresource 'PLOTRC' 'EXPORT'_'GC'_DIFFMIN'
                                                            diffmin = result

if( fact    = 'NULL' ) ; fact    = 1            ; endif
if( title   = 'NULL' )
  'getdesc 'alias
     title  = alias': 'result
endif

'run getenv "CINTDIFF"'
         CINTDIFF = result

say ''
say 'TITLE: 'title
say 'LEVTPYE: 'LEVTYPE
say ' FACT: 'fact
say 'CLEVS: 'clevs
say 'CCOLS: 'ccols
say 'DLEVS: 'dlevs
say 'DCOLS: 'dcols
say 'DIFFMAX: 'diffmax
say 'DIFFMIN: 'diffmin

if( zlog = 'OFF' )
    oname = '/hdiag_'obsnam'_'EXPORT'.'GC'_z'
else
    oname = '/hdiag_'obsnam'_'EXPORT'.'GC'_zlog'ptop
endif

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


* Find QMIN and QMAX for MODEL Field
* ----------------------------------
'set dfile 'qfile
'set lat -90 90'
'set lon 0'
if( numlevs = 1 )
   'set z 1'
else
   'set lev 1000 'ptop
endif
'set t 1'

  gg = xyz (xmin,xmax,ymin,ymax,zmin,zmax)
xmin = subwrd(gg,1)
xmax = subwrd(gg,2)
ymin = subwrd(gg,3)
ymax = subwrd(gg,4)
zmin = subwrd(gg,5)
zmax = subwrd(gg,6)

say ' XMIN: 'xmin'   XMAX: 'xmax
say ' YMIN: 'ymin'   YMAX: 'ymax
say ' ZMIN: 'zmin'   ZMAX: 'zmax

'set x 'xmin' 'xmax
'set y 'ymin' 'ymax
'set z 'zmin' 'zmax

* Determine QMIN and QMAX for Model Field and Corresponding Levels
* ----------------------------------------------------------------
     z = zmin
'set z 'z
'minmax.simple modz*'fact
   qmax = subwrd(result,1)
   qmin = subwrd(result,2)
  qzmax = zmin
  qzmin = zmin
       z = z + 1
 while( z <= zmax )
'set z 'z
'minmax.simple modz*'fact
 dmax = subwrd(result,1)
 dmin = subwrd(result,2)
 if( dmax > qmax ) ; qmax = dmax ; qzmax = z ; endif
 if( dmin < qmin ) ; qmin = dmin ; qzmin = z ; endif
 z = z + 1
 endwhile
* ------------------------------------------------

say ' QMIN: 'qmin'   QMAX: 'qmax
say 'QZMIN: 'qzmin'  QZMAX: 'qzmax

'd abs('qmin')'
        qmin = subwrd(result,4)
'd abs('qmax')'
        qmax = subwrd(result,4)

if( qmin > qmax )
    qmax = qmin
   'd abs('qzmin')'
           qzmin = subwrd(result,4)
   'd abs('qzmax')'
           qzmax = subwrd(result,4)
    if( qzmin > qzmax ) ; qzmax = qzmin ; endif
endif


* Determin if ZLOG is appropriate
* -------------------------------
if( zlog = NULL )
   'set z 'zmin' 'zmax
    if( zmin < zmax ) then
       'set lev 100'
       'getinfo zpos'
                zpos = result
       'set z 'zmin' 'zmax
       if( qzmax > zpos )
           zlog = ON
       else
           zlog = OFF
       endif
    endif
say 'Setting ZLOG = 'zlog
endif

* -------------------------------


* Make Mean Plot
* --------------
'set vpage off'
'set parea off'
'set grid  off'
'set frame on'
'set xlopts 1 3 .11'
'set ylopts 1 3 .11'
'rgbset'

************************************************************
*                         Top Plot
************************************************************

'set dfile 'qfile
'set lat -90 90'
'set lon 0'
'set t 1'

'set vpage 0 8.5 0.0 11'
'set grads off'
'set gxout shaded'
if( numlevs = 1 )
   'set parea 1.5 7.0 4.30 10.50'
   'set z 1'
   'set zlog off'
   'set ccolor 4'
else
   'set parea 1.5 7.0 7.70 10.50'
   'set lev 1000 'ptop
   'set zlog 'zlog
    if( zlog = ON ) ; 'setlevs' ; endif
endif
say ' '
say 'Top Plot:'
say '---------'

       qn = 0
       qm = 0
if( ccols = NULL )
* ----------------

   'd abs('qmin')'
           qmin = subwrd(result,4)
   'd abs('qmax')'
           qmax = subwrd(result,4)
   if( qmin > qmax ) ; qmax = qmin ; endif
   if( qmax > 0 )
      'd log10('qmax')'
       qn = subwrd(result,4)
   else
       qn = 0
   endif
   say '    Log Factor: 'qn
   if( qn<0 ) ; qn = qn-2 ; endif
   'getint 'qn
            qn = result
   if( qn>0 )
       if( qn<=2 )
           qn = 0
        else
           qn = qn+2
        endif
   endif
   if( qn<0 )
       qm = -qn
   else
       qm =  qn
   endif

   say 'Scaling Factor: 'qn
     if( qn>0 )
       'shades modz*'fact'/1e'qm' 0'
       'd      modz*'fact'/1e'qm
     else
       'shades modz*'fact'*1e'qm' 0'
       'd      modz*'fact'*1e'qm
     endif

* ----------------
else
* ----------------

   'set clevs 'clevs
   'set ccols 'ccols
   'd modz*'fact

endif
* ----------------

'set_clevs'

if( numlevs > 1 )
if( ccols = NULL )
    if( qn>0 )
       'd modz*'fact'/1e'qm
    else
       'd modz*'fact'*1e'qm
    endif
else
   'd modz*'fact
endif
'draw ylab Pressure (mb)'
endif


************************************************************
*                        Middle Plot
************************************************************

'set dfile 'ofile
'set lat -90 90'
'set lon 0'
'set t 1'
'set gxout shaded'
if( numlevs = 1 )
   'set z 1'
   'set zlog off'
   'set ccolor 1'
else
   'set parea 0 8.5 7.0 11'
   'cbarn -vert'
   'set parea off'
   'set vpage 0 8.5 0.0 11'
   'set parea 1.5 7.0 4.30 7.10'
   'set lev 1000 'ptop
   'set zlog 'zlog
    if( zlog = ON ) ; 'setlevs' ; endif
endif
say ' '
say 'Middle Plot:'
say '------------'
'set grads off'

if( ccols = NULL )
    if( qn>0 )
       'shades modz*'fact'/1e'qm' 0'
       'd      obsz*'fact'/1e'qm
    else
       'shades modz*'fact'*1e'qm' 0'
       'd      obsz*'fact'*1e'qm
    endif
else
   'set clevs 'clevs
   'set ccols 'ccols
   'd obsz*'fact
endif

'set_clevs'

if( numlevs > 1 )
if( ccols = NULL )
    if( qn>0 )
       'd obsz*'fact'/1e'qm
    else
       'd obsz*'fact'*1e'qm
    endif
else
   'd obsz*'fact
endif
'draw ylab Pressure (mb)'
endif

'set parea off'

************************************************************
*                        Bottom Plot
************************************************************

'set dfile 'qfile
'set lat -90 90'
'set lon 0'
'set t 1'
'set vpage 0 8.5 0.0 11'
'set parea 1.5 7.0 0.90 3.70'
'set grads off'
'set gxout shaded'
if( numlevs = 1 )
   'set z 1'
   'set zlog off'
else
   'set lev 1000 'ptop
   'set zlog 'zlog
    if( zlog = ON ) ; 'setlevs' ; endif
endif
say ' '
say 'Bottom Plot:'
say '------------'

       dn = 0
       dm = 0

if( dcols = NULL | CINTDIFF != NULL | USE_PLOTRC = TRUE )
* -------------------------------------------------------

  if( diffmax = NULL ) 
   'd 'dqmax'*'fact
       dqmax = subwrd(result,4)
  else
       dqmax = diffmax
  endif
  if( diffmin = NULL ) 
   'd 'dqmin'*'fact
       dqmin = subwrd(result,4)
  else
       dqmin = diffmin
  endif

    say 'DQMAX * FACT: 'dqmax
    say 'DQMIN * FACT: 'dqmin

   'd abs('dqmin')'
           dqmin = subwrd(result,4)
   'd abs('dqmax')'
           dqmax = subwrd(result,4)
   if( dqmin > dqmax ) ; dqmax = dqmin ; endif
   if( dqmax > 0 )
      'd log10('dqmax')'
       dn = subwrd(result,4)
   else
       dn = 0
   endif
   say '    Log Factor: 'dn
   if( dn<0 ) ; dn = dn-2 ; endif
   'getint 'dn
            dn = result
   if( dn>0 )
       if( dn<=2 )
           dn = 0
        else
           dn = dn+2
        endif
   endif
   if( dn<0 )
       dm = -dn
   else
       dm =  dn
   endif

   say 'Scaling Factor: 'dn

     if( dn>0 )
       'd 0.1*'dqmax'/1e'dm
        cint = subwrd(result,4)
        say 'dn> 0,  CINT: 'cint
       'shades 'cint
       'd qz*'fact'/1e'dm
     else
       'd 0.1*'dqmax'*1e'dm
        cint = subwrd(result,4)
        say 'dn< 0,  CINT: 'cint
       'shades 'cint
       'd qz*'fact'*1e'dm
     endif

* ----------------
else
* ----------------

      'set clevs 'dlevs
      'set ccols 'dcols
      'd qz*'fact

endif
* -------------------------------------------

'cbarn -snum 0.55 -xmid 4.25 -ymid 0.4'

if( numlevs > 1 )
'draw ylab Pressure (mb)'
endif
'set gxout contour'
'set ccolor 1'
if( dcols = NULL | CINTDIFF != NULL | USE_PLOTRC = TRUE )
   'set clevs -'cint' 'cint
    if( dn>0 )
      'd qz*'fact'/1e'dm
    else
      'd qz*'fact'*1e'dm
    endif
else
   'set clevs 'dlevs
   'd qz*'fact
endif

************************************************************
************************************************************

'set vpage off'
'set string 1 l 4'
'set strsiz 0.065'
'draw string 0.05 0.08 ( EXPID:  'expid' )'

'set string 1 c 6'
'set strsiz .13'
'draw string 4.25 10.85 'title
'set strsiz .11'

if( numlevs > 1 )
'set string 1 c 6'
if( qn != 0 )
'draw string 4.25 10.64 'qdesc' 'season' ('nmod') (x 10**'qn')'
else
'draw string 4.25 10.64 'qdesc' 'season' ('nmod')'
endif
'draw string 4.25  7.24 'odesc' 'season' ('nobs') ('climate')'

else

'set string 4 c 6'
if( qn != 0 )
'draw string 4.25 10.64 'qdesc' 'season' ('nmod') (x 10**'qn')'
else
'draw string 4.25 10.64 'qdesc' 'season' ('nmod')'
endif
'set string 1 c 6'
'draw string 4.25 7.24 'odesc' 'season' ('nobs') ('climate')'
endif

if( dn != 0 )
'draw string 4.25  3.80 Difference (Top-Middle) (x 10**'dn')'
else
'draw string 4.25  3.80 Difference (Top-Middle)'
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

if( numlevs > 1 )
'set string 1 l 4'
'set strsiz .08'
'draw string 0.10 10.37 Beg: 'bmnthm' 'byearm
'draw string 0.10 10.24 End: 'emnthm' 'eyearm
'draw string 0.10  6.97 Beg: 'bmntho' 'byearo
'draw string 0.10  6.84 End: 'emntho' 'eyearo
else
'set string 4 l 4'
'set strsiz .08'
'draw string 0.10 10.37 Beg: 'bmnthm' 'byearm
'draw string 0.10 10.24 End: 'emnthm' 'eyearm
'set string 1 r 4'
'draw string 8.30 10.37 Beg: 'bmntho' 'byearo
'draw string 8.30 10.24 End: 'emntho' 'eyearo
endif

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

function xyz (xmin,xmax,ymin,ymax,zmin,zmax)
'getinfo xfreq'
         xfreq = result
     if( xfreq = 'varying' )
         'getinfo xmin'
                  xmin = result
         'getinfo xmax'
                  xmax = result
     endif
     if( xfreq = 'fixed' )
         'getinfo xpos'
                  xmin = result
                  xmax = result
     endif

'getinfo yfreq'
         yfreq = result
     if( yfreq = 'varying' )
         'getinfo ymin'
                  ymin = result
         'getinfo ymax'
                  ymax = result
     endif
     if( yfreq = 'fixed' )
         'getinfo ypos'
                  ymin = result
                  ymax = result
     endif

'getinfo zfreq'
         zfreq = result
     if( zfreq = 'varying' )
         'getinfo zmin'
                  zmin = result
         'getinfo zmax'
                  zmax = result
     endif
     if( zfreq = 'fixed' )
         'getinfo zpos'
                  zmin = result
                  zmax = result
     endif

return xmin' 'xmax' 'ymin' 'ymax' 'zmin' 'zmax

