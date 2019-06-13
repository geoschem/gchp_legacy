function closeness (args)

* Initialize INPUT Parameters
* ---------------------------
    cvar = getarg (args,CVAR)
    mvar = getarg (args,MVAR)
    ovar = getarg (args,OVAR)

   cname = getarg (args,CNAME)
   mname = getarg (args,MNAME)
   oname = getarg (args,ONAME)

   cdesc = getarg (args,CDESC)
   mdesc = getarg (args,MDESC)
   odesc = getarg (args,ODESC)

   mfile = getarg (args,MFILE)
   bdate = getarg (args,MBEGDATE)
   edate = getarg (args,MENDDATE)

   ofile = getarg (args,OFILE)
  bdateo = getarg (args,OBEGDATE)
  edateo = getarg (args,OENDDATE)

   expid = getarg (args,EXPID)
gridcomp = getarg (args,GC)
  prefix = getarg (args,PREFIX)
  season = getarg (args,SEASON)
  output = getarg (args,OUTPUT)
 climate = getarg (args,CLIMATE)
    math = getarg (args,MATH)
   level = getarg (args,LEVEL)

* ---------------------------
if(     math = NULL ) ;     math = '' ; endif
if(   season = NULL ) ;   season = '' ; endif
if( gridcomp = NULL ) ; gridcomp = '' ; endif

'set t 1'
'run getenv "GEOSUTIL"'
             geosutil = result

'run getenv "LEVTYPE"'
             LEVTYPE = result
say 'GETENV LEVTYPE = 'LEVTYPE
         if( LEVTYPE = 'NULL' ) ; LEVTYPE = DLEVS ; endif

if( prefix != NULL )
    PFX = prefix'_'
else
    PFX = ''
endif
say ''

title = 'NULL'
clevs = 'NULL'
ccols = 'NULL'
dlevs = 'NULL'
dcols = 'NULL'
ccint = 'NULL'

* Check for Existance of NAME Specific plot.rc
* --------------------------------------------
PLOTRC = geosutil'/plots/'mname'/plot.rc'

'!remove   grads.txt'
'!listfile 'PLOTRC' > grads.txt'
checkrc = sublin ( read(grads.txt),2 )
             rc = close(grads.txt)

* Check Variable Attributes from NAME Specific PLOTRC
* ---------------------------------------------------
if( checkrc = PLOTRC )
                        'getresource 'PLOTRC' 'PFX'TITLE'
if( result = 'NULL' ) ; 'getresource 'PLOTRC'      TITLE' ; endif
                                                   title  = result

                        'getresource 'PLOTRC' 'PFX'CINT'
if( result = 'NULL' ) ; 'getresource 'PLOTRC'      CINT' ; endif
                                                   ccint  = result

                        'getresource 'PLOTRC' 'PFX'CLEVS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC'      CLEVS' ; endif
                                                   clevs  = result

                        'getresource 'PLOTRC' 'PFX'CCOLS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC'      CCOLS' ; endif
                                                   ccols  = result

                        'getresource 'PLOTRC' 'PFX''LEVTYPE
if( result = 'NULL' ) ; 'getresource 'PLOTRC'      'LEVTYPE ; endif
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'PFX'DLEVS'   ; endif
if( result = 'NULL' ) ; 'getresource 'PLOTRC'      DLEVS'   ; endif
                                                   dlevs  = result

                        'getresource 'PLOTRC' 'PFX'DCOLS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC'      DCOLS' ; endif
                                                   dcols  = result
factor = 1

else

* Check Variable Attributes from Generic PLOTRC
* ---------------------------------------------
PLOTRC = geosutil'/plots/grads_util/plot.rc'

                        'getresource 'PLOTRC' 'mname'_'gridcomp'_'level'_CBSCALE'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'mname'_'gridcomp'_CBSCALE' ; endif
                                                                cbscale = result

                        'getresource 'PLOTRC' 'mname'_'gridcomp'_'level'_FACTOR'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'mname'_'gridcomp'_FACTOR' ; endif
                                                                factor = result

                        'getresource 'PLOTRC' 'mname'_'gridcomp'_'level'_TITLE'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'mname'_'gridcomp'_TITLE' ; endif
                                                                title = result

                        'getresource 'PLOTRC' 'mname'_'gridcomp'_'level'_CINT'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'mname'_'gridcomp'_CINT' ; endif
                                                                ccint = result

                        'getresource 'PLOTRC' 'mname'_'gridcomp'_'level'_CCOLS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'mname'_'gridcomp'_CCOLS' ; endif
                                                                ccols = result

                        'getresource 'PLOTRC' 'mname'_'gridcomp'_'level'_CLEVS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'mname'_'gridcomp'_CLEVS' ; endif
                                                                clevs = result

                        'getresource 'PLOTRC' 'mname'_'gridcomp'_REGRID'
                                                                method = result

                        'getresource 'PLOTRC' 'mname'_'gridcomp'_'level'_DCOLS'
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'mname'_'gridcomp'_DCOLS' ; endif
                                                                 dcols = result

                    say 'getresource 'PLOTRC' 'mname'_'gridcomp'_'level'_'LEVTYPE
                        'getresource 'PLOTRC' 'mname'_'gridcomp'_'level'_'LEVTYPE
                    say 'RESULT = 'result
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'mname'_'gridcomp'_'level' 'DLEVS ; endif
if( result = 'NULL' ) ; 'getresource 'PLOTRC' 'mname'_'gridcomp'_'DLEVS         ; endif
                                                                  dlevs = result
endif

'run getenv "CINTDIFF"'
             CINTDIFF  = result
         if( CINTDIFF != 'NULL' ) ; dcols = 'NULL' ; endif

say ''
if( factor = 'NULL' ) ; factor = 1 ; endif
if( title  = 'NULL' )
   'getdesc 'mname
             desc = result
    title = mname':'gridcomp'  'desc
   "rmstring '"title"' '[column]'"
    title = result
   "rmstring '"title"' '__ENSEMBLE__'"
    title = result
endif

if( dcols = 'NULL' )
    dcols = '55  49  47  45  44  36  34  33  32  0  21  22  23  24  25  26  27  28 69'
endif

* Perform Mathematics if necessary
* --------------------------------
'define qmod = 'mvar''season'*'factor
'define cmod = 'cvar''season'*'factor
'define qobs = 'ovar''season'*'factor

if( math = LOG )
   'define qmod = log(qmod+0.00001)'
   'define cmod = log(cmod+0.00001)'
   'define qobs = log(qobs+0.00001)'
endif

'set vpage off'
'set parea off'
'set grid  off'
'set mproj scaled'
'set frame on'
'set xlopts 1 3 .11'
'set ylopts 1 3 .11'

* Count Seasons
* -------------
'set dfile 'ofile
'count "'season'" 'bdateo' 'edateo
 nobs = result

'set dfile 'mfile
'count "'season'" 'bdate' 'edate
 nmod = result

'set t 1'

'set vpage 0 8.5 0.0 11'
'set grads off'
'rgbset'

'getinfo lon'
         lon = result
'define obsg = qobs'
'define modg = qmod'
'define codg = cmod'
'define difg = maskout( modg-obsg,abs(obsg) )'
'define cifg = maskout( codg-obsg,abs(obsg) )'

* Top Panel
* ---------
'set parea 1.5 7.0 7.70 10.50'
ntop = 0
    say 'TOP CLEVS: 'dlevs
    say 'TOP CCOLS: 'dcols
if( dcols != NULL & dlevs != NULL )
   'set clevs 'dlevs
   'set ccols 'dcols
   'd difg'
else
   'stats difg'
     avgdif = subwrd(result,1)
     stddif = subwrd(result,2)
     say 'AVGDIF: 'avgdif
     say 'STDDIF: 'stddif
       qmax = stddif/3
     say '  QMAX: 'qmax
   if( qmax > 0 )
      'd log10('qmax')'
       ntop = subwrd(result,4)
   else
       ntop = 0
   endif
   say '    Log Factor: 'ntop
   if( ntop<0 ) ; ntop = ntop-2 ; endif
   'getint 'ntop
            ntop = result
   if( ntop>0 )
       if( ntop<=2 )
           ntop = 0
        else
           ntop = ntop+2
        endif
   endif
   say 'Diff Scaling Factor: 'ntop
      if( ntop < 0 )
          ztop = -1 * ntop
         'd 'qmax'*1e'ztop
      else
      '   d 'qmax'/1e'ntop
      endif
       cint = subwrd(result,4)
       say 'TOP CINT: 'cint
      'shades 'cint
      if( ntop < 0 )
          ztop = -1 * ntop
         'define difg = difg*1e'ztop
      else
         'define difg = difg/1e'ntop
      endif
      'd difg'
endif
'set parea 0 8.5 7.0 11'
'cbarn -vert'

* Middle Panel
* ------------
'set parea 1.5 7.0 4.30 7.10'
nmid = 0
if( dcols != NULL & dlevs != NULL )
   'set clevs 'dlevs
   'set ccols 'dcols
   'd cifg'
else
       nmid = ntop
      'shades 'cint
      if( ntop < 0 )
          ztop = -1 * ntop
         'define cifg = cifg*1e'ztop
      else
         'define cifg = cifg/1e'ntop
      endif
      'd cifg'
endif

* Bottom Panel
* ------------
'set parea 1.5 7.0 0.90 3.70'
'define closeness = abs(difg)-abs(cifg)'

   'stats closeness'
     avgdif = subwrd(result,1)
     stddif = subwrd(result,2)
       qmax = stddif/3
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
   say 'Diff Scaling Factor: 'n
      if( n < 0 )
          ztop = -1 * n
         'd 'qmax'*1e'ztop
      else
         'd 'qmax'/1e'n
      endif
       cint = subwrd(result,4)
      'shades 'cint
      if( n < 0 )
          ztop = -1 * n
         'define closeness = closeness*1e'ztop
      else
         'define closeness = closeness/1e'n
      endif
      'd closeness'

'cbarn -snum 0.55 -xmid 4.25 -ymid 0.4'

'stats difg'
 avgmod = subwrd(result,1)
 stdmod = subwrd(result,2)
'stats cifg'
 avgobs = subwrd(result,1)
 stdobs = subwrd(result,2)
'stats closeness'
 avgdif = subwrd(result,1)
 stddif = subwrd(result,2)

'set vpage off'
'set string 1 l 4'
'set strsiz .065'
'draw string 0.05 0.08 ( EXPID:  'expid' )'

'set string 1 c 6'
'set strsiz .125'
if( level = '' )
   'draw string 4.25 10.85 'math' 'title
else
   'draw string 4.25 10.85 'level'-mb 'math' 'title
endif
'set strsiz .10'

if( ntop != 0 )
   if( ntop>0 )
      'draw string 4.25 10.62 'mdesc' - 'oname'  'season' ('nmod')  (x 10** -'ntop')'
   else
      'draw string 4.25 10.62 'mdesc' - 'oname'  'season' ('nmod')  (x 10**'ntop')'
   endif
else
   'draw string 4.25 10.62 'mdesc' - 'oname'  'season' ('nmod')'
endif

if( nmid != 0 )
   if( nmid>0 )
      'draw string 4.25 7.22 'cdesc' - 'oname'  'season' ('nobs')  (x 10** -'nmid')  ('climate')'
   else
      'draw string 4.25 7.22 'cdesc' - 'oname'  'season' ('nobs')  (x 10**'nmid')  ('climate')'
   endif
else
   'draw string 4.25 7.22 'cdesc' - 'oname'  'season' ('nobs')  ('climate')'
endif

if( n != 0 )
   'draw string 4.25 3.80 Closeness to 'oname':  ABS(Top)-ABS(Middle)  (x 10**'n')'
else
   'draw string 4.25 3.80 Closeness to 'oname':  ABS(Top)-ABS(Middle)'
endif

* Print Beginning and Ending Dates
* --------------------------------
                date = getdate (bdate)
bmnthm = subwrd(date,1)
byearm = subwrd(date,2)
                date = getdate (edate)
emnthm = subwrd(date,1)
eyearm = subwrd(date,2)
                date = getdate (bdateo)
bmntho = subwrd(date,1)
byearo = subwrd(date,2)
                date = getdate (edateo)
emntho = subwrd(date,1)
eyearo = subwrd(date,2)

* --------------------------------

'set string 1 l 4'
'set strsiz .08'
'draw string 0.050 10.50 Beg: 'bmnthm' 'byearm
'draw string 0.050 10.35 End: 'emnthm' 'eyearm
'draw string 0.050 7.10 Beg: 'bmntho' 'byearo
'draw string 0.050 6.95 End: 'emntho' 'eyearo

'draw string 0.050 9.85 Mean: 'avgmod
'draw string 0.050 9.70  Std: 'stdmod
'draw string 0.050 6.45 Mean: 'avgobs
'draw string 0.050 6.30  Std: 'stdobs
'draw string 0.050 3.05 Mean: 'avgdif
'draw string 0.050 2.90  Std: 'stddif

'set mproj latlon'
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

function getarg (args,name)
'numargs  'args
 numargs = result
        num = 0
while ( num < numargs )
        num = num + 1
if( subwrd(args,num) = '-'name ) 
    arg = subwrd(args,num+1)
    say name' = 'arg
    return arg
endif
endwhile
return

