function zonal (args)

* Note: To produce residual data for new verifications, simply continue
*       after quit command to read VERIFICATION.rc prog files.
* ---------------------------------------------------------------------

'run getvar V DYN'
        vname  = subwrd(result,1)
        vfile  = subwrd(result,2)
        vscale = subwrd(result,3)
        expdsc = subwrd(result,4)
        expid  = subwrd(result,5)
'run getvar T DYN'
        tname  = subwrd(result,1)
        tfile  = subwrd(result,2)

say ' EXPID: 'expid
say 'EXPDSC: 'expdsc

'run getenv "GEOSUTIL"'
         geosutil = result

'run getenv "VERIFICATION"'
         verification = result

'run getenv "ARCH"'
         arch = result

'run getpwd'
        pwd = result

'!/bin/cp 'geosutil'/plots/res/VERIFICATION*rc .'

* Initialize Environment using V-Wind File
* ----------------------------------------
'set dfile 'vfile
'setdates'
'set y 1'
'getinfo lat'
         lat0 = result
'setx'
'sety'
'setz'

* Create Environment Variables for Seasonal Utility
* -------------------------------------------------
'setdates'
'run getenv "BEGDATE"'
             begdate  = result
'run getenv "ENDDATE"'
             enddate  = result
'sett'

'getinfo xdim'
         xdim = result
'getinfo ydim'
         ydim = result
'getinfo zdim'
         zdim = result
'getinfo tdim'
         tdim = result

'run setenv    "LAT0.'expid'" 'lat0
'run setenv    "XDIM.'expid'" 'xdim
'run setenv    "YDIM.'expid'" 'ydim
'run setenv    "ZDIM.'expid'" 'zdim
'run setenv    "TDIM.'expid'" 'tdim
'run setenv "BEGDATE.'expid'" 'begdate

'makezf lev-lev zeros z'

* Create Zonal Means
* ------------------

if(  vfile != "NULL" ) ; 'set dfile 'vfile ; else ; 'chckname 'vname ; endif
'makezf 'vname' 'vname' z' 

if(  tfile != "NULL" ) ; 'set dfile 'tfile ; else ; 'chckname 'tname ; endif
'makezf 'tname' 'tname' z' 


* Assume VSTS Quadratic is in TFILE
* ---------------------------------
if(  tfile != "NULL" ) 
    'set dfile ' tfile 
    'chckname  vsts'
    'makezf vsts vsts z' 
else
    'chckname  vsts'
    'makezf vsts vsts z' 
endif


* Define Pressure Variables
* -------------------------
'set dfile 'vfile
'set x 1'
'sety'
'setz'
'set t '1

'define pl = lev'
'define pk = pow(pl,2/7)'

'set gxout fwrite'
'set fwrite grads.'expid'.fwrite'

* Write data
* ----------
t=1
while(t<=tdim)
  'set t 't
   say 'Writing Data T = 't'  zdim = 'zdim

   z=1
   while(z<=zdim)
  'set z 'z
   if(  vfile != "NULL" ) ; 'd ' vname'z' ; else ; 'd zerosz' ; endif
   z=z+1
   endwhile

   z=1
   while(z<=zdim)
  'set z 'z
   if(  tfile != "NULL" ) ; 'd ' tname'z' ; else ; 'd zerosz' ; endif
   z=z+1
   endwhile

   z=1
   while(z<=zdim)
  'set z 'z
   if(  tfile != "NULL" ) ; 'd     vstsz' ; else ; 'd zerosz' ; endif
   z=z+1
   endwhile

   z=1
   while(z<=zdim)
  'set z 'z
  'd pl'
   z=z+1
   endwhile

   z=1
   while(z<=zdim)
  'set z 'z
  'd pk'
   z=z+1
   endwhile

t=t+1
endwhile
'disable fwrite'

* Run Fortran Code to Produce StreamFunction and Residual Circulation
* -------------------------------------------------------------------
say ' 'geosutil'/plots/zonal_'arch'.x -tag 'expid' -desc 'descm
'!    'geosutil'/plots/zonal_'arch'.x -tag 'expid' -desc 'descm

'!remove sedfile'
'!touch  sedfile'

'!echo "s?@EXPID?"'expid'?g >> sedfile'
'!echo "s?@EXPDSC?"'expdsc'?g >> sedfile'
'!echo "s?@pwd?"'pwd'?g >> sedfile'
'!/bin/cp 'geosutil'/plots/res/VERIFICATION.rc.tmpl .'
'!sed -f   sedfile VERIFICATION.rc.tmpl > VERIFICATION.'expid'.rc'
'!remove VERIFICATION.rc.tmpl'

*'!cat sedfile'
*'quit'
*return

*'!remove LAT0.txt'
*'!remove XDIM.txt'
*'!remove YDIM.txt'
*'!remove ZDIM.txt'
*'!remove TDIM.txt'

* Loop over Possible Experiment Datasets for Comparison
* -----------------------------------------------------
'!/bin/mv HISTORY.T HISTORY.Tmp'
'run getenv "CMPEXP"'
         cmpexp = result
            num = 1

          dummy = get_cmpexp (cmpexp,num)
            exp = subwrd(dummy,1)
           type = subwrd(dummy,2)

while( exp != 'NULL' )
say ' '
say 'Comparing  with: 'exp
say 'Comparison type: 'type

'!chckfile 'exp'/.HOMDIR'
 'run getenv CHECKFILE'
             CHECKFILE  = result
         if( CHECKFILE != 'NULL' )
            '!/bin/cp `cat 'exp'/.HOMDIR`/HISTORY.rc .'
         else
            '!/bin/cp 'exp'/HISTORY.rc .'
         endif

'!remove CHECKFILE.txt'
'!cat HISTORY.rc | sed -e "s/,/ , /g" | sed -e "s/*/@/g" > HISTORY.T'

'run getvar V DYN 'exp
say 'GETVAR output: 'result
        vname  = subwrd(result,1)
        vfile  = subwrd(result,2)
        vscale = subwrd(result,3)
        obsdsc = subwrd(result,4)
        obsid  = subwrd(result,5)
'run getvar T DYN 'exp
        tname  = subwrd(result,1)
        tfile  = subwrd(result,2)

say 'Comparison   ID: 'obsid
say 'Comparison Desc: 'obsdsc

* VERIFICATION.obsid.rc CHECKFILE Test
* ------------------------------------
'!chckfile VERIFICATION.'obsid'.rc'
 'run getenv CHECKFILE'
             CHECKFILE = result
         if( CHECKFILE = 'NULL' )
 
'set dfile 'vfile
'set y 1'
'getinfo lat'
         lat0 = result
'setx'
'sety'
'setz'

'getdates'
 begdateo = subwrd(result,1)
 enddateo = subwrd(result,2)

'getinfo tmin'
         tmin = result
'getinfo tmax'
         tmax = result

'set t 'tmin
'getinfo   date'
        begdate = result

'run setenv   "BEGDATEO" 'begdateo
'run setenv   "ENDDATEO" 'enddateo
'set t 'tmin' 'tmax
        tdim = tmax-tmin+1

    'getinfo   xdim'
               xdim = result
    'getinfo   ydim'
               ydim = result
    'getinfo   zdim'
               zdim = result

'run setenv    "LAT0.'obsid'" 'lat0
'run setenv    "XDIM.'obsid'" 'xdim
'run setenv    "YDIM.'obsid'" 'ydim
'run setenv    "ZDIM.'obsid'" 'zdim
'run setenv    "TDIM.'obsid'" 'tdim
'run setenv "BEGDATE.'obsid'" 'begdate

'makezf lev-lev zeros z'

* Create Zonal Means
* ------------------

if(  vfile != "NULL" ) ; 'set dfile 'vfile ; else ; 'chckname 'vname ; endif
'makezf 'vname' 'vname' z'

if(  tfile != "NULL" ) ; 'set dfile 'tfile ; else ; 'chckname 'tname ; endif
'makezf 'tname' 'tname' z'


* Assume VSTS Quadratic is in TFILE
* ---------------------------------
if(  tfile != "NULL" ) 
    'set dfile ' tfile 
    'chckname vsts'
    'makezf   vsts vsts z'
else
    'chckname vsts'
    'makezf   vsts vsts z'
endif


* Define Pressure Variables
* -------------------------
'set dfile 'vfile
'set x 1'
'sety'
'setz'
'set t 'tmin

'define pl = lev'
'define pk = pow(pl,2/7)'

'set gxout fwrite'
'set fwrite grads.'obsid'.fwrite'

* Write data
* ----------
t=tmin
while(t<=tmax)
  'set t 't
   say 'Writing Data T = 't'  zdim = 'zdim

   z=1
   while(z<=zdim)
  'set z 'z
   if(  vfile != "NULL" ) ; 'd ' vname'z' ; else ; 'd zerosz' ; endif
   z=z+1
   endwhile

   z=1
   while(z<=zdim)
  'set z 'z
   if(  tfile != "NULL" ) ; 'd ' tname'z' ; else ; 'd zerosz' ; endif
   z=z+1
   endwhile

   z=1
   while(z<=zdim)
  'set z 'z
   if(  tfile != "NULL" ) ; 'd     vstsz' ; else ; 'd zerosz' ; endif
   z=z+1
   endwhile

   z=1
   while(z<=zdim)
  'set z 'z
  'd pl'
   z=z+1
   endwhile

   z=1
   while(z<=zdim)
  'set z 'z
  'd pk'
   z=z+1
   endwhile

t=t+1
endwhile
'disable fwrite'

* Run Fortran Code to Produce StreamFunction and Residual Circulation
* -------------------------------------------------------------------
say ' 'geosutil'/plots/zonal_'arch'.x -tag 'obsid' -desc 'desco
'!    'geosutil'/plots/zonal_'arch'.x -tag 'obsid' -desc 'desco

'!remove sedfile'
'!touch  sedfile'

'!echo "s?@EXPID?"'obsid'?g >> sedfile'
'!echo "s?@EXPDSC?"'obsdsc'?g >> sedfile'
'!echo "s?@pwd?"'pwd'?g >> sedfile'
'!/bin/cp 'geosutil'/plots/res/VERIFICATION.rc.tmpl .'
'!sed -f   sedfile VERIFICATION.rc.tmpl > VERIFICATION.'obsid'.rc'
'!remove VERIFICATION.rc.tmpl'
'!cat sedfile'

* End VERIFICATION.obsid.rc CHECKFILE Test
* ----------------------------------------
endif
'!remove CHECKFILE.txt'

* Check next Comparison Experiment Dataset
* ----------------------------------------
    num = num + 1
  dummy = get_cmpexp (cmpexp,num)
    exp = subwrd(dummy,1)
   type = subwrd(dummy,2)
endwhile

'!/bin/mv HISTORY.Tmp HISTORY.T'

'quit'
return

* Get Next EXP from CMPEXP List
* -----------------------------
function get_cmpexp (cmpexp,num)
      exp  = subwrd(cmpexp,num)
      len = get_length (exp)
      bit = substr(exp,len-1,1)
      if( bit = ":" )
          type = substr(exp,len,1)
          exp  = substr(exp,1,len-2)
      else
          type = M
      endif
return exp' 'type

function get_length (string)
tb = ""
i = 1
while (i<=256)
blank = substr(string,i,1)
if( blank = tb )
length = i-1
i = 999
else
i = i + 1
endif
endwhile
return length

