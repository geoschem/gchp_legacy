function zonal (args)

* Note: To produce residual data for new verifications, simply continue
*       after quit command to read VERIFICATION.rc prog files.
* ---------------------------------------------------------------------

'run getvar V DYN'
        vname  = subwrd(result,1)
        vfile  = subwrd(result,2)
'run getvar T DYN'
        tname  = subwrd(result,1)
        tfile  = subwrd(result,2)


'run getenv "GEOSUTIL"'
         geosutil = result

'run getenv "VERIFICATION"'
         verification = result

'run getenv "ARCH"'
         arch = result


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
'sett'

'getinfo xdim'
         xdim = result
'getinfo ydim'
         ydim = result
'getinfo zdim'
         zdim = result
'getinfo tdim'
         tdim = result

'run setenv "LAT0" 'lat0
'run setenv "XDIM" 'xdim
'run setenv "YDIM" 'ydim
'run setenv "ZDIM" 'zdim
'run setenv "TDIM" 'tdim

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
'!'geosutil'/plots/zonal_'arch'.x'
'quit'
return

'!remove LAT0.txt'
'!remove XDIM.txt'
'!remove YDIM.txt'
'!remove ZDIM.txt'
'!remove TDIM.txt'

* Loop over Possible Verification Datasets
* ----------------------------------------
'getnumrc 'geosutil'/plots/res'
     rcinfo = result
     numrc  = subwrd( rcinfo,1 )
       num  = 1
       cnt  = 0
while( num <= numrc )
        loc = num + 1
     rcfile = subwrd( rcinfo,loc )

'run getobs V DYN 'rcfile
        vname  = subwrd(result,1)
        vfile  = subwrd(result,2)
        vscale = subwrd(result,3)
        obsdsc = subwrd(result,4)
        obsid  = subwrd(result,5)
'run getobs T DYN 'rcfile
        tname  = subwrd(result,1)
        tfile  = subwrd(result,2)

'set dfile 'vfile
'set y 1'
'getinfo lat'
         lat0 = result
'set t 1'
'getinfo   date'
        begdate = result

'setx'
'sety'
'setz'
'sett'

    'getinfo   xdim'
               xdim = result
    'getinfo   ydim'
               ydim = result
    'getinfo   zdim'
               zdim = result
    'getinfo   tdim'
               tdim = result

'run setenv    "LAT0_rc'num'" 'lat0
'run setenv    "XDIM_rc'num'" 'xdim
'run setenv    "YDIM_rc'num'" 'ydim
'run setenv    "ZDIM_rc'num'" 'zdim
'run setenv    "TDIM_rc'num'" 'tdim
'run setenv "BEGDATE_rc'num'" 'begdate

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
'set t '1

'define pl = lev'
'define pk = pow(pl,2/7)'

'set gxout fwrite'
'set fwrite grads_rc'num'.fwrite'

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
'!'geosutil'/plots/zonal_'arch'.x -tag rc'num

num = num + 1
endwhile

'quit'
return
