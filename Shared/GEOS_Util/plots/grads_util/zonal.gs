function zonal (args)

'run getvar U DYN'
        uname  = subwrd(result,1)
        ufile  = subwrd(result,2)
'run getvar V DYN'
        vname  = subwrd(result,1)
        vfile  = subwrd(result,2)
'run getvar T DYN'
        tname  = subwrd(result,1)
        tfile  = subwrd(result,2)
'run getvar Q MOIST'
        qname  = subwrd(result,1)
        qfile  = subwrd(result,2)
'run getvar RH2 MOIST'
        rhname  = subwrd(result,1)
        rhfile  = subwrd(result,2)


* Initialize Environment using U-Wind File
* ----------------------------------------
'set dfile 'ufile
'setdates'
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

'run setenv "XDIM" 'xdim
'run setenv "YDIM" 'ydim
'run setenv "ZDIM" 'zdim
'run setenv "TDIM" 'tdim

'define zeros = lev-lev'
'makez  zeros z'

* Create Zonal Means
* ------------------
if(  ufile != "NULL" ) 
    'set dfile ' ufile 
    'makez ' uname' z' 
else
    'chckname 'uname
    'makez    'uname' z' 
endif

if(  vfile != "NULL" ) 
    'set dfile ' vfile 
    'makez ' vname' z' 
else
    'chckname 'vname
    'makez    'vname' z' 
endif

if(  tfile != "NULL" ) 
    'set dfile ' tfile 
    'makez ' tname' z' 
else
    'chckname 'tname
    'makez    'tname' z' 
endif

if(  qfile != "NULL" ) 
    'set dfile ' qfile 
    'makez ' qname' z' 
else
    'chckname 'qname
    'makez    'qname' z' 
endif

if( rhfile != "NULL" ) 
    'set dfile 'rhfile 
    'makez 'rhname' z' 
else
    'chckname 'rhname
    'makez    'rhname' z' 
endif

* Assume vsts Quadratic is in TFILE
* ---------------------------------
if(  tfile != "NULL" ) 
    'set dfile ' tfile 
    'chckname vsts'
    'makez    vsts z'
else
    'chckname vsts'
    'makez    vsts z'
endif


* Define Pressure Variables
* -------------------------
'set dfile 'ufile
'set x 1'
'sety'
'setz'
'define pl = lev'
'define pk = pow(pl,2/7)'

'set gxout fwrite'

'getinfo tmax'
         tmax = result
'getinfo zmax'
         zmax = result

* Write data
* ----------
t=1
while(t<=tmax)
  'set t 't
   say 'Writing Data T = 't

   z=1
   while(z<=zmax)
  'set z 'z

if(  ufile != "NULL" ) ; 'd ' uname'z' ; else ; 'd zerosz' ; endif
if(  vfile != "NULL" ) ; 'd ' vname'z' ; else ; 'd zerosz' ; endif
if(  tfile != "NULL" ) ; 'd ' tname'z' ; else ; 'd zerosz' ; endif
if(  qfile != "NULL" ) ; 'd ' qname'z' ; else ; 'd zerosz' ; endif
if( rhfile != "NULL" ) ; 'd 'rhname'z' ; else ; 'd zerosz' ; endif
if(  tfile != "NULL" ) ; 'd     vstsz' ; else ; 'd zerosz' ; endif

  'd        pl'
  'd        pk'

   z=z+1
   endwhile

t=t+1
endwhile

'quit'
return
