function getfile (args)
fname = subwrd(args,1)
fnum  = NULL

*'getinfo numfiles'
*         numfiles = result
*if( numfiles!=NULL )
*'q files'
*   files  = result
*        n = 1
* while (n<=numfiles & fnum=NULL)
* dummy = sublin( files,3*n-1)
* dummy = subwrd(dummy,2)
* if( dummy=fname ) 
*     fnum = n
* endif
*        n = n+1
* endwhile
*endif

return fnum

