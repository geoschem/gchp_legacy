function titles (args)

'numargs  'args
 numargs = result
 num     = subwrd(args,numargs)

*******************************************************************
*****                                                         *****
*****  Usage:  get_titles 'title1' 'title2' ... 'titleN' NUM  *****
*****                                                         *****
*****  Extracts a title (defined between quotes) from         *****
*****  an argument list.  NUM is the desired title you        *****
*****  want.  ARGS can contain any arbritray number of        *****
*****  titles and non-title information.                      *****
*****                                                         *****
*******************************************************************

      max = 120
      loc = 1
        m = 0

* Position loc to be at desired title
* -----------------------------------
while ( m != num )
  title = subwrd(args,loc)
  first = substr(title,1,1)
  if( first = "'" ) ; m=m+1 ; endif
  loc = loc + 1
endwhile

* Construct title information
* ---------------------------
end = 0
while (end=0 & loc<max )
tnxt = subwrd(args,loc)
title = title % ' ' % tnxt
              n = 1
      while ( n<max )
      last = substr(tnxt,n,1)
      if( last="'" | last="" )
              n = max
      else
              n = n+1
      endif
      if( last="'" & m=num ) ; end = 1 ; endif
      endwhile
loc = loc + 1
endwhile

* Extract title between quotes
* ----------------------------
               n = 2
       while ( n<max )
       last = substr(title,n,1)
       if( last = "'" )
           len = n-2 
             n = max
       else
             n = n+1
       endif
       endwhile
       title = substr( title,2,len )
return title
