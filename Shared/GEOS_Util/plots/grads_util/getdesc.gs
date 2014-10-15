  function getdesc (args)
  name = subwrd(args,1)
  file = subwrd(args,2)

'lowercase 'name
            name = result

**************************************************
**************************************************
****                                          ****
****    This routine is used to "get" the     ****
****    description for a particular variable ****
****                                          ****
**************************************************
**************************************************

  'query file 'file
  numvar = sublin(result,6)
  numvar = subwrd(numvar,5)
  n = 1
  while ( n<=numvar )
  loc = n+6
     varline = sublin(result ,loc)
     varname = subwrd(varline,1)
     if( varname = name )
         vardesc = subwrd(varline,4)
             loc = 5
            next = subwrd(varline,loc)
          while( next != '' )
               vardesc = vardesc' 'next
                   loc = loc + 1
                  next = subwrd(varline,loc)
          endwhile
         return vardesc
     else
         n = n + 1
     endif
   endwhile
   return qname'_not_found'
