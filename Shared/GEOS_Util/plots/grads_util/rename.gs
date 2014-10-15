**************************************************************
*                                                            *
* This script renames an original Grads data variable.       *
* If the variable is missing, a zero fill is used.           *
*                                                            *
**************************************************************
 
  function rename (args) 
  oldname = subwrd(args,1)
'lowercase 'oldname
            oldname = result
  newname = subwrd(args,2)

  'query file'
  numvars = sublin(result,6)
  numvars = subwrd(numvars,5)

  flag = false

  n = 1
  while ( n<numvars+1 )
  name  = sublin(result,6+n)
  name  = subwrd(name ,1)
        if( name=oldname )
        flag = true
        endif
  n = n+1
  endwhile

  if (flag=false)
       say     oldname' is not defined!  A zero-fill will be used.'
      'define 'newname' = 0'
  else
       say 'Renaming 'oldname' to 'newname' ...'
      'define 'newname' = 'oldname
  endif
