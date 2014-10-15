function fixname (desc)

* Remove possible BLANKS from Experiment Description
* --------------------------------------------------
DESC = ''
length = getlength(desc)
i = 1
while( i<=length )
  bit = substr(desc,i,1)
  if( bit != ' ' )
      if( DESC = '' )
          DESC = bit
      else
          DESC = DESC''bit
      endif
  endif
i = i+1
endwhile

* Ensure NAME has no underscores or dashes
* ----------------------------------------
length = getlength(DESC)
alias  = ""
     i = 1
while ( i<=length )
      bit = substr(DESC,i,1)
  if( bit != "_" & bit != "-" ) ; alias = alias % bit ; endif
i = i+1
endwhile
DESC = alias

return DESC

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
