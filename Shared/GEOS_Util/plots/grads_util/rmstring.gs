function rmstring (args)

'run getenv VERBOSE'
            verbose = result

"get_titles "args" 1"
             desc   = result
"get_titles "args" 2"
            string = result

if( verbose != NULL )
    say 'Inside rmstring,   desc: 'desc
    say 'Inside rmstring, string: 'string
endif

* Remove STRING from DESC
* -----------------------
dlength = getlength(desc)
slength = getlength(string)

if( verbose != NULL )
    say 'Inside rmstring,   desc length: 'dlength
    say 'Inside rmstring, string length: 'slength
endif

alias   = ""
        i = 1
while ( i<=dlength )
      dum = substr(desc,i,slength)
      if( dum != string )
          bit  = substr(desc,i,1)
         alias = alias % bit
         if( verbose != NULL ) ; say 'i: 'i'  alias: 'alias'  dum: 'dum ; endif
             i = i+1
      else
         if( verbose != NULL ) ; say 'i: 'i'  alias: 'alias'  dum: 'dum ; endif
             i = i + slength
      endif
      
if( verbose != NULL )
    if( i > 256 )
        say 'Hit Enter to continue ...'
        pull flag
    endif
endif

endwhile
desc = alias

return desc

function getlength (string)

maxlength = 256
       tb = ""

      i = 1
while (i<=maxlength)
    blank = substr(string,i,1)
if( blank = tb )
    length = i-1
    i = maxlength + 1
else
    i = i + 1
endif
endwhile

return length
