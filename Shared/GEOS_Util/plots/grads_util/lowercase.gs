function lowercas (args)

        qname  = subwrd(args,1)
'!echo 'qname' | tr "[:upper:]" "[:lower:]" > lowercase.txt'
         name = sublin ( read(lowercase.txt),2 )

'!/bin/rm lowercase.txt'
return name
