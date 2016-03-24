function lowercase (args)

        qname  = subwrd(args,1)
'!remove lowercase.txt'
'!echo 'qname' | tr "[:upper:]" "[:lower:]" > lowercase.txt'
         name = sublin ( read(lowercase.txt),2 )
'!remove lowercase.txt'
return name
