function uppercase (args)

         qname  = args
'!remove uppercase.txt'
'!echo  'qname' | tr "[:lower:]" "[:upper:]" > uppercase.txt'
         qname = sublin ( read(uppercase.txt),2 )
'!remove uppercase.txt'

return   qname
