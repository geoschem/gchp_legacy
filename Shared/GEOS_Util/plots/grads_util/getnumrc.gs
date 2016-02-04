function getnumrc (args)

DIR = subwrd(args,1)

'!remove rcfiles'
'!/bin/ls -1 'DIR'/VERIFICATION*.rc | wc -l > rcfiles'
'!/bin/ls -1 'DIR'/VERIFICATION*.rc        >> rcfiles'
numrc = sublin( read(rcfiles),2 )

     string  = numrc
        cnt  = 1
while ( cnt <= numrc )
     rcfile  = sublin( read(rcfiles),2 )
     string  = string' 'rcfile
        cnt  = cnt + 1
endwhile

return string
