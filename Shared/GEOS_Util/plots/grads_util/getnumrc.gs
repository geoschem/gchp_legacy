function getnumrc (args)

DIR = subwrd(args,1)

'!remove rcfiles'
'!/bin/ls -1 'DIR'/VERIFICATION*.rc | wc -l > rcfiles'
'!/bin/ls -1 'DIR'/VERIFICATION*.rc        >> rcfiles'
numrc = sublin( read(rcfiles),2 )

num = ' '
cnt = 1
while ( cnt <= 100 & num = ' ' )
        num = substr(numrc,cnt,1)
        cnt = cnt + 1
endwhile

     string  = num
        cnt  = 1
while ( cnt <= num )
     rcfile  = sublin( read(rcfiles),2 )
     string  = string' 'rcfile
        cnt  = cnt + 1
endwhile

return string
