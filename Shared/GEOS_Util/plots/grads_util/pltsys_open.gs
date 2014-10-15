function pltsysopen (args)

   EXPDSC = subwrd(args,1)
   EXPDIR = subwrd(args,2)
   SETEXP = EXPDSC'.setup'

if( EXPDIR != '' )
    EXPDIR = EXPDIR'/'
endif

'!remove 'SETEXP
'!/bin/ls -1 'EXPDIR'*globl*.ctl1  | wc -l > 'SETEXP
'!/bin/ls -1 'EXPDIR'*globl*.ctl1         >> 'SETEXP

NUMFILES = sublin( read(SETEXP),2 )

* Open Control Experiment
* -----------------------
    say ''
            n  = 1
    while ( n <= NUMFILES )
           file  = sublin( read(SETEXP),2 )
    say 'Opening CTL: 'file
    'open 'file
            n = n + 1
    endwhile
    say ''
    return NUMFILES

