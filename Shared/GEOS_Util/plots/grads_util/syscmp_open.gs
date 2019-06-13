function syscmpopen (args)

   EXPDSC = subwrd(args,1)
   EXPDIR = subwrd(args,2)
   SETEXP = EXPDSC'.setup'

NUMFILES = sublin( read(SETEXP),2 )

* Open Experiment
* ---------------
    say ''
            n  = 1
    while ( n <= NUMFILES )
           file  = sublin( read(SETEXP),2 )
    say 'Opening CTL/EXP: 'file
    'open 'file
            n = n + 1
    endwhile
    say ''
    return NUMFILES

