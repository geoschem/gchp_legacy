function corcmpopen (args)

'numargs  'args
 numargs = result/2

   EXPDIRS  = ''
       n = 0
while( n <= numargs-1 )
       loc1 = 2*n  + 1
       loc2 = loc1 + 1
   EXPDIR.n = subwrd(args,loc1)
   EXPDSC.n = subwrd(args,loc2)
   EXPDIRS  = EXPDIRS' 'EXPDIR.n
          n = n + 1
endwhile

'run getenv "GEOSUTIL"'
             geosutil = result

'!'geosutil'/plots/grads_util/create_subset_files 'EXPDIRS
'!/bin/mv -f subset_files 'EXPDSC.0'.setup'
say ''

* Open Control Experiment
* -----------------------
   SETCTL = EXPDSC.0'.setup'
 CTLFILES = sublin( read(SETCTL),2 )
            n  = 1
    while ( n <= CTLFILES )
           file  = sublin( read(SETCTL),2 )
    say 'Opening CTL: 'file
    'open 'file
            n = n + 1
    endwhile
    say ''

* Open Comparison Experiments
* ---------------------------
       L = 1
while( L <= numargs-1 )
       loc1 = 2*L  + 1
       loc2 = loc1 + 1
   EXPDIR.L = subwrd(args,loc1)
   EXPDSC.L = subwrd(args,loc2)

       close = close(SETCTL)
    CTLFILES = sublin( read(SETCTL),2 )
                m  = 1
        while ( m <= CTLFILES )
              cfile  = sublin( read(SETCTL),2 )

             '!remove ctlname.txt'
             '!basename 'cfile' > ctlname.txt'
               ctlname = sublin ( read(ctlname.txt),2 )
                 close = close(ctlname.txt)

                     k = 0
                   bit = substr(ctlname,1,1)
                 nodes = 1
                 while( bit != '' )
                     k = k+1
                   bit = substr(ctlname,k,1)
                   if( bit = "." )
                       nodes = nodes + 1
                   endif
                 endwhile
                 n2 = nodes-3

* For ctldate to use both beginning AND ending dates, use:
* --------------------------------------------------------
                 n1 = nodes
* For ctldate to use ONLY beginning dates, use:
* ---------------------------------------------
                 n1 = n2 + 1

             '!remove ctldate.txt'
             '!basename 'cfile' | cut -d. -f'n2'-'n1' > ctldate.txt'
                ctldate = sublin ( read(ctldate.txt),2 )
                close = close(ctldate.txt)

* Open Comparison Experiments
* ---------------------------
          SETEXP = EXPDSC.L'.setup'
'!remove 'SETEXP
'!/bin/ls -1 'EXPDIR.L'/*stats*.ctl1 | wc -l > 'SETEXP
'!/bin/ls -1 'EXPDIR.L'/*stats*.ctl1        >> 'SETEXP

          NUMFILES = sublin( read(SETEXP),2 )
            status = 999
                n  = 1
        while ( n <= NUMFILES )
               file  = sublin( read(SETEXP),2 )

             '!remove expname.txt'
             '!basename 'file' > expname.txt'
               expname = sublin ( read(expname.txt),2 )
                 close = close(expname.txt)

                     k = 0
                   bit = substr(expname,1,1)
                 nodes = 1
                 while( bit != '' )
                     k = k+1
                   bit = substr(expname,k,1)
                   if( bit = "." )
                       nodes = nodes + 1
                   endif
                 endwhile
                 n2 = nodes-3

* For expdate to use both beginning AND ending dates, use:
* --------------------------------------------------------
                 n1 = nodes
* For expdate to use ONLY beginning dates, use:
* ---------------------------------------------
                 n1 = n2 + 1

             '!remove expdate.txt'
             '!basename 'file' | cut -d. -f'n2'-'n1' > expdate.txt'
               expdate = sublin ( read(expdate.txt),2 )
                 close = close(expdate.txt)

             if( expdate = ctldate )
                'open 'file
                 say 'Opening EXP: 'file
                     n = NUMFILES 
                status = 0
             endif

            n = n + 1
        endwhile

        if( status = 999 )
            say 'Failed to find STATS file for EXPDIR: 'SETEXP
            say '                                Date: 'ctldate
            return status
        endif

* Note: If the files are NOT date-ordered (from the ls -1 command)
*       Then the files must be CLOSED and searched from the beginning
* -------------------------------------------------------------------
*       close = close(SETEXP)
*       NUMFILES = sublin( read(SETEXP),2 )
*       nbeg = 1

                m = m + 1
            close = close(SETEXP)
        endwhile

        say ''

L = L + 1
endwhile
return
