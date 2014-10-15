function count  (args)
season  = subwrd(args,1)
begdate = subwrd(args,2)
enddate = subwrd(args,3)

'getdates'
if( begdate != "" & enddate != "" )
    say 'Count Utility based on BEGDATE: 'begdate'  and  ENDDATE: 'enddate
   'set time 'begdate' 'enddate
endif

'getinfo tmin'
         tmin = result
'getinfo tmax'
         tmax = result
quote = '"'
t = tmin
n = 0
while( t<=tmax )
'set t 't
'getinfo month'
         month = result

     if( quote''month''quote = season ) ; n = n + 1 ; endif

     if( season = quote''DJF''quote )
     if( quote''month''quote = quote''DEC''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''JAN''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''FEB''quote  ) ; n = n + 1 ; endif
     endif

     if( season = quote''MAM''quote )
     if( quote''month''quote = quote''MAR''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''APR''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''MAY''quote  ) ; n = n + 1 ; endif
     endif

     if( season = quote''JJA''quote )
     if( quote''month''quote = quote''JUN''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''JUL''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''AUG''quote  ) ; n = n + 1 ; endif
     endif

     if( season = quote''SON''quote )
     if( quote''month''quote = quote''SEP''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''OCT''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''NOV''quote  ) ; n = n + 1 ; endif
     endif

     if( season = quote''ANN''quote )
     if( quote''month''quote = quote''JAN''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''FEB''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''MAR''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''APR''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''MAY''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''JUN''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''JUL''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''AUG''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''SEP''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''OCT''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''NOV''quote  ) ; n = n + 1 ; endif
     if( quote''month''quote = quote''DEC''quote  ) ; n = n + 1 ; endif
     endif

t = t + 1
endwhile

     if( season = quote''DJF''quote ) ; n = n/3   ; endif
     if( season = quote''MAM''quote ) ; n = n/3   ; endif
     if( season = quote''JJA''quote ) ; n = n/3   ; endif
     if( season = quote''SON''quote ) ; n = n/3   ; endif
     if( season = quote''ANN''quote ) ; n = n/12  ; endif

* Note:  Limit n to 2 decimal points
* ----------------------------------
         ndot    = 2
         numdot  = 0
         numdot1 = 1
         while ( numdot1<20 )
         dot = substr(n,numdot1,1)
         if( dot='.' )
         numdot = numdot1
         endif
         numdot1 = numdot1 + 1
         endwhile
         if( numdot=2 )
             numdot = numdot+2
             n = substr(n,1,numdot)
         else 
             if( numdot=3 )
                 numdot = numdot+1
                 n = substr(n,1,numdot)
             else 
                 if( numdot!=0 )
                     n = substr(n,1,numdot)
                 endif
             endif
         endif

say "The Total Number of "season"'s = "n
return n
