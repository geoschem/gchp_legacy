function getdates


* Initialize Time based on Existance of Pre-Set Environment Variables
* -------------------------------------------------------------------
'run getenv "BEGDATE"'
             begdate  = result
'run getenv "ENDDATE"'
             enddate  = result
'run getinfo tdim'
             tdim     = result
'run getinfo date'
             time = substr(result,1,5)

tmin = 0
tmax = tdim + 1

if( begdate != "NULL" & enddate != "NULL" )
    say 'Attempting to Set TIME based on BEGDATE: 'begdate'  and  ENDDATE: 'enddate
   'set time 'begdate' 'enddate
   'getinfo   tmin'
              tmin = result
   'getinfo   tmax'
              tmax = result
endif

if( tmin<1 | tmax>tdim )
    'getinfo file'
             file = result
        say 'TIME requested falls outside domain of File: 'file
        say 'Default times will be used.'
       'set t 1'
       'getinfo date'
        begdate = result
       'set t 'tdim
       'getinfo date'
        enddate = result
       'set t 1 'tdim

* Set TIME Boundaries based on CLIM tabl file (if necessary)
* ----------------------------------------------------------
       'getinfo desc'
                desc = result
       '!froot 'desc
       'run getenv FROOT'
            basename = result
       if(  basename = clim.tabl )
           'q ctlinfo'
           title = sublin(result,2)
           n = 1
           word = subwrd(title,n)
           while( word != '' )
           n = n + 1
           word = subwrd(title,n)

           if(    word = "Climatology:" )
               begYYYYMM = subwrd(title,n+1)
               endYYYYMM = subwrd(title,n+3)
           endif   
           endwhile
            
            begyear  = substr(begYYYYMM,1,4)
            begmonth = substr(begYYYYMM,5,2)
               month = getmon(begmonth)
            begdate  = time''month''begyear 
                   
            endyear  = substr(endYYYYMM,1,4)
            endmonth = substr(endYYYYMM,5,2)
               month = getmon(endmonth)
            enddate  = time''month''endyear
       
       endif
endif

'getinfo   tmin'
           tmin = result
'getinfo   tmax'
           tmax = result

say 'Setting Time Index: 'tmin' to 'tmax

return begdate' 'enddate

function getmon(num)
            if( num = "01" ) ; month = JAN ; endif
            if( num = "02" ) ; month = FEB ; endif
            if( num = "03" ) ; month = MAR ; endif
            if( num = "04" ) ; month = APR ; endif
            if( num = "05" ) ; month = MAY ; endif
            if( num = "06" ) ; month = JUN ; endif
            if( num = "07" ) ; month = JUL ; endif
            if( num = "08" ) ; month = AUG ; endif
            if( num = "09" ) ; month = SEP ; endif
            if( num = "10" ) ; month = OCT ; endif
            if( num = "11" ) ; month = NOV ; endif
            if( num = "12" ) ; month = DEC ; endif
return month

