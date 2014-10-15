function setdates

* Create Environment Variables for Seasonal Utility
* -------------------------------------------------

'getinfo date'
         date = substr(result,1,5)
'run getenv "TBEG" '
         TBEG = result
'run getenv "TEND" '
         TEND = result

* Compute TIME Boundaries based on Input TBEG & TEND
* --------------------------------------------------
if( TBEG != NULL &  TEND != NULL ) 

     begyear  = substr(TBEG,1,4)
     begmonth = substr(TBEG,5,2)
        month = getmon(begmonth)
     begdate  = date''month''begyear

     endyear  = substr(TEND,1,4)
     endmonth = substr(TEND,5,2)
        month = getmon(endmonth)
     enddate  = date''month''endyear

else

'getinfo desc'
         desc = result
'!froot 'desc
'run getenv FROOT'
     basename = result

* Compute TIME Boundaries based on CLIM tabl file
* -----------------------------------------------
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
     begdate  = date''month''begyear

     endyear  = substr(endYYYYMM,1,4)
     endmonth = substr(endYYYYMM,5,2)
        month = getmon(endmonth)
     enddate  = date''month''endyear

else

* Compute TIME Boundaries based on XDF tabl file
* ----------------------------------------------
'getinfo tdim'
         tdim = result

'set t  '1
'getinfo date'
      begdate = result
'set t  'tdim
'getinfo date'
      enddate = result

endif

endif

say 'setenv "BEGDATE" 'begdate
say 'setenv "ENDDATE" 'enddate
'run setenv "BEGDATE" 'begdate
'run setenv "ENDDATE" 'enddate

return

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

