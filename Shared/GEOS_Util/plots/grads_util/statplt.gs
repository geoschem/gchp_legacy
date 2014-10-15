function statplt (args)
field = subwrd (args,1)
name  = field
label = field

'numargs  'args
 numargs = result

        num = 0
while ( num < numargs )
        num = num + 1
if( subwrd(args,num) = '-desc'   ) ; DESC  = subwrd(args,num+1) ; endif
if( subwrd(args,num) = '-nfcst'  ) ; nfcst = subwrd(args,num+1) ; endif
endwhile

'getinfo pagex'
         pagex = result
     if( pagex = 8.5 ) ; 'run setenv ORIENTATION PORTRAIT'  ; endif
     if( pagex = 11  ) ; 'run setenv ORIENTATION LANDSCAPE' ; endif

'getinfo level'
         level = result
'run getenv GEOSUTIL'
        geosutil = result

* Initialize Plot Values
* ----------------------
    FmCmean_COLS   = '59   57   55   47   44   37   36   34   33    0     21   22   23   24   25   26   27   28   29'
    CLAB    =  off
      Fmean_scale  =  1
    FmCmean_scale  =  1
    FmAmean_scale  =  1
    FmAstd_scale  =  1
    aerosol = false

if( field = 'tau' ) ; name = 'Total Aerosol'  ; endif
if( field = 'du'  ) ; name = 'Dust'           ; endif
if( field = 'ss'  ) ; name = 'Sea Salt'       ; endif
if( field = 'bc'  ) ; name = 'Black Carbon'   ; endif
if( field = 'oc'  ) ; name = 'Organic Carbon' ; endif
if( field = 'su'  ) ; name = 'Sulfate'        ; endif

if( field = 'tau' | field = 'du' )
    unit  = ""
    mean_cint  = 0.2
        aerosol = true
        mean_rbrange = '-3 0'
        FmAmean_LEVS = '-.45 -.4 -.35 -.3 -.25 -.2 -.15 -.1 -.05 .05 .01 .15 .2 .25 .3 .35 .4 .45'
        FmAmean_CINT = 0.05
        FmCmean_LEVS = '-13.5 -12.0 -10.5 -9.0 -7.5 -6.0 -4.5 -3.0 -1.5 1.5 3.0 4.5 6.0 7.5 9.0 10.5 12.0 13.5'
        FmCmean_CINT = 0.05
        FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
        FmAstd_LEVS = '0.5 1 1.5 2 2.5 3 3.5  4 4.5  5 5.5  6  7  8   9  10 11 12'
        FmAstd_LEVS = '0.05 .1 .15 .2 .25 .3 .35  .4 .45  .5 .55  .6  .7  .8   .9  1.0 1.1 1.2'
        FmAstd_CINT = 0.05
endif

if( field = 'ss' | field = 'bc' )
    unit  = ""
    mean_cint  = 0.2
        aerosol = true
        mean_rbrange = '-4 -2.4'
        FmAmean_LEVS = '-.45 -.4 -.35 -.3 -.25 -.2 -.15 -.1 -.05 .05 .01 .15 .2 .25 .3 .35 .4 .45'
        FmAmean_CINT = 0.05
        FmCmean_LEVS = '-13.5 -12.0 -10.5 -9.0 -7.5 -6.0 -4.5 -3.0 -1.5 1.5 3.0 4.5 6.0 7.5 9.0 10.5 12.0 13.5'
        FmCmean_CINT = 0.005
        FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
        FmAstd_LEVS = '0.5 1 1.5 2 2.5 3 3.5  4 4.5  5 5.5  6  7  8   9  10 11 12'
        FmAstd_LEVS = '0.05 .1 .15 .2 .25 .3 .35  .4 .45  .5 .55  .6  .7  .8   .9  1.0 1.1 1.2'
        FmAstd_CINT = 0.05
endif

if( field = 'oc' | field = 'su' )
    unit  = ""
    mean_cint  = 0.2
        aerosol = true
        mean_rbrange = '-4 -2.4'
        FmAmean_LEVS = '-.45 -.4 -.35 -.3 -.25 -.2 -.15 -.1 -.05 .05 .01 .15 .2 .25 .3 .35 .4 .45'
        FmAmean_CINT = 0.05
        FmCmean_LEVS = '-13.5 -12.0 -10.5 -9.0 -7.5 -6.0 -4.5 -3.0 -1.5 1.5 3.0 4.5 6.0 7.5 9.0 10.5 12.0 13.5'
        FmCmean_CINT = 0.005
        FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
        FmAstd_LEVS = '0.5 1 1.5 2 2.5 3 3.5  4 4.5  5 5.5  6  7  8   9  10 11 12'
        FmAstd_LEVS = '0.05 .1 .15 .2 .25 .3 .35  .4 .45  .5 .55  .6  .7  .8   .9  1.0 1.1 1.2'
        FmAstd_CINT = 0.05
endif

if( field = 'precip' )
    unit  = ""
    mean_cint  = 0.002
    FmCmean_scale = 86400
    FmAmean_scale = 50
    FmAstd_scale = 50
        aerosol = true
        mean_rbrange = '-4.6 -4.55'
        FmAmean_LEVS = '-.45 -.4 -.35 -.3 -.25 -.2 -.15 -.1 -.05 .05 .01 .15 .2 .25 .3 .35 .4 .45'
        FmAmean_CINT = 0.05
        FmCmean_LEVS = '-13.5 -12.0 -10.5 -9.0 -7.5 -6.0 -4.5 -3.0 -1.5 1.5 3.0 4.5 6.0 7.5 9.0 10.5 12.0 13.5'
        FmCmean_CINT = 1
        FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
        FmAstd_LEVS = '0.5 1 1.5 2 2.5 3 3.5  4 4.5  5 5.5  6  7  8   9  10 11 12'
        FmAstd_LEVS = '0.05 .1 .15 .2 .25 .3 .35  .4 .45  .5 .55  .6  .7  .8   .9  1.0 1.1 1.2'
        FmAstd_CINT = 0.05
endif

if( field = "p" )
    name  = "Sea-Level Pressure"
    unit  = "(mb)"
    label = "slp"
    mean_cint  = 4
        mean_rbrange = '970 1025'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-13.5 -12.0 -10.5 -9.0 -7.5 -6.0 -4.5 -3.0 -1.5 1.5 3.0 4.5 6.0 7.5 9.0 10.5 12.0 13.5'
        FmCmean_CINT = 1.5
        FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
        FmAstd_LEVS = '0.5 1 1.5 2 2.5 3 3.5  4 4.5  5 5.5  6  7  8   9  10 11 12'
        FmAstd_CINT = 0.5
endif

if( field = "h" )
    name  = "Heights"
    unit  = "(m)"
    label = "hght"
    FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    FmAstd_LEVS = '1  2  3  4  5  6  7  8  9  10 11   12  14  16  18  20   22   24'
    FmAstd_CINT = 1
    FmAstd_LEVS = '2  4  6  8  10  15  20  25  30  40  50  60  70  80  90  100  120  140'
    FmAstd_CINT = 2
    FmCmean_LEVS = '-45 -40 -35 -30 -25 -20 -15 -10 -5 5 10 15 20 25 30 35 40 45'
    FmCmean_CINT = 5
    FmCmean_LEVS = '-90 -80 -70 -60 -50 -40 -30 -20 -10 10 20 30 40 50 60 70 80 90'
    FmCmean_CINT = 10
    FmCmean_LEVS = '-135 -120 -105 -90 -75 -60 -45 -30 -15 15 30 45 60 75 90 105 120 135'
    FmCmean_CINT = 15
    if( level = 1000 )
        mean_cint  = 20
        mean_rbrange = '-200 200'
        FmAmean_LEVS = '-36 -32 -28 -24 -20 -16 -12 -8 -4 4 8 12 16 20 24 28 32 36'
        FmAmean_CINT = 4
    endif
    if( level = 850 )
        mean_cint  = 40
        mean_rbrange = '1000 1600'
        FmAmean_LEVS = '-36 -32 -28 -24 -20 -16 -12 -8 -4 4 8 12 16 20 24 28 32 36'
        FmAmean_CINT = 4
    endif
    if( level = 700 )
        mean_cint  = 50
        mean_rbrange = '2800 3100'
        FmAmean_LEVS = '-36 -32 -28 -24 -20 -16 -12 -8 -4 4 8 12 16 20 24 28 32 36'
        FmAmean_CINT = 4
    endif
    if( level = 500 )
        mean_cint  = 25
        mean_rbrange = '-200 200'
        FmAmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2 4 6 8 10 12 14 16 18'
        FmAmean_CINT = 2
        FmAmean_LEVS = '-36 -32 -28 -24 -20 -16 -12 -8 -4 4 8 12 16 20 24 28 32 36'
        FmAmean_CINT = 4
        mean_cint  = 60
        mean_rbrange = '4860 5880'
        FmAmean_LEVS = '-36 -32 -28 -24 -20 -16 -12 -8 -4 4 8 12 16 20 24 28 32 36'
        FmAmean_CINT = 4
    endif
    if( level = 400 )
        mean_cint  = 60
        mean_rbrange = '6400 7600'
        FmAmean_LEVS = '-36 -32 -28 -24 -20 -16 -12 -8 -4 4 8 12 16 20 24 28 32 36'
        FmAmean_CINT = 4
    endif
    if( level = 300 )
        mean_cint  = 60
        mean_rbrange = '8200 9600'
        FmAmean_LEVS = '-36 -32 -28 -24 -20 -16 -12 -8 -4 4 8 12 16 20 24 28 32 36'
        FmAmean_CINT = 4
    endif
    if( level = 250 )
        mean_cint  = 60
        mean_rbrange = '9400 10800'
        FmAmean_LEVS = '-45 -40 -35 -30 -25 -20 -15 -10 -5 5 10 15 20 25 30 35 40 45'
        FmAmean_CINT = 5
    endif
    if( level = 200 )
        mean_cint  = 60
        mean_rbrange = '10800 12400'
        FmAmean_LEVS = '-45 -40 -35 -30 -25 -20 -15 -10 -5 5 10 15 20 25 30 35 40 45'
        FmAmean_CINT = 5
    endif
    if( level = 150 )
        mean_cint  = 100
        mean_rbrange = '12400 14200'
        FmAmean_LEVS = '-45 -40 -35 -30 -25 -20 -15 -10 -5 5 10 15 20 25 30 35 40 45'
        FmAmean_CINT = 5
    endif
    if( level = 100 )
        mean_cint  = 100
        mean_rbrange = '14800 16600'
        FmAmean_LEVS = '-45 -40 -35 -30 -25 -20 -15 -10 -5 5 10 15 20 25 30 35 40 45'
        FmAmean_CINT = 5
    endif
endif

if( field = "u" )
    name  = "U-Wind"
    unit  = "(m/sec)"
    label = "uwnd"
    FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    FmAstd_LEVS = '2  3  4   5   6   7   8   9  10  11  12  13  14  15   16   17   18 20'
    FmAstd_CINT = 2
    FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
    FmCmean_CINT = 2.0
    if( level = 1000 )
        mean_cint  = 2
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
    endif
    if( level = 850 )
        mean_cint  = 5
        mean_rbrange = '-10 30'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
    endif
    if( level = 700 )
        mean_cint  = 5
        mean_rbrange = '-10 30'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
    endif
    if( level = 500 )
        mean_cint  = 5
        mean_rbrange = '-10 30'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
    endif
    if( level = 400 )
        mean_cint  = 5
        mean_rbrange = '-10 30'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
    endif
    if( level = 300 )
        mean_cint  = 5
        mean_rbrange = '-10 35'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
    endif
    if( level = 250 )
        mean_cint  = 5
        mean_rbrange = '-10 50'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
    endif
    if( level = 200 )
        mean_cint  = 5
        mean_rbrange = '-10 60'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
    endif
    if( level = 150 )
        mean_cint  = 5
        mean_rbrange = '-10 50'
        FmAmean_LEVS = '-9 -8 -7 -6 -5 -4 -3 -2 -1 1 2 3 4 5 6 7 8 9'
        FmAmean_CINT = 1.0
    endif
    if( level = 100 )
        mean_cint  = 5
        mean_rbrange = '-10 40'
        FmAmean_LEVS = '-9 -8 -7 -6 -5 -4 -3 -2 -1 1 2 3 4 5 6 7 8 9'
        FmAmean_CINT = 1.0
    endif
endif

if( field = "v" )
    name  = "V-Wind"
    unit  = "(m/sec)"
    label = "vwnd"
    FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    FmAstd_LEVS = '2  3  4   5   6   7   8   9  10  11  12  13  14  15   16   17   18 20'
    FmAstd_CINT = 2
    if( level = 1000 )
        mean_cint  = 2
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
        FmCmean_CINT = 2.0
    endif
    if( level = 850 )
        mean_cint  = 2
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
        FmCmean_CINT = 2.0
    endif
    if( level = 700 )
        mean_cint  = 2
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
        FmCmean_CINT = 2.0
    endif
    if( level = 500 )
        mean_cint  = 2
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
        FmCmean_CINT = 2.0
    endif
    if( level = 400 )
        mean_cint  = 2
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
        FmCmean_CINT = 2.0
    endif
    if( level = 300 )
        mean_cint  = 2
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
        FmCmean_CINT = 2.0
    endif
    if( level = 250 )
        mean_cint  = 2
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
        FmCmean_CINT = 2.0
    endif
    if( level = 200 )
        mean_cint  = 2
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
        FmCmean_CINT = 2.0
    endif
    if( level = 150 )
        mean_cint  = 3
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
        FmCmean_CINT = 2.0
    endif
    if( level = 100 )
        mean_cint  = 3
        mean_rbrange = '-10 10'
        FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmAmean_CINT = 0.5
        FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
        FmCmean_CINT = 2.0
    endif
endif

if( field = "t" )
    name  = "Temperature"
    unit  = "(K)"
    label = "tmpu"
    FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    FmAstd_LEVS = '.2  .4  .6 .8  1  1.2 1.4 1.6 1.8 2 2.3 2.5 2.7  3   3.5  4   4.5  5.0  '
    FmAstd_CINT = 0.2
    FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
    FmCmean_CINT = 0.5
    if( level = 1000 )
        mean_cint  = 2
        mean_rbrange = '270 300'
        FmAmean_LEVS = '-1.8 -1.6 -1.4 -1.2 -1.0 -.8 -.6 -.4 -.2 .2 .4 .6 .8 1.0 1.2 1.4 1.6 1.8'
        FmAmean_CINT = 0.2
    endif
    if( level = 850 )
        mean_cint  = 2
        mean_rbrange = '250 290'
        FmAmean_LEVS = '-3.6 -3.2 -2.8 -2.4 -2.0 -1.6 -1.2 -.8 -.4 .4 .8 1.2 1.6 2.0 2.4 2.8 3.2 3.6'
        FmAmean_CINT = 0.4
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
    endif
    if( level = 700 )
        mean_cint  = 2
        mean_rbrange = '250 290'
        FmAmean_LEVS = '-3.6 -3.2 -2.8 -2.4 -2.0 -1.6 -1.2 -.8 -.4 .4 .8 1.2 1.6 2.0 2.4 2.8 3.2 3.6'
        FmAmean_CINT = 0.4
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
    endif
    if( level = 500 )
        mean_cint  = 2
        mean_rbrange = '230 270'
        FmAmean_LEVS = '-1.8 -1.6 -1.4 -1.2 -1.0 -.8 -.6 -.4 -.2 .2 .4 .6 .8 1.0 1.2 1.4 1.6 1.8'
        FmAmean_CINT = 0.2
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
    endif
    if( level = 400 )
        mean_cint  = 2
        mean_rbrange = '220 260'
        FmAmean_LEVS = '-1.8 -1.6 -1.4 -1.2 -1.0 -.8 -.6 -.4 -.2 .2 .4 .6 .8 1.0 1.2 1.4 1.6 1.8'
        FmAmean_CINT = 0.2
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
    endif
    if( level = 300 )
        mean_cint  = 2
        mean_rbrange = '210 244'
        FmAmean_LEVS = '-1.8 -1.6 -1.4 -1.2 -1.0 -.8 -.6 -.4 -.2 .2 .4 .6 .8 1.0 1.2 1.4 1.6 1.8'
        FmAmean_CINT = 0.2
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
    endif
    if( level = 250 )
        mean_cint  = 2
        mean_rbrange = '206 232'
        FmAmean_LEVS = '-1.8 -1.6 -1.4 -1.2 -1.0 -.8 -.6 -.4 -.2 .2 .4 .6 .8 1.0 1.2 1.4 1.6 1.8'
        FmAmean_CINT = 0.2
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
    endif
    if( level = 200 )
        mean_cint  = 2
        mean_rbrange = '200 220'
        FmAmean_LEVS = '-2.7 -2.4 -2.1 -1.8 -1.5 -1.2 -.9 -.6 -.3 .3 .6 .9 1.2 1.5 1.8 2.1 2.4 2.7'
        FmAmean_CINT = 0.3
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
    endif
    if( level = 150 )
        mean_cint  = 2
        mean_rbrange = '200 220'
        FmAmean_LEVS = '-2.7 -2.4 -2.1 -1.8 -1.5 -1.2 -.9 -.6 -.3 .3 .6 .9 1.2 1.5 1.8 2.1 2.4 2.7'
        FmAmean_CINT = 0.3
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
    endif
    if( level = 100 )
        mean_cint  = 2
        mean_rbrange = '195 225'
        FmAmean_LEVS = '-2.7 -2.4 -2.1 -1.8 -1.5 -1.2 -.9 -.6 -.3 .3 .6 .9 1.2 1.5 1.8 2.1 2.4 2.7'
        FmAmean_CINT = 0.3
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
    endif
endif

if( field = "q" )
    name  = "Specific Humidity"
    unit  = "(g/g)"
    label = "sphu"
    Fmean_scale = 1000
    FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    FmAstd_LEVS = '.2  .4  .6 .8  1  1.2 1.4 1.6 1.8 2 2.3 2.5 2.7  3   3.5  4   4.5  5.0  '
    FmAstd_CINT = 0.2
    if( level = 1000 )
        mean_cint  = 1
        mean_rbrange = '1 16'
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
        FmAmean_LEVS = '-3.6 -3.2 -2.8 -2.4 -2.0 -1.6 -1.2 -.8 -.4 .4 .8 1.2 1.6 2.0 2.4 2.8 3.2 3.6'
        FmAmean_CINT = 0.4
    endif
    if( level = 850 )
        mean_cint  = 1
        mean_rbrange = '1 13'
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
        FmAmean_LEVS = '-3.6 -3.2 -2.8 -2.4 -2.0 -1.6 -1.2 -.8 -.4 .4 .8 1.2 1.6 2.0 2.4 2.8 3.2 3.6'
        FmAmean_CINT = 0.4
    endif
    if( level = 700 )
        mean_cint  = 1
        mean_rbrange = '1 10'
        FmCmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
        FmCmean_CINT = 0.5
        FmAmean_LEVS = '-3.6 -3.2 -2.8 -2.4 -2.0 -1.6 -1.2 -.8 -.4 .4 .8 1.2 1.6 2.0 2.4 2.8 3.2 3.6'
        FmAmean_CINT = 0.4
    endif
    if( level = 500 )
        mean_cint  = 0.5
        mean_rbrange = '.5 4'
        FmCmean_LEVS = '-2.7 -2.4 -2.1 -1.8 -1.5 -1.2 -.9 -.6 -.3 .3 .6 .9 1.2 1.5 1.8 2.1 2.4 2.7'
        FmCmean_CINT = 0.3
        FmAmean_LEVS = '-2.7 -2.4 -2.1 -1.8 -1.5 -1.2 -.9 -.6 -.3 .3 .6 .9 1.2 1.5 1.8 2.1 2.4 2.7'
        FmAmean_CINT = 0.3
        FmAstd_LEVS = '.1  .2  .3  .4  .5  .6  .7  .8  .9  1.0 1.1   1.2  1.3  1.4  1.5  1.6   1.8   2.0'
        FmAstd_CINT = 0.1
    endif
    if( level = 400 )
        mean_cint  = 0.2
        mean_rbrange = '.2 2'
        FmCmean_LEVS = '-1.8 -1.6 -1.4 -1.2 -1.0 -.8 -.6 -.4 -.2 .2 .4 .6 .8 1.0 1.2 1.4 1.6 1.8'
        FmCmean_CINT = 0.2
        FmAmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmAmean_CINT = 0.1
        FmAstd_LEVS = '.1  .2  .3  .4  .5  .6  .7  .8  .9  1.0 1.1   1.2  1.3  1.4  1.5  1.6   1.8   2.0'
        FmAstd_CINT = 0.1
    endif
    if( level = 300 )
        mean_cint  = 0.1
        mean_rbrange = '.1 1'
        FmCmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmCmean_CINT = 0.1
        FmAmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmAmean_CINT = 0.1
        FmAstd_LEVS = '.1  .2  .3  .4  .5  .6  .7  .8  .9  1.0 1.1   1.2  1.3  1.4  1.5  1.6   1.8   2.0'
        FmAstd_CINT = 0.1
    endif
    if( level = 250 )
        mean_cint  = 0.1
        mean_rbrange = '.1 1'
        FmCmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmCmean_CINT = 0.1
        FmAmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmAmean_CINT = 0.1
        FmAstd_LEVS = '.1  .2  .3  .4  .5  .6  .7  .8  .9  1.0 1.1   1.2  1.3  1.4  1.5  1.6   1.8   2.0'
        FmAstd_CINT = 0.1
    endif
    if( level = 200 )
        mean_cint  = 0.1
        mean_rbrange = '.1 1'
        FmCmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmCmean_CINT = 0.1
        FmAmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmAmean_CINT = 0.1
        FmAstd_LEVS = '.1  .2  .3  .4  .5  .6  .7  .8  .9  1.0 1.1   1.2  1.3  1.4  1.5  1.6   1.8   2.0'
        FmAstd_CINT = 0.1
    endif
    if( level = 150 )
        mean_cint  = 0.1
        mean_rbrange = '.1 1'
        FmCmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmCmean_CINT = 0.1
        FmAmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmAmean_CINT = 0.1
        FmAstd_LEVS = '.1  .2  .3  .4  .5  .6  .7  .8  .9  1.0 1.1   1.2  1.3  1.4  1.5  1.6   1.8   2.0'
        FmAstd_CINT = 0.1
    endif
    if( level = 100 )
        mean_cint  = 0.1
        mean_rbrange = '.1 1'
        FmCmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmCmean_CINT = 0.1
        FmAmean_LEVS = '-.9 -.8 -.7 -.6 -.5 -.4 -.3 -.2 -.1 .1 .2 .3 .4 .5 .6 .7 .8 .9'
        FmAmean_CINT = 0.1
        FmAstd_LEVS = '.1  .2  .3  .4  .5  .6  .7  .8  .9  1.0 1.1   1.2  1.3  1.4  1.5  1.6   1.8   2.0'
        FmAstd_CINT = 0.1
    endif
endif

if( field = "chi" )
    name  = "Velocity Potential (x 10`a6`n)"
    unit  = ""
    label = "chi"
      Fmean_scale = 1e-6
    FmCmean_scale = 1e-6
    FmAmean_scale = 1e-6
    mean_cint  = 1
    mean_rbrange = '-10 10'
    FmAstd_scale = 1e-6
    FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    FmAstd_LEVS = '.2  .4  .6 .8  1  1.2 1.4 1.6 1.8 2 2.3 2.5 2.7  3   3.5  4   4.5  5.0  '
    FmAstd_CINT = 0.2
    FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
    FmAmean_CINT = 0.5
    FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
    FmCmean_CINT = 2.0
endif

if( field = "psi" )
    name  = "Streamfunction (x 10`a6`n)"
    unit  = ""
    label = "psi"
      Fmean_scale = 1e-6
    FmCmean_scale = 1e-6
    FmAmean_scale = 1e-6
    mean_cint  = 20
    mean_rbrange = '-10 10'
    FmAstd_scale = 1e-6
    FmAstd_COLS = '0  50  42  44  46  48  39  37  36  34  32  31  21  22  24  25  26  27  28   29'
    FmAstd_LEVS = '.2  .4  .6 .8  1  1.2 1.4 1.6 1.8 2 2.3 2.5 2.7  3   3.5  4   4.5  5.0  '
    FmAstd_CINT = 0.2
    FmAmean_LEVS = '-4.5 -4 -3.5 -3 -2.5 -2 -1.5 -1 -.5 .5 1 1.5 2 2.5 3 3.5 4 4.5'
    FmAmean_CINT = 0.5
    FmCmean_LEVS = '-18 -16 -14 -12 -10 -8 -6 -4 -2 2  4 6   8 10 12 14 16 18'
    FmCmean_CINT = 2.0
endif

'getinfo lat'
         lat = result
'getinfo lon'
         lon = result

'set mproj latlon'


* Forecast (MEAN)
* ---------------
'set vpage 0 11 0 8.5'
'set parea 0.4 5.4 4.0 8.5'
'set grads off'
'set grid  off'
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd mask'
'set clab 'CLAB
'set gxout contour'
'set ccolor rainbow'
'set cint    'mean_cint
'set rbrange 'mean_rbrange
'd 'field'fmean*'Fmean_scale


* Forecast-Climatology (MEAN)
* ---------------------------
if( aerosol = 'true' )
   'define aerosol = exp('field'fmean) - 0.01'
   'define diff = regrid2( aerosol, .25, .25, bs_p1, 'lon', 'lat' )*'FmCmean_scale
else
   'define diff = regrid2( 'field'fmean-'field'cmean, .25, .25, bs_p1, 'lon', 'lat' )*'FmCmean_scale
endif

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  4.0 8.5'
'set grads off'
'set grid  off'
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd mask'
'set gxout shaded'
if( aerosol = 'true' )
   'shades aerosol 0 -cint 'FmCmean_CINT
else
   'set CCOLS 'FmCmean_COLS
   'set CLEVS 'FmCmean_LEVS
*  'black'
endif
   'd maskout( diff,abs(diff)-'FmCmean_CINT')'
*  'd diff'
'cbarn -scalex 0.55 -scaley 0.4 -xmid 8.45 -ymid 4.4 -snum 0.5'


* Forecast-Analysis (MEAN)
* ------------------------
'define diff = regrid2( 'field'fmean-'field'amean, .25, .25, bs_p1, 'lon', 'lat' )*'FmAmean_scale

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.4 5.4  0.2 4.7'
'set grads off'
'set grid  off'
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd  mask'
'set gxout shaded'
'set CCOLS 'FmCmean_COLS
'set CLEVS 'FmAmean_LEVS
*'black'
'd maskout( diff,abs(diff)-'FmAmean_CINT')'
*'d diff'
'cbarn -scale 0.55 -xmid 2.95'


* Forecast-Analysis (STD)
* -----------------------
'define diff = regrid2( 'field'std, .25, .25, bs_p1, 'lon', 'lat' )*'FmAstd_scale

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  0.2 4.7'
'set grads off'
'set grid  off'
'set gxout shaded'
'set ccols 92 0'
'set clevs 0.5'
'd mask'
'set CCOLS 'FmAstd_COLS
'set CLEVS 'FmAstd_LEVS
*'black'
'd maskout( diff,abs(diff)-'FmAstd_CINT')'
*'d diff'
'cbarn -sbar 0.55 -snum 0.45 -xmid 8.45'


'set vpage off'
'set string 1 c 6'
'set strsiz .13'

'getinfo month'
         month = result
'getinfo year'
         year  = result
'getinfo time'
         time  = result
'getinfo tinc'
         tinc  = result
         hour  = (time-1)*tinc

'draw string 5.5  8.4 'DESC'   'month' 'year'   'nfcst'-member Ensemble'
'draw string 5.5  8.12 Field: 'name'  Level: 'level' mb   Hour: 'hour

'set strsiz .10'
'draw string 2.8  7.85 Forecast'
'draw string 2.8  4.10 Mean (Forecast-Analysis)  CINT: 'FmAmean_CINT
'draw string 8.2  4.10 Standard Deviation (F-A)'
if( aerosol = 'true' )
   'draw string 8.2  7.85 Aerosol: EXP(x)-0.01'
else
   'draw string 8.2  7.85 Forecast-Climatology'
endif
