* ----------
'reinit'
'set display color white'
'set clab off'
'c'
**************************************************
'xdfopen /discover/nobackup/ltakacs/e0357/geosgcm_surf/clim.tabl'
'open /discover/nobackup/ltakacs/verification/GSSTF_Surf_Flux/GSSTF.Jul1987_Dec2000_Clim.01.ctl'

'setenv VERIFICATION /nobackup/ltakacs/verification'

'setlons'
'set lat -90 90'
'set z 1'

* Set BEGDATE and ENDDATE for Seasonal Calculations
* -------------------------------------------------
'setdates'

* Extract Beginning and Ending Dates for Plots
* --------------------------------------------
   'set t    '1
   'getinfo date'
         begdate = result
   'getinfo tdim'
            tdim     = result
   'set t  'tdim
   'getinfo date'
         enddate = result

* Land/Water Masks
* ----------------
   'setmask mod'
   'define omaskmod = maskout( 1, lwmaskmod-0.5 )'

'sett'
'define qmod1 = maskout( taux,omaskmod )'
'define qmod2 = maskout( tauy,omaskmod )'

'seasonal qmod1'
'seasonal qmod2'

'set dfile 2'
'setlons'
'set lat -90 90'

* Land/Water Masks
* ----------------
   'set z 1'
   'set t 1'
   'setmask obs'
   'define omaskobs = maskout( 1, lwmaskobs-0.5 )'

'set dfile 2'
   'set z 1'
    'getdates'
     begdateo = subwrd(result,1)
     enddateo = subwrd(result,2)

   'define qobs1 = maskout( uflux,omaskobs )'
   'define qobs2 = maskout( vflux,omaskobs )'

'seasonal qobs1'
'seasonal qobs2'

'set dfile 1'
'set lat -90 90'
'set lon 0 360'
'define qqm1 = regrid2(qmod1djf,1,1,bs_p1,0,-90)'
'define qqm2 = regrid2(qmod2djf,1,1,bs_p1,0,-90)'
'define qqq1 = regrid2(qobs1djf,1,1,bs_p1,0,-90)'
'define qqq2 = regrid2(qobs2djf,1,1,bs_p1,0,-90)'

'enable print stressdjf.gsstf.out'
'set vpage 0 8.5 5.5 11'
'set grads off'
'set arrscl 0.5 0.2'
'set rbrange 0 0.2'
'set cint 0.02'
'set arrlab off'
'd skip(qqm1,4);skip(qqm2,4);mag(skip(qqm1,4),skip(qqm2,4))'
'run cbar'
'draw title GEOS-5 DJF Surface Wind Stress'
'set vpage 0 8.5 0 5.5'
'set grads off'
'set arrscl 0.5 0.2'
'set rbrange 0 0.2'
'set cint 0.02'
'set arrlab off'
'd skip(qqq1,4);skip(qqq2,4);mag(skip(qqq1,4),skip(qqq2,4))'
'draw title GSSTF DJF Surface Wind Stress'
'q pos'
'print'
'disable print'
'!gxeps -c -i stressdjf.gsstf.out -o stressdjf.gsstf.eps'
'!gxps -c -i stressdjf.gsstf.out -o stressdjf.gsstf.ps'
'c'

'define qqm1 = regrid2(qmod1jja,1,1,bs_p1,0,-90)'
'define qqm2 = regrid2(qmod2jja,1,1,bs_p1,0,-90)'
'define qqq1 = regrid2(qobs1jja,1,1,bs_p1,0,-90)'
'define qqq2 = regrid2(qobs2jja,1,1,bs_p1,0,-90)'

'enable print stressjja.gsstf.out'
'set vpage 0 8.5 5.5 11'
'set grads off'
'set arrscl 0.5 0.2'
'set rbrange 0 0.2'
'set cint 0.02'
'set arrlab off'
'd skip(qqm1,4);skip(qqm2,4);mag(skip(qqm1,4),skip(qqm2,4))'
'run cbar'
'draw title GEOS-5 JJA Surface Wind Stress'
'set vpage 0 8.5 0 5.5'
'set grads off'
'set arrscl 0.5 0.2'
'set rbrange 0 0.2'
'set cint 0.02'
'set arrlab off'
'd skip(qqq1,4);skip(qqq2,4);mag(skip(qqq1,4),skip(qqq2,4))'
'draw title GSSTF JJA Surface Wind Stress'
'print'
'disable print'
'!gxeps -c -i stressjja.gsstf.out -o stressjja.gsstf.eps'
'!gxps -c -i stressjja.gsstf.out -o stressjja.gsstf.ps'
