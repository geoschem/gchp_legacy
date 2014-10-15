dset ^ut_ocean.grd
title m_lp unit tester output
options big_endian sequential
undef 1e+15
xdef 144 linear 0 2.500000
ydef 91 linear -90.000000 2.000000
zdef 26 linear 0 2
tdef 1 linear 0Z21dec1997  6hr
vars 9
ts    0  0 sea surface temperature
qs    0  0 saturation specific humidity at ocean surface
z1    0  0 height above surface at middle of model lowest layer 
u1    0  0 wind speed at middle of model lowest layer 
t1    0  0 temperature at middle of model lowest layer 
q1    0  0 specific humidity at middle of model lowest layer 
u2    26 0 wind speed at surface layer 
t2    26 0 temperature at surface layer 
q2    26 0 specific humidity at surface layer 
endvars
