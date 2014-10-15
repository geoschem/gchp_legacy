dset ^ut_insitu.grd
title m_insitu unit tester output
options big_endian sequential
undef 1e+15
xdef 144 linear -180 2.500000
ydef 91 linear -90.000000 2.000000
zdef 20 levels
1000 925 850 700 500 400 300 250 200 150 
100 70 50 30 20 10 5 2 1 0.4 
tdef 1 linear 0Z21dec1997  6hr
vars 7
slp    0  0 slp
slpf   0  0 slp factor
hght   20 0  height
tmpu   20 0  temps
mixr   20 0  temps
uwnd   20 0 U-Wind (m/sec) []
vwnd   20 0  V-Wind (m/sec) []
endvars
