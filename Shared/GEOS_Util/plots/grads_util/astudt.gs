***************************************************************************************
*	$Id: astudt.gs,v 1.1 2010-08-17 18:24:18 ltakacs Exp $
*	Copyright (C) 2004 Bin Guan.
*	Distributed under GNU/GPL.
***************************************************************************************
function astudt(arg)
*
* Calculate t for given dof and p in a two-tailed Student t-test. 
*
rc = gsfallow("on")
dof=subwrd(arg,1)
p=subwrd(arg,2)
tname=subwrd(arg,3)
if(p='')
usage()
return
endif
if(tname='')
tname='astudtout'
endif

v=0.5
dv=0.5
t=0

while(dv>1e-6)
t=1/v-1
dv=dv/2
'studt 'dof' 't
'q defval studtout 1 1'
*'q defval studtp 1 1'
studtp=subwrd(result,3)
if(studtp>p)
v=v-dv
else
v=v+dv
endif
endwhile

tname'='t
return
***************************************************************************************
function usage()
*
* Print usage information.
*
say '  Two-tailed Student t-test.'
say ''
say '  Usage: astudt <dof> <p> [<t>]'
say '     <dof>: degree of freedom.'
say '     <p>: probability.'
say '     <t>: t-statistic. Defaults to astudtout.'
say ''
say '  Dependencies: studt.gs'
say ''
say '  Copyright (C) 2004 Bin Guan.'
say '  Distributed under GNU/GPL.'
return
