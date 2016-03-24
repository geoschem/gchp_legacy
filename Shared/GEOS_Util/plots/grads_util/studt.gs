***************************************************************************************
*	$Id$
*	Copyright (C) 2004 Bin Guan.
*	Distributed under GNU/GPL.
***************************************************************************************
function studt(arg)
*
* Calculate p for given dof and t in a two-tailed Student t-test. 
*
dof=subwrd(arg,1)
t=subwrd(arg,2)
pname=subwrd(arg,3)
if(t='')
usage()
return
endif
if(pname='')
pname='studtout'
endif

pi=3.141593
pid2=pi/2.0

t=math_abs(t)
w=t/math_sqrt(dof)
th=math_atan(w)

if(dof=1)
p=1-th/pid2
pname'='p
return
endif

sth=math_sin(th)
cth=math_cos(th)
if(math_fmod(dof,2)=1)
p=1-(th+sth*cth*statcom(cth*cth,2,dof-3,-1))/pid2
else
p=1-sth*statcom(cth*cth,1,dof-3,-1)
endif
pname'='p
return
***************************************************************************************
*
* Calculate some statistic. 
*
function statcom(q,i,j,b)
zz=1
z=zz

k=i
while(k<=j)
zz=zz*q*k/(k-b)
z=z+zz
k=k+2
endwhile

return z
***************************************************************************************
function usage()
*
* Print usage information.
*
say '  Two-tailed Student t-test.'
say ''
say '  Usage: studt <dof> <t> [<p>]'
say '         <dof>: degree of freedom.'
say '         <t>: t-statistic.'
say '         <p>: probability. Defaults to studtout.'
say ''
say '  Copyright (C) 2004 Bin Guan.'
say '  Distributed under GNU/GPL.'
return
