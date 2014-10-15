***************************************************************************************
*	$Id: rmean.gs,v 1.2 2013-05-09 18:16:58 ltakacs Exp $
*	Copyright (C) 2004 Bin Guan.
*	Distributed under GNU/GPL.
***************************************************************************************
function rmean(arg)
*
* Calculate running mean of a time series.
*
* NOTE:  Any value for arg5 will disable running mean
***************************************************************************************
arg5=subwrd(arg,5)

input=subwrd(arg,1)
arg2 =subwrd(arg,2)

if(arg5 = '')
arg2 =subwrd(arg,2)
else
arg2 = -0
endif

if(arg2 = '')
usage()
return
endif

if(arg5 = '')
arg3=subwrd(arg,3)
else
arg3 = 0
endif

if(!valnum(arg3))
   window=arg2
   tminus=math_int(window/2)
   tplus=window-tminus-1
   output=arg3
else
   tminus=-arg2
   tplus=arg3
   output=subwrd(arg,4)
endif
if(output = '')
   output=display
endif

tt=tminus+tplus+1

'rmntmp='input
if(output='display'|output='DISPLAY')
   'd tloop(ave(rmntmp,t-'tminus',t+'tplus'))'
else
   'define 'output' = tloop(ave(rmntmp,t-'tminus',t+'tplus'))'
endif
'undefine rmntmp'
return
***************************************************************************************
function usage()
*
* Print usage information.
*
say '  Running mean of a time series.'
say ''
say '  USAGE 1: rmean <input> <window> [<output>]'
say ''
say '           Calculate running mean using the given length of window and assign result to <output>.'
say ''
say '  USAGE 2: rmean <input> <begin> <end> [<output>]'
say ''
say '           Calculate running mean using the window specified by <begin> and <end>, and assign'
say '           result to <output>.'
say ''
say '           If not specified, <output> defaults to DISPLAY.'
say '           If <output> = DISPLAY, the result is displayed and not defined in a variable.' 
say ''
say '  EXAMPLE 1: rmean sst 4 sst4trmn'
say '             Successively averages the field sst over 4 time steps t-2, t-1, t, t+1 and' 
say '             defines the  result as sst4trmn.'
say ''
say '  EXAMPLE 2: rmean sst -2 1 DISPLAY'
say '             Successively averages the field sst over 4 time steps t-2, t-1, t, t+1 and' 
say '             displays the  result.'
say ''
say '  Copyright (C) 2004 Bin Guan.'
say '  Distributed under GNU/GPL.'
return
