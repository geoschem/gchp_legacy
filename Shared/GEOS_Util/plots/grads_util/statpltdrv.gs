function driver (args)
field = subwrd  (args,1)
tag1  = subwrd  (args,2)
tag2  = subwrd  (args,3)
numf  = subwrd  (args,4)
cint  = subwrd  (args,5)

* --------------------------------------------------------
* To Plot Simple Diffs     (BIAS, STD, RMS), set flag  = 1
* To Plot Diffs of Squares (BIAS, STD, RMS), set flag  = 2
* --------------------------------------------------------

flag = 1

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.4 5.4  4.0 8.5'
'set grads off'
'set grid  off'
if( flag = 1 )
   'run statdplt 'field' dbia 'tag1' 'tag2' 'numf' 'cint
else
   'run statdplt 'field' mes 'tag1' 'tag2' 'numf' 'cint
endif

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  4.0 8.5'
'set grads off'
'set grid  off'
if( flag = 1 )
   'run statdplt 'field' dstd 'tag1' 'tag2' 'numf' 'cint
else
   'run statdplt 'field' var 'tag1' 'tag2' 'numf' 'cint
endif

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 3.15 8.15  0.0 4.5'
'set grads off'
'set grid  off'
if( flag = 1 )
   'run statdplt 'field' drms 'tag1' 'tag2' 'numf' 'cint
else
   'run statdplt 'field' mse 'tag1' 'tag2' 'numf' 'cint
endif


'set vpage off'
'set parea off'
return
