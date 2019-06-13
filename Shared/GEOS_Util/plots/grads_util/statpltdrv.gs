function driver (args)
field = subwrd  (args,1)
tag1  = subwrd  (args,2)
tag2  = subwrd  (args,3)
numf  = subwrd  (args,4)
flag  = subwrd  (args,5)

* --------------------------------------------------------
* First Set:  MES, VAR, MSE, RES
* --------------------------------------------------------

if( flag = 1 )

'set vpage off'
'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.4 5.4  4.0 8.5'
'set grads off'
'set grid  off'
'run statdplt 'field' Dmes 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  4.0 8.5'
'set grads off'
'set grid  off'
'run statdplt 'field' Dvar 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.4 5.4  0.0 4.5'
'set grads off'
'set grid  off'
'run statdplt 'field' Dmse 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  0.0 4.5'
'set grads off'
'set grid  off'
'run statdplt 'field' Dres 'tag1' 'tag2' 'numf' 'flag

'set vpage off'
'set parea off'

endif

* --------------------------------------------------------
* Second Set:  AMP, PHZ, MSE, RES
* --------------------------------------------------------

if( flag = 2 )

'set vpage off'
'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.4 5.4  4.0 8.5'
'set grads off'
'set grid  off'
'run statdplt 'field' Damp 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  4.0 8.5'
'set grads off'
'set grid  off'
'run statdplt 'field' Dphz 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.4 5.4  0.0 4.5'
'set grads off'
'set grid  off'
'run statdplt 'field' Dmse 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 5.9 10.9  0.0 4.5'
'set grads off'
'set grid  off'
'run statdplt 'field' Dres 'tag1' 'tag2' 'numf' 'flag


'set vpage off'
'set parea off'

endif

return
