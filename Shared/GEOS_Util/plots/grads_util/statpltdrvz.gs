function driver (args)
field = subwrd  (args,1)
tag1  = subwrd  (args,2)
tag2  = subwrd  (args,3)
numf  = subwrd  (args,4)
flag  = subwrd  (args,5)

if( flag = 1 )

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.6 5.2  4.65 7.65'
'set grads off'
'set grid  off'
'run statdpltz 'field' Dmes 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 6.1 10.6  4.65 7.65'
'set grads off'
'set grid  off'
'run statdpltz 'field' Dvar 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.6  5.2   0.75 3.75'
'set grads off'
'set grid  off'
'run statdpltz 'field' Dmse 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 6.1 10.6   0.75 3.75'
'set grads off'
'set grid  off'
'run statdpltz 'field' Dres 'tag1' 'tag2' 'numf' 'flag

endif


if( flag = 2 )

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.6 5.2  4.65 7.65'
'set grads off'
'set grid  off'
'run statdpltz 'field' Damp 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 6.1 10.6  4.65 7.65'
'set grads off'
'set grid  off'
'run statdpltz 'field' Dphz 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.6  5.2   0.75 3.75'
'set grads off'
'set grid  off'
'run statdpltz 'field' Dmse 'tag1' 'tag2' 'numf' 'flag

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 6.1 10.6   0.75 3.75'
'set grads off'
'set grid  off'
'run statdpltz 'field' Dres 'tag1' 'tag2' 'numf' 'flag

endif

return
