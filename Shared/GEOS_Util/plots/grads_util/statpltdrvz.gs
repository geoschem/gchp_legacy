function driver (args)
field = subwrd  (args,1)
tag1  = subwrd  (args,2)
tag2  = subwrd  (args,3)
numf  = subwrd  (args,4)
cint  = subwrd  (args,5)

say 'cint = 'cint

if( field = h )
 if( cint = '' )
     cint = 2
     if( type = mse ) ; cint = 100 ;endif
     if( type = mes ) ; cint = 10  ;endif
 endif
endif
if( field = u )
 if( cint = '' )
     cint = 0.2
     if( type = mse ) ; cint = 5   ;endif
     if( type = mes ) ; cint = 0.5 ;endif
 endif
endif
if( field = v )
 if( cint = '' )
     cint = 0.2
     if( type = mse ) ; cint = 5   ;endif
     if( type = mes ) ; cint = 0.5 ;endif
 endif
endif
if( field = t )
 if( cint = '' )
     cint = 0.1
     if( type = mse ) ; cint = 1   ;endif
     if( type = mes ) ; cint = 0.2 ;endif
 endif
endif
if( field = q )
 if( cint = '' )
     cint = 0.1
 endif
endif

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 0.6 5.2  4.55 7.55'
'set grads off'
'set grid  off'
'run statdpltz 'field' dbia 'tag1' 'tag2' 'numf' 'cint

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 6.1 10.6  4.55 7.55'
'set grads off'
'set grid  off'
'run statdpltz 'field' dstd 'tag1' 'tag2' 'numf' 'cint

'set parea off'
'set vpage 0 11 0 8.5'
'set parea 3.35 7.85  0.75 3.75'
'set grads off'
'set grid  off'
'run statdpltz 'field' drms 'tag1' 'tag2' 'numf' 'cint

return
