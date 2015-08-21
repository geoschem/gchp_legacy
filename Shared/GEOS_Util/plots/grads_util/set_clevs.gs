function set_clevs (args)

'numargs  'args
 numargs = result

       'q shades'
       say ' result:'result
       line  = sublin( result, 1 )
       nmax  = subwrd( line,   5 )
       zcols = ''
       zlevs = ''
        n = 2
       while( n<=nmax )
       line.n = sublin( result, n+1 )
       zcol.n = subwrd( line.n, 1 )
       zlev.n = subwrd( line.n, 2 )
       zcols  = zcols' 'zcol.n
       zlevs  = zlevs' 'zlev.n
       n = n + 1
       endwhile
       say ' NMAX: 'nmax
       say 'CCOLS: 'zcols
       say 'CLEVS: 'zlevs

'set gxout contour'
'set clevs 'zlevs
'set ccolor 1'

return
end
