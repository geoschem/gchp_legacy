function cmplt (args)

field  = subwrd(args,1)
file1  = subwrd(args,2)
file2  = subwrd(args,3)
output = subwrd(args,4)

season = 'NULL'

if( file1  = '' ) ;  file1 = 1      ; endif
if( file2  = '' ) ;  file2 = 2      ; endif
if( output = '' ) ; output = 'NULL' ; endif

'run getinfo date'
             date  = result
'run getinfo label  'file1
             desc1 = result
'run getinfo label  'file2
             desc2 = result

'getinfo time'
         time = result

'run getinfo file'
             file = result

'set dfile 'file2
'set t  'time
'getinfo date'
         date2 = result
'set dfile 'file1
'set t  'time
'getinfo date'
         date1 = result
'set dfile 'file
'set t 'time

'define qmod = 'field'.'file1'(t='time')'
'define qobs = 'field'.'file2'(t='time')'

'run makplot 'field' test1 NULL 'season' 'output' 'file1' 'desc1' 'file2' 'field' 'desc2' 'date1' 'date1' 'date2' 'date2' Actual NULL NULL'
