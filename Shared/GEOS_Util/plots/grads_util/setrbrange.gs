function rbrange (args)

*************************************************************
*****                                                   *****
*****  Usage:  set_rbrange_cint name field1 file_1      *****
*****                                field2 file_2      *****
*****                                field3 file_3 ...  *****
*****                                                   *****
*****  This function sets the RBRANGE and the CINT      *****
*****  based on an arbitrary number of input fields.    *****
*****  If NAME exists as a file, RBRANGE and CINT will  *****
*****  be read in, otherwise RBRANGE and CINT will be   *****
*****  computed.                                        *****
*****                                                   *****
*************************************************************

name   = subwrd(args,1)
field1 = subwrd(args,2)
file1  = subwrd(args,3)
field2 = subwrd(args,4)
file2  = subwrd(args,5)

'!/bin/rm 'name
'set dfile 'file1
'set_rbrange_cint 'name' 'field1
min1 = subwrd(result,1)
max1 = subwrd(result,2)

'!/bin/rm 'name
'set dfile 'file2
'set_rbrange_cint 'name' 'field2
min2 = subwrd(result,1)
max2 = subwrd(result,2)

max = (max1+max2)/2
min = (min1+min2)/2
cnt = (max-min)/15

'!/bin/rm 'name
write(name,min)
write(name,max)
write(name,cnt)

return
