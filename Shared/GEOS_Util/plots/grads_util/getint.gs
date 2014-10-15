function int (args)
numb = subwrd(args,1)

  n = 1
int = ''
bit = x
while( bit != '' )
bit = substr(numb,n,1)
if( bit != '.' )
    int  = int''bit
else
    bit  = ''
endif
  n = n + 1
endwhile

return int
