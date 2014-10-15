function getvarz (args)
*
* Returns the NAME and NUMLEVs for Variable # of Input
*
* eg) getvar 7
*     Returns the 7th variable and its number of levels
*
loc = subwrd(args,1)
'q file'
varinfo = sublin(result,6+loc)
name    = subwrd(varinfo,1)
levs    = subwrd(varinfo,2)

say 'Variable 'loc':  'name'  Levs: 'levs
return name' 'levs
