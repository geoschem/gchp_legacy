#!/usr/bin/env python
#
# REVISION HISTORY:
#
# 26Mar2017 Todling  - initial version 
#
#--------------------------------------

import sys
from f90nml import Parser 

def my_usage():
    print 
    print "Usage: ";
    print "nmlread.py  rcfile namelist variable";
    print 
    exit(1)

n = len(sys.argv)
if n<3:
   my_usage();

rcfile_  = sys.argv[1]
nmlname_ = sys.argv[2]
nmlarg_  = sys.argv[3]

parser = Parser()
nml = parser.read(rcfile_)
var=nml[nmlname_][nmlarg_]
print var


