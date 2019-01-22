#!/usr/bin/env python
import sys
from dateutils import *
import random
analdate = sys.argv[1]
verifdate = sys.argv[2]
yyyy,mm,dd,hh = splitdate(analdate)
analdate = datetime.datetime(yyyy,mm,dd,hh)
yyyy,mm,dd,hh = splitdate(verifdate)
verifdate = datetime.datetime(yyyy,mm,dd,hh)
diff = verifdate - analdate
hours = diff.seconds/3600
print '%s' % diff.days, '%s' % hours
