#!/usr/bin/env python
"Check obsys_rc file against available data."

from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser
from glob     import glob
from os.path  import abspath, basename
from re       import compile
from sys      import stdout
from time     import strftime

from obsys_rc import ObsysRc

global zeros, zeros_
zeros  = "00000000_0000"
zeros_ = "00000000_00"

#.......................................................................
def check(filename="obsys.rc",
          newfile="default",
          errfile="default",
          obslist=["all"],
          ignore_gaps=[],
          lastday=7):
   """
   Check obsys_rc file against available data.

   input parameters
   => filename: name of obsys_rc to check
   => newfile: name of output new file; defaults to basename(filename)+".new"
   => errfile: name of output err file; defaults to basename(filename)+".err"
   => obslist: list of observation classes to process
   => ignore_gaps: list of obsclass[=threshold] values where data gaps
      less than or equal to threshold (hours) will be ignored;
      - if obsclass eq "all", then the associated threshold value applies to
        all obsclasses that don't have a specific value assigned to them
      - if a data collection is listed in ignore_gaps without a threshold
        value, then the threshold defaults to 24 (hours)
      - if a data collection is not listed in ignore_gaps, then threshold
        defaults to the interval of the data collection (i.e. no gap) or
        to 1 hour, if the data interval is less than 1 hour
      - to show all data gaps for collections with interval less than an hour,
        use threshold = 0
   => lastday: number of days before "today" to stop looking for data
   """
   global pattern
   global today
   global zeros, zeros_

   # initializations
   #----------------
   set_global_pattern_dictionary()
   today = strftime("%Y%m%d_2359")
   onehour = 60

   border1 = "  #"+"-"*26+"\n"
   border2 = "  #"+"="*26+"\n"
   indefinite = "21001231_1800"

   misfile_list = []

   # open output files
   #------------------
   if newfile == "default":
      newfile = basename(filename) + ".new"
   if abspath(newfile) == abspath(filename):
      msg = "Cannot write new info back to the same file: {0}"
      raise Exception(msg.format(abspath(newfile)))

   if errfile == "default":
      errfile = basename(filename) + ".err"
   if abspath(errfile) == abspath(filename):
      msg = "Cannot write error info back to the same file: {0}"
      raise Exception(msg.format(abspath(errfile)))

   if newfile == errfile:
      msg = "Cannot use same name for newfile and errfile: {0}"
      raise Exception(msg.format(newfile))

   # set ignore_gaps thresholds
   #---------------------------
   threshold = {}
   for obsStr in ignore_gaps:
      if "=" in obsStr:
         (obsclass, threshHrs) = obsStr.split("=")
      else:           
         obsclass = obsStr
         threshHrs = 24
      threshold[obsclass] = threshHrs

   # set lastdate to look for data
   #------------------------------
   if lastday > 0: lastday = -1 * lastday
   deltaMins = int(lastday)*24*60
   lastdate = incr_datetime(today, deltaMins)[0:11]

   # get data from obsys_rc file
   #----------------------------
   obsys = ObsysRc(filename)

   # open output files
   #------------------
   newfl = open(newfile, mode="w")
   errfl = open(errfile, mode="w")

   # loop thru data from obsys_rc file
   #----------------------------------
   for (obsclass, recvals) in obsys.obsinfo():
      if "all" not in obslist and obsclass not in obslist:
         continue

      print("\nChecking {0}".format(obsclass))

      # set thresh gap value
      #---------------------
      if threshold.has_key("all"):
         threshHrs = threshold["all"]
      else:
         threshHrs = 1   # default to one hour
      threshHrs = threshold.get(obsclass, threshHrs)

      # output prolog lines, table start, and comments
      #-----------------------------------------------
      if recvals["prolog"]:
         for line in recvals["prolog"]:
            newfl.write(line+"\n")
            errfl.write(line+"\n")

      if recvals["stdname"] == "":
         newfl.write("BEGIN {0}\n".format(obsclass))
         errfl.write("BEGIN {0}\n".format(obsclass))
      else:
         newfl.write("BEGIN {0} => {1}\n".format(obsclass, recvals["stdname"]))
         errfl.write("BEGIN {0} => {1}\n".format(obsclass, recvals["stdname"]))

      for line in recvals["comments"]:
         newfl.write(line+"\n")
         errfl.write(line+"\n")

      # sort obsclass rows by template
      #-------------------------------
      # Notes
      # -----
      # 1. an obsclass can have multiple templates
      # 2. datetimes in the file have format, yyyymmdd_hh, but in this script
      #    datetimes are stored as yyyymmdd_hhnn, since some data intervals
      #    are less than an hour.
      #----------------------------------------------------------------------
      template_info_list = []
      for row in recvals["rows"]:
         (start_stop, interval, template) = row.split()
         (start, stop) = pattern["start_stop"].search(start_stop).groups()
         start += "00"
         stop  += "00"

         if dict(template_info_list).has_key((template, interval)):
            dict(template_info_list)[(template, interval)].append((start, stop))
         else:
            template_info_list.append(((template, interval), [(start, stop)]))

      # process and output rows by template
      #------------------------------------
      nodata_list = []
      for ((template, interval), start_stop_list) in template_info_list:
         print("=> {0}".format(template))
         got_data_info = {}

         for (start, stop) in start_stop_list:
            start_ = start[0:11]
            stop_ = stop[0:11]
            line = "  %sz-%sz %s %s\n"%(start_, stop_, interval, template)
            errfl.write(line)

            if template in got_data_info:
               continue

            # get list of datetimes for data (plus other info)
            #-------------------------------------------------
            (datetime_list, hhmm_list, misfiles) = get_data_info(template)
            got_data_info[template] = 1

            if hhmm_list == {}:
               msg = "  # NO DATA FOUND!!!\n"
               errfl.write(border2+msg+border1+"\n")
               nodata_list.append((template, interval))
               continue

            misfile_list.extend(misfiles)

            # determine actual data interval
            #-------------------------------
            num_times = len(hhmm_list.keys())
            intervalMins = (24.*60.)/num_times
   
            if intervalMins != int(intervalMins):
               msg = "Non-divisible number of hour+min times found: {0} for {1}"
               raise Exception(msg.format(num_times, template))
            intervalMins = int(intervalMins)

            hh = intervalMins / 60
            if hh == 0:
               nn = intervalMins
            else:
               nn = intervalMins % (hh*60)
            interval_actual = "%02d%02d00"%(hh, nn)

            # get list of start_stop tuples, each representing a span
            # of continous data, ignoring gaps less than the threshold
            #---------------------------------------------------------
            threshMins = max(intervalMins, int(threshHrs)*60)
            threshHrs = threshMins/60.0
            if threshHrs == int(threshHrs):
               threshHrs = int(threshHrs)

            start_stop_data = get_start_stop_tuples(datetime_list, threshMins)

            # write records to newfile
            #-------------------------
            new_start = True
            last_index = len(start_stop_data) - 1

            for index, (start, stop) in enumerate(start_stop_data):

               # truncate datetimes, i.e remove minutes
               #---------------------------------------
               if new_start:
                  start_ = start[0:11]
                  new_start = False
               stop_ = stop[0:11]

               # merge contiguous stop-start tuples on newfile
               #------------------------------------------------
               # check whether truncated stop datetime is within
               # one hour of the next start datetime (truncated)
               #------------------------------------------------
               if index < last_index:
                  (start1, stop1) = start_stop_data[index+1]
                  start0 = incr_datetime(start1, -onehour)

                  if stop_ >= start0[0:11]:
                     continue

               # if stop is near today, then assume data still being collected
               #--------------------------------------------------------------
               if start_ < lastdate and stop >= lastdate:
                  stop_ = indefinite[0:11]

               # write to newfile
               #-----------------
               line = "  %sz-%sz %s %s\n"%(start_, stop_, interval_actual, template)
               if start == zeros:
                  line = "#" + line[1:]
               newfl.write(line)
               new_start = True

            # make note of interval discrepancy
            #----------------------------------
            if interval != interval_actual:
               msg  = "  # INCORRECT INTERVAL\n"+border1
               msg += "  # interval listed: {0}\n".format(interval)
               msg += "  # interval actual: {0}\n".format(interval_actual)
               errfl.write(border2+msg+border1+"\n")
               interval = interval_actual

            # determine date range for checking data
            #---------------------------------------
            first = min(start_stop_list[0][0], start_stop_data[0][0])
            final = max(start_stop_list[-1][1], start_stop_data[-1][1])

            # check that first datetime is "on track"
            #----------------------------------------
            first_hhmm = first[9:13]
            if first_hhmm not in hhmm_list:
               first_hhmm = sorted(hhmm_list.keys())[0]
            first = first[0:9]+first_hhmm

            # final sanity checks
            #--------------------
            if final > today:
                final = today

            if final < first:
                msg = "first datetime is greater than final: {0} > {1}"
                raise Exception(msg.format(first, final))

            # loop from first to final looking for missing or extra data files
            #-----------------------------------------------------------------
            miss_data_list = []
            found_data_list = []

            datetime = first
            while datetime <= final:

               if datetime == zeros:  # i.e. no data were found
                  break

               # data listed but not found
               #--------------------------
               if included(start_stop_list, datetime):
                  if not included(start_stop_data, datetime):
                     miss_data_list.append(datetime)

               # data found but not listed
               #--------------------------
               elif datetime in datetime_list:
                  found_data_list.append(datetime)

               datetime = incr_datetime(datetime, intervalMins)
                
            # write miss data info to errfile
            #--------------------------------
            if miss_data_list:
                msg  = "  # MISSING DATA\n"

                if threshMins != intervalMins:
                   if threshHrs <= 1:
                      msg += "  # data gaps > {0} mins\n".format(threshMins)
                   else:
                      msg += "  # data gaps > {0} hrs\n".format(threshHrs)

                if lastdate < stop:
                   msg += "  # prior to {0}z\n".format(lastdate)

                miss_tuples = get_start_stop_tuples(miss_data_list, intervalMins)
                wrote_info = False
                for (start, stop) in miss_tuples:

                    if start < lastdate:
                        if not wrote_info:
                            errfl.write(border2+msg+border1)
                            wrote_info = True

                        if intervalMins < onehour:
                           start_ = start
                           stop_ = stop
                        else:
                           start_ = start[0:11]
                           stop_ = stop[0:11]

                        if start == stop:
                            errfl.write("  [%sz]\n"%(start_))
                        else:
                            errfl.write("  [%sz - %sz]\n"%(start_, stop_))

                if wrote_info:
                    errfl.write(border1+"\n")

            # write extra found data info to errfile
            #---------------------------------------
            if found_data_list:
                msg = "  # MORE DATA FOUND\n"
                errfl.write(border2+msg+border1)

                nogap_tuples = get_start_stop_tuples(found_data_list, intervalMins)
                for (start, stop) in nogap_tuples:

                   if intervalMins < onehour:
                      start_ = start
                      stop_ = stop
                   else:
                      start_ = start[0:11]
                      stop_ = stop[0:11]

                   if start == stop:
                      errfl.write("  [%sz]\n"%(start_))
                   else:
                      errfl.write("  [%sz - %sz]\n"%(start_, stop_))
                errfl.write(border1+"\n")

            # write misfile info to errfile
            #------------------------------
            if misfiles:
                msg  = "  # MISFILES\n"
                errfl.write(border2+msg+border1)
                misfiles.sort()
                for mis in misfiles:
                    errfl.write("  #"+mis+"\n")
                errfl.write(border1+"\n")

      # write zeros for templates which found no data
      #----------------------------------------------
      for (template, interval) in nodata_list:
         line = "# %sz-%sz %s %s\n"%(zeros_, zeros_, interval, template)
         newfl.write(line)

      # output table end
      #-----------------
      newfl.write("END\n")
      newfl.flush()

      errfl.write("END\n")
      errfl.flush()

   newfl.close()
   errfl.close()

   # write misfile summary to stdout
   #--------------------------------
   if misfile_list:
       msg  = "\n# MISLABELED and MISPLACED files\n" \
           +    "#-------------------------------\n"
       stdout.write(msg)
       misfile_list.sort()
       for mis in misfile_list:
           stdout.write(mis+"\n")

#.......................................................................
def get_data_info(template):
   """
   Return a list of datetimes [yyyymmdd_hhnn] of available data found using
   specified template. Also return the data interval in minutes and a list
   of misplaced or misnamed files.

   input parameter
   => template: data path/name of data collection template
   """
   global zeros

   # regular expression patterns to find date/time
   #----------------------------------------------
   global pattern

   # get list of data filepaths
   #---------------------------
   index = template.find(":")+1
   tmpl = template[index:]
   tmpl = tmpl.replace("%y4", "????").replace("%y2", "??").replace("%m2", "??")
   tmpl = tmpl.replace("%d2", "??").replace("%h2", "??").replace("%n2", "??")
   tmpl = tmpl.replace("%j3", "???").replace("%c", "?")

   filepath_list = []
   filepath_list = glob(tmpl)

   # extract datetimes from available data
   #--------------------------------------
   hhmm_list = {}
   datetime_list = []
   misfiles = []

   if filepath_list:
      for fpath in filepath_list:
         year  = None
         month = None
         day   = None
         jjj   = None
         hour  = None
         min   = "00"

         # extract date/time from filename
         #--------------------------------
         if pattern["Ayyyyjjj_hhnn"].search(fpath):
            returnVals = pattern["Ayyyyjjj_hhnn"].search(fpath).groups()
            (year, jjj, hour, min) = returnVals

         elif pattern["yyyymmddhh"].search(fpath):
            returnVals = pattern["yyyymmddhh"].search(fpath).groups()
            (year, month, day, hour) = returnVals

         elif pattern["yyyymmdd_hhnnz"].search(fpath):
            returnVals = pattern["yyyymmdd_hhnnz"].search(fpath).groups()
            (year, month, day, hour, min) = returnVals

         elif pattern["yyyymmdd_hh"].search(fpath):
            returnVals = pattern["yyyymmdd_hh"].search(fpath).groups()
            (year, month, day, hour) = returnVals

         elif pattern["yyyymmdd__hhz"].search(fpath):
            returnVals = pattern["yyyymmdd__hhz"].search(fpath).groups()
            (year, month, day, hour) = returnVals

         elif pattern["yymmdd__hhz"].search(fpath):
            returnVals = pattern["yymmdd__hhz"].search(fpath).groups()
            (yy, month, day, hour) = returnVals
            if int(yy ) > 60: year = "19"+yy
            else:             year = "20"+yy

         elif pattern["yyyymmdd"].search(fpath):
            returnVals = pattern["yyyymmdd"].search(fpath).groups()
            (year, month, day) = returnVals
            hour = "00"

         elif pattern["yyyyjjj_hhnn"].search(fpath):
            returnVals = pattern["yyyyjjj_hhnn"].search(fpath).groups()
            (year, jjj, hour, min) = returnVals

         elif pattern["Y4_M2_D2"].search(fpath) and pattern["hhz"].search(fpath):
            (year, month, day) = pattern["Y4_M2_D2"].search(fpath,1).groups()
            (hour,) = pattern["hhz"].search(fpath).groups()

         else:
            msg = "Cannot extract date/time from filename in fpath: {0}"
            raise Exception(msg.format(fpath))

         # check for misplaced or mislabeled files
         #----------------------------------------
         if pattern["YYYY_JJJ"].search(fpath):
            (YYYY, JJJ) = pattern["YYYY_JJJ"].search(fpath).groups()

            if jjj == None:
               msg = "JJJ (day-of_year) found in path but not in filename: {0}"
               raise Exception(msg.format(fpath))

            if year != YYYY or jjj != JJJ:
               misfiles.append("(MISPLACED) "+fpath)
               continue

            (month, day) = jjj2mmdd(year, jjj)

         elif pattern["Y4_M2"].search(fpath):
            (yyyy, mm) = pattern["Y4_M2"].search(fpath).groups()

            if year != yyyy or month != mm:
               misfiles.append("(MISPLACED) "+fpath)
               continue

         if int(month) < 1 or int(month) > 12:
            misfiles.append("(MISLABELED) "+fpath)
            continue

         numdays = num_days_in_month(int(year), int(month))
         if int(day) < 1 or int(day) > numdays:
            misfiles.append("(MISLABELED) "+fpath)
            continue
                 
         if int(hour) > 24:
            misfiles.append("(MISLABELED) "+fpath)
            continue
                 
         if int(min) > 60:
            misfiles.append("(MISLABELED) "+fpath)
            continue
                 
         datetime = year+month+day+"_"+hour+min
         if (datetime > today):
            misfiles.append("(FUTURE_DATE?) "+fpath)
            continue

         # add file date/time to list
         #---------------------------
         datetime_list.append(datetime)
         hhmm_list[hour+min] = 1

   if hhmm_list:
      datetime_list.sort()
   else:
      datetime_list = [zeros]

   return (datetime_list, hhmm_list, misfiles)

#.......................................................................
def get_start_stop_tuples(datetime_list, intervalMins):
   """
   Take a list of data datetimes and return a list of (start, stop) datetime
   tuples, each representing a span of continuous available data (i.e. no gaps)
   """
   start = datetime_list[0]
   previous = start
   next_datetime = incr_datetime(start, intervalMins)

   tuple_list = []
   for datetime in datetime_list[1:]:
      if datetime > next_datetime:
         tuple_list.append((start, previous))
         start = datetime

      previous = datetime
      next_datetime = incr_datetime(datetime, intervalMins)

   tuple_list.append((start, previous))

   return tuple_list

#.......................................................................
def included(date_ranges, datetime):
   "Return True if datetime is in any of the included date_ranges"
   for (start, stop) in date_ranges:
      if datetime >= start and datetime <= stop:
         return True
   return False

#.......................................................................
def incr_datetime(datetime, deltaMins):
   """
   Increment datetime by delta minutes and return the value.

   input parameters
   => datetime: date/time in yyyymmdd_hhnnz format
   => deltaMins: integer number of minutes to add to datetime

   return value
   => new_datetime: format yyyymmdd_hhnnz
   """
   year  = int(datetime[0:4])
   month = int(datetime[4:6])
   day   = int(datetime[6:8])
   try:
      hour = int(datetime[9:11])
      min  = int(datetime[11:13])
   except ValueError:
      msg = "EXCEPTION: datetime = {0}"
      raise Exception(msg.format(datetime))

   min += deltaMins
   while min > 59:
      min -= 60
      hour += 1

   while min < 0:
      min += 60
      hour -= 1

   while hour > 23:
      hour -= 24
      day += 1

      if day > num_days_in_month(year, month):
         day = 1
         month += 1

         if month > 12:
            month = 1
            year += 1

   while hour < 0:
      hour += 24
      day -= 1

      if day < 1:
         month -= 1

         if month < 1:
            month = 12
            year -=1

         day = num_days_in_month(year, month)

   return "%04d%02d%02d_%02d%02d"%(year, month, day, hour, min)

#.......................................................................
def jjj2mmdd(year, jjj):
   "Return the month and day for a given year and day-of-year"
   dd = int(jjj)
   for mm in range(1, 13):
      last = num_days_in_month(int(year), mm)

      if dd <= last:
         break

      if mm == 12:
         msg = "Day-of-year value is too large: year = {0}, jjj = {1}"
         raise Exception(msg.format(year, jjj))

      dd -= last

   month = str("%02d"%mm)
   day = str("%02d"%dd)

   return (month, day)       

#.......................................................................
def num_days_in_month(year, month):
   "Return the number of days for a given year/month"
   numdays = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

   if month==2 and year%4==0 and (year%100!=0 or year%400==0):
      numdays[2] = 29

   return numdays[month]

#.......................................................................
def set_global_pattern_dictionary():
   "Create patterns to be used in regexp searches to find date/time."
   global pattern
   pattern = {}

   # regular expression strings
   #---------------------------
   start_stop_string     = r"(\d{8}_\d{2})z-(\d{8}_\d{2})z"

   Ayyyyjjj_hhnn_string  = r"A(\d{4})(\d{3})\D(\d{2})(\d{2})\D"
   yyyymmddhh_string     = r"\D(\d{4})(\d{2})(\d{2})(\d{2})"
   yyyymmdd_hhnnz_string = r"\D(\d{4})(\d{2})(\d{2})\D(\d{2})(\d{2})z"
   yyyymmdd_hhz_string   = r"\D(\d{4})(\d{2})(\d{2})\D(\d{2})z"
   yyyymmdd_hh_string    = r"\D(\d{4})(\d{2})(\d{2})\D(\d{2})"
   yyyymmdd__hhz_string  = r"\D(\d{4})(\d{2})(\d{2})\D+(\d{2})z"
   yymmdd__hhz_string    = r"\D(\d{2})(\d{2})(\d{2})\D+(\d{2})z"
   yyyymmdd_string       = r"\D(\d{4})(\d{2})(\d{2})"
   yyyyjjj_hhnn_string   = r"\D(\d{4})(\d{3})\D(\d{2})(\d{2})"
   hhz_string            = r"\D(\d{2})z"

   Y4_M2_D2_string       = r"/Y(\d{4})/M(\d{2})/D(\d{2})/"
   Y4_M2_string          = r"/Y(\d{4})/M(\d{2})/"
   YYYY_JJJ_string       = r"/(\d{4})/(\d{3})/"


   # store regular expression patterns in global variable
   #-----------------------------------------------------
   pattern["start_stop"]     = compile(start_stop_string)

   pattern["Ayyyyjjj_hhnn"]  = compile(Ayyyyjjj_hhnn_string)
   pattern["yyyymmddhh"]     = compile(yyyymmddhh_string)
   pattern["yyyymmdd_hhnnz"] = compile(yyyymmdd_hhnnz_string)
   pattern["yyyymmdd_hhz"]   = compile(yyyymmdd_hhz_string)
   pattern["yyyymmdd_hh"]    = compile(yyyymmdd_hh_string)
   pattern["yyyymmdd__hhz"]  = compile(yyyymmdd__hhz_string)
   pattern["yymmdd__hhz"]    = compile(yymmdd__hhz_string)
   pattern["yyyymmdd"]       = compile(yyyymmdd_string)
   pattern["yyyyjjj_hhnn"]   = compile(yyyyjjj_hhnn_string)
   pattern["hhz"]            = compile(hhz_string)

   pattern["Y4_M2_D2"]       = compile(Y4_M2_D2_string)
   pattern["Y4_M2"]          = compile(Y4_M2_string)
   pattern["YYYY_JJJ"]       = compile(YYYY_JJJ_string)

#.......................................................................
def stringsplit(strval, char=","): return strval.split(char)

#.......................................................................
if __name__ == "__main__":
   """Check obsys_rc file against available data."""

   # get calling parameters
   #-----------------------
   ArgumentDefaults = ArgumentDefaultsHelpFormatter
   parser = ArgumentParser(description=__doc__,
                            formatter_class=ArgumentDefaults)
   #=========
   # filename
   #=========
   parser.add_argument("filename",
                       nargs="?",
                       type=str,
                       default="obsys.rc",
                       help="name of obsys_rc file")
   #========
   # newfile
   #========
   parser.add_argument("newfile",
                       nargs="?",
                       type=str,
                       default="default",
                       help="name of output new file")
   #========
   # errfile
   #========
   parser.add_argument("errfile",
                       nargs="?",
                       type=str,
                       default="default",
                       help="name of output error file")
   #==========
   # --obslist
   #==========
   parser.add_argument("--obslist",
                       metavar="obsclass_list",
                       type=stringsplit,
                       default="all",
                       help="""list of observation classes to process,
                               separated by commas, no spaces""")
   #==============
   # --ignore_gaps
   #==============
   parser.add_argument("--ignore_gaps",
                       metavar="obsclass_threshold_list",
                       type=stringsplit,
                       default=[],
                       help="""list of obsclass[=threshold] values where data
                               gaps less than threshold (hours) will be ignored;
                               multiple values separated by commas, no spaces;
                               if obsclass equals "all", then apply threshold to
                               all obsclasses; default threshold is 24 (hours)
                               if threshold value is not given.""")
   #==========
   # --lastday
   #==========
   help_msg = """number of days before "today" to stop looking for data""";
   parser.add_argument("--lastday",
                       nargs="?",
                       type= int,
                       default=7,
                       help = help_msg)

   # extract calling parameters
   #---------------------------
   args = parser.parse_args()

   filename    = args.filename
   obslist     = args.obslist
   ignore_gaps = args.ignore_gaps
   lastday     = args.lastday

   if args.newfile == 'filename+".new"': newfile = filename+".new"
   else:                                 newfile = args.newfile

   if args.errfile == 'filename+".err"': errfile = filename+".err"
   else:                                 errfile = args.errfile

   # call check function
   #--------------------
   check(filename, newfile, errfile, obslist, ignore_gaps, lastday)
