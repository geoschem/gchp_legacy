#!/usr/bin/env python
"""Check obsys_rc file against available data."""

import argparse
import glob
import obsys_rc
import re
import time

global zeros
zeros = "00000000_00z"

#.......................................................................
def check(filename="obsys.rc",
          outfile="default",
          obslist=["all"],
          write_comments=True,
          show_miss_data=True,
          show_misfiles=True,
          show_gaps=False,
          show_found_data=True,
          suppress_date=False):
    """
    Check obsys_rc file against available data.

    input parameters
    => filename: name of obsys_rc to check
    => outfile: name of output file; defaults to filename+".check"
    => obslist: list of observation classes to process
    => write_comments: (boolean) write obsys_rc table comments to output
    => show_miss_data: (int string "N" or False) show dates of
                       missing data up to N days before "today"
    => show_misfiles: (boolean) show misfile info
    => show_gaps: (boolean) show all data gaps
    => show_found_data: (boolean) show data found in data gaps
    => suppress_date: (boolean) do not display lastday in output;
                      (for testing purposes only)
    """

    _set_global_pattern_dictionary()
    global pattern

    if outfile == "default":
        outfile = filename + ".check"

    if outfile == filename:
        msg = "Cannot write rebuilt info back to the same file: {0}"
        raise ValueError(msg.format(outfile))

    today = time.strftime("%Y%m%d_00z")
    if show_miss_data:
        try:
            num_hours = int(show_miss_data)*24 * -1
            lastday = incr_datetime(today, num_hours)
        except:
            lastday = today

    # load input obsys_rc file
    #-------------------------
    obsys = obsys_rc.Load(filename)

    # open output file
    #-----------------
    with open(outfile, mode='w') as outfl:

        # loop thru data from obsys_rc file
        #----------------------------------
        for (obsclass, recvals) in obsys.obsinfo():
            if "all" not in obslist and obsclass not in obslist:
                continue

            print("\nChecking {0}".format(obsclass))

            # output prolog lines, table start, and comments
            #-----------------------------------------------
            if recvals["prolog"]:
                for line in recvals["prolog"]:
                    outfl.write(line+'\n')

            outfl.write("BEGIN {0} => {1}\n".format(obsclass, recvals["outtmpl"]))
            if write_comments:
                for line in recvals["comments"]:
                    outfl.write(line+"\n")

            # sort obsclass rows by template
            #----------------------------------------------
            # NOTE: an obsclass can have multiple templates
            #----------------------------------------------
            template_start_stop_list = []
            for row in recvals["rows"]:
                (start_stop, interval, template) = row.split()
                (start, stop) = pattern["start_stop"].search(start_stop).groups()

                if dict(template_start_stop_list).has_key(template):
                    dict(template_start_stop_list)[template].append((start, stop))
                else:
                    template_start_stop_list.append((template, [(start, stop)]))

            # process and output rows by template
            #------------------------------------
            for (template, start_stop_list) in template_start_stop_list:
                print("=> {0}".format(template))

                # get and output available data info
                #-----------------------------------
                (start_stop_data, delta, misfiled_list) = _get_data_info(template)

                for (start, stop) in start_stop_list:
                    line = "  %s-%s %s %s\n"%(start, stop, interval, template)

                    if start == zeros:
                        line[0] = '#'
                    outfl.write(line)

                # look for gap information
                #-------------------------
                first = min(start_stop_list[0][0],  start_stop_data[0][0])
                final = max(start_stop_list[-1][1], start_stop_data[-1][1])

                if final > today:
                    final = today

                if final < first:
                    msg = "first datetime is greater than final: {0} > {1}"
                    raise ValueError(msg.format(first, final))

                gaps = []
                miss_data = []
                found_data = []

                datetime = first
                while datetime <= final:
                    if _not_included(start_stop_list, datetime):

                        if _not_included(start_stop_data, datetime):
                            gaps.append(datetime)
                        else:
                            found_data.append(datetime)

                    elif _not_included(start_stop_data, datetime):
                        miss_data.append(datetime)
                        gaps.append(datetime)

                    datetime = incr_datetime(datetime, delta)
                    
                border1 = "  #"+"-"*26+"\n"
                border2 = "  #"+"="*26+"\n"

                # write miss data info to output
                #-------------------------------
                if show_miss_data and miss_data:

                    msg = "  # MISSING DATA"
                    if not suppress_date:
                        msg += "\n  # start  < %s"%(lastday)
                        msg += "\n  # or end < %s"%(today)
                    msg += "\n"
                    miss_tuples = _start_stop_tuples(miss_data, delta)

                    wrote_info = False
                    for (start, stop) in miss_tuples:

                        if stop != today or start < lastday:
                            if not wrote_info:
                                outfl.write(border2+msg+border1)
                                wrote_info = True

                            if start == stop:
                                outfl.write("  %s\n"%(start))
                            else:
                                outfl.write("  %s-%s\n"%(start, stop))

                    if wrote_info:
                        outfl.write(border1+'\n')

                # write all gap info to output
                #-----------------------------
                if show_gaps and gaps:
                    msg =  "  # all data gaps\n"
                    msg += "  # including MISSING DATA\n"
                    outfl.write(border2+msg+border1)

                    gap_tuples = _start_stop_tuples(gaps, delta)
                    for (start, stop) in gap_tuples:
                        if start == stop:
                            outfl.write("  %s\n"%(start))
                        else:
                            outfl.write("  %s-%s\n"%(start, stop))
                    outfl.write(border1+'\n')

                # write found data info to output
                #--------------------------------
                if show_found_data and found_data:
                    msg = "  # MORE DATA FOUND\n"
                    outfl.write(border2+msg+border1)

                    nogap_tuples = _start_stop_tuples(found_data, delta)
                    for (start, stop) in nogap_tuples:
                        if start == stop:
                            outfl.write("  %s\n"%(start))
                        else:
                            outfl.write("  %s-%s\n"%(start, stop))
                    outfl.write(border1+'\n')

                # write misfiled info to output
                #------------------------------
                if show_misfiles and misfiled_list:
                    msg  = "  # MISFILES\n"
                    outfl.write(border2+msg+border1)
                    misfiled_list.sort()
                    for mis in misfiled_list:
                        outfl.write("  #"+mis+"\n")
                    outfl.write(border1+'\n')

            # output table end
            #-----------------
            outfl.write("END\n")
            outfl.flush()

#.......................................................................
def _csplit(strval, char=','): return strval.split(char)

#.......................................................................
def _get_data_info(template):
    """
    Return list of (start, stop) datetime tuples,
    plus delta value and list of misplaced and misnamed data.

    input parameters
    => template: data path/name template
    """
    global zeros

    # regular expression patterns to find date/time
    #----------------------------------------------
    global pattern

    # get list of data filepaths
    #---------------------------
    index = template.find(':')+1
    tmpl = template[index:]
    tmpl = tmpl.replace("%y4", "????").replace("%y2", "??").replace("%m2", "??")
    tmpl = tmpl.replace("%d2", "??").replace("%h2", "??")

    filepath_list = []
    filepath_list = glob.glob(tmpl)

    # extract datetimes from available data
    #--------------------------------------
    hours_found = {}
    datetime_list = []
    misfiled_list = []

    if filepath_list:
        for fpath in filepath_list:

            if pattern["yyyymmdd_hhz"].search(fpath):
                returnVals = pattern["yyyymmdd_hhz"].search(fpath).groups()
                (year, month, day, hour) = returnVals

                if pattern["Y4_M2"].search(fpath):
                    (yyyy, mm) = pattern["Y4_M2"].search(fpath).groups()

                    if year != yyyy or month != mm:
                        misfiled_list.append("(MISPLACED) "+fpath)
                        continue

            elif pattern["yyyymmdd_hh"].search(fpath):
                returnVals = pattern["yyyymmdd_hh"].search(fpath).groups()
                (year, month, day, hour) = returnVals
                if pattern["Y4_M2"].search(fpath):
                    (yyyy, mm) = pattern["Y4_M2"].search(fpath).groups()

                    if year != yyyy or month != mm:
                        misfiled_list.append("(MISPLACED) "+fpath)
                        continue
 
            elif pattern["Y4_M2"].search(fpath) and pattern["yyyymmdd__hhz"].search(fpath):
                (year, month) = pattern["Y4_M2"].search(fpath).groups()
                (yyyy, mm, day, hour) = pattern["yyyymmdd__hhz"].search(fpath).groups()

                if year != yyyy or month != mm:
                    misfiled_list.append("(MISPLACED) "+fpath)
                    continue

            elif pattern["Y4_M2"].search(fpath) and pattern["yymmdd__hhz"].search(fpath):
                (year, month) = pattern["Y4_M2"].search(fpath).groups()
                (yy, mm, day, hour) = pattern["yymmdd__hhz"].search(fpath).groups()

                if year[2:4] != yy or month != mm:
                    misfiled_list.append("(MISPLACED) "+fpath)
                    continue

            elif pattern["Y4_M2_D2"].search(fpath) and pattern["hhz"].search(fpath):
                (year, month, day) = pattern["Y4_M2_D2"].search(fpath,1).groups()
                (hour,) = pattern["hhz"].search(fpath).groups()

            elif pattern["Y4_M2"].search(fpath):
                (year, month) = pattern["Y4_M2"].search(fpath).groups()

                ddhh_string = year+month+r"(\d{2})(\d{2})"
                ddhh_pattern = re.compile(ddhh_string)
                (day, hour) = ddhh_pattern.search(fpath).groups()

            else:
                msg = "Cannot extract date/time from fpath: {0}"
                raise ValueError(msg.format(fpath))

            numdays = num_days_in_month(int(year), int(month))
            if int(month) < 1 or int(month) > 12 or \
                    int(day) < 1 or int(day) > numdays:
                misfiled_list.append("(MISLABELED) "+fpath)
                continue
                    
            datetime = year+month+day+'_'+hour+'z'
            datetime_list.append(datetime)
            hours_found[hour] = 1

    # determine data delta
    #---------------------
    if hours_found:
        delta = 24/len(hours_found.keys())
    
        if delta != int(delta):
            msg = "Non-divisible number of hours found: {0} for {1]"
            raise ValueError(msg.format(hours, template))
    else:
        datetime_list = [zeros]
        delta = 24

    start_stop_data = _start_stop_tuples(datetime_list, delta)
    return (start_stop_data, delta, misfiled_list)

#.......................................................................
def _start_stop_tuples(datetime_list, delta):
    datetime_list.sort()

    start = datetime_list[0]
    previous = start
    next = incr_datetime(start, delta)

    tuple_list = []
    for datetime in datetime_list[1:]:
        if datetime != next:
            tuple_list.append((start, previous))
            start = datetime

        previous = datetime
        next = incr_datetime(datetime, delta)

    tuple_list.append((start, previous))

    return tuple_list

#.......................................................................
def _not_included(date_ranges, datetime):
    """Return False if datetime is not in any of the included date_ranges"""
    for (start, stop) in date_ranges:
        if datetime >= start and datetime <= stop:
            return False
    return True

#.......................................................................
def _set_global_pattern_dictionary():
    """Create patterns to be used in regexp searches to find date/time."""
    global pattern
    pattern = {}

    # regular expression strings
    #---------------------------
    start_stop_string    = r"(\d{8}_\d{2}z)-(\d{8}_\d{2}z)"
    Y4_M2_string         = r"Y(\d{4})/M(\d{2})\D"
    Y4_M2_D2_string      = r"Y(\d{4})/M(\d{2})/D(\d{2})\D"
    hhz_string           = r"\D(\d{2})z"
    yymmdd__hhz_string   = r"\D(\d{2})(\d{2})(\d{2})\D+(\d{2})z"
    yyyymmdd__hhz_string = r"\D(\d{4})(\d{2})(\d{2})\D+(\d{2})z"
    yyyymmdd_hh_string   = r"(\d{4})(\d{2})(\d{2})\D(\d{2})"
    yyyymmdd_hhz_string  = r"(\d{4})(\d{2})(\d{2})\D(\d{2})z"

    # store regular expression patterns in global variable
    #-----------------------------------------------------
    pattern["start_stop"]    = re.compile(start_stop_string)
    pattern["Y4_M2"]         = re.compile(Y4_M2_string)
    pattern["Y4_M2_D2"]      = re.compile(Y4_M2_D2_string)
    pattern["hhz"]           = re.compile(hhz_string)
    pattern["yymmdd__hhz"]   = re.compile(yymmdd__hhz_string)
    pattern["yyyymmdd__hhz"] = re.compile(yyyymmdd__hhz_string)
    pattern["yyyymmdd_hh"]   = re.compile(yyyymmdd_hh_string)
    pattern["yyyymmdd_hhz"]  = re.compile(yyyymmdd_hhz_string)

#.......................................................................
def incr_datetime(datetime, delta_hours):
    """
    Increment datetime by delta_hours and return the value.

    input parameters
    => datetime: date/time in yyyymmdd_hhz format
    => delta_hours: integer number of hours to add to datetime

    return value
    => new_datetime: format yyyymmdd_hhz
    """
    year  = int(datetime[0:4])
    month = int(datetime[4:6])
    day   = int(datetime[6:8])
    try:
        hour = int(datetime[9:11])
    except ValueError:
        print("EXCEPTION: datetime = {0}".format(datetime))

    hour += int(delta_hours)
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

    return "%04d%02d%02d_%02dz"%(year, month, day, hour)

#.......................................................................
def num_days_in_month(year, month):
    numdays = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    if month==2 and year%4==0 and (year%100!=0 or year%400==0):
        numdays[2] = 29

    return numdays[month]

#.......................................................................
if __name__ == "__main__":
    """Check obsys_rc file against available data."""

    # get calling parameters
    #-----------------------
    ArgumentDefaults = argparse.ArgumentDefaultsHelpFormatter
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=ArgumentDefaults)
    #=========
    # filename
    #=========
    parser.add_argument("filename",
                        nargs='?',
                        type=str,
                        default="obsys.rc",
                        help="name of obsys_rc file")
    #========
    # outfile
    #========
    parser.add_argument("outfile",
                        nargs='?',
                        type=str,
                        default='filename+".check"',
                        help='name of output file')
    #==========
    # --obslist
    #==========
    parser.add_argument("--obslist",
                        metavar="clA[,clB..]",
                        type=_csplit,
                        default="all",
                        help="""list of observation classes to process,
                                separated by commas, no spaces""")
    #===========
    # --comments
    #===========
    help_msg = "write obsys_rc table comments to output; 0 to turn off"
    parser.add_argument("--comments",
                        nargs='?',
                        metavar="0",
                        action="store",
                        type = int,
                        const=True,       # for --comments flag without value
                        default=True,     # for when --comments flag is missing
                        help = help_msg)
    #=======
    # --miss
    #=======
    help_msg = """show dates of missing data up to N days before "today";
                  "False" to turn off"""
    days_before_today = 7
    parser.add_argument("--miss",
                        nargs='?',
                        metavar="N or False",
                        const=days_before_today,
                        default=days_before_today,
                        help = help_msg)
    #===========
    # --misfiles
    #===========
    parser.add_argument("--misfiles",
                        nargs='?',
                        metavar="0",
                        type=int,
                        const=True,
                        default=True,
                        help = "show misfile information; 0 to turn off")
    #=======
    # --gaps
    #=======
    parser.add_argument("--gaps",
                        action="store_true",
                        default=False,
                        help = "show data gaps")
    #========
    # --found
    #========
    parser.add_argument("--found",
                        nargs='?',
                        metavar="0",
                        type=int,
                        const=True,
                        default=True,
                        help = "show data found in the gaps; 0 to turn off")
    #================
    # --suppress_date
    #================
    help_msg = "Do not show dates in MISSING DATA title (testing only)"
    parser.add_argument("--suppress_date",
                        action="store_true",
                        default=False,
                        help=help_msg)

    # extract calling parameters
    #---------------------------
    args = parser.parse_args()

    filename        = args.filename
    obslist         = args.obslist
    show_found_data = args.found
    show_misfiles   = args.misfiles
    write_comments  = args.comments
    suppress_date   = args.suppress_date

    if args.outfile == 'filename+".check"': outfile = filename+".check"
    else:                                   outfile = args.outfile

    if args.gaps: show_gaps = True
    else:         show_gaps = False

    if args.miss:
        show_miss_data = args.miss
        if show_miss_data == "False":
            show_miss_data = False
        else:
            try:
                dummy = int(show_miss_data)
            except:
                show_miss_data = days_before_today

    # call check function
    #--------------------
    check(filename, outfile, obslist, write_comments,
          show_miss_data, show_misfiles, show_gaps, show_found_data,
          suppress_date)
