import os, re

class Load:

    #.......................................................................
    def __init__(self, filename="obsys.rc"):
        """
        Initialize obsys.rc instance.

        Note:
        self._obsinfo is a list of tuples with the following record format:
        (obsclass, recvals)

        where
        => obsclass: name of observation class
        => recvals: dictionary, {"prolog":prolog, "outtmpl":outtmpl,
                                 "comments":comments, "rows":rows}
           where
           => prolog: list of blank and commented lines above obsclass table
           => outtmpl: name template to use for retrieved data
           => comments: list of commented lines within obsclass table
           => rows: list of strings with the following format:
                "startdate-stopdate interval path_template/name_template"

                where
                => startdate, stopdate format: yyyymmdd_hhz
                => interval: interval time in format, hhmmss
                => path_template: template for data location
                => name_template: template for data name

        template tokens
        => %y4: 4-digit year
        => %y2: 2-digit year
        => %m2: 2-digit month
        => %d2: 2-digit day
        => %h2: 2-digit hour
        """
        if not os.path.isfile(filename):
            raise ValueError("File not found: {0}".format(filename))
        self._filename = filename
        self._obsinfo = []
        self.__read()

    #.......................................................................
    def __read(self):
        """
        Read information from obsys.rc file.
        """
        with open(self._filename, mode='r') as obsys_file:

            # create RE pattern objects
            #--------------------------
            begin_string = r"^BEGIN\s+(\S+)\s*=>\s*(\S+)$"
            begin_pattern = re.compile(begin_string)

            end_string = r"^\s*END\s*$"
            end_pattern = re.compile(end_string)

            start_stop_string = r"(\d{8}_\d{2}z)-(\d{8}_\d{2}z)"
            row_string = start_stop_string + r"\s+(\d{6})\s+(\S+)\s*$"
            row_pattern = re.compile(row_string)

            # loop through lines of file
            #---------------------------
            count = {}
            comments = []
            prolog = []
            rows = []
            read_table = False

            for line in obsys_file:
                line = line.strip()

                # store blank lines and comments
                #-------------------------------
                # Note:
                # - comments: comments within table
                # - prolog: blanks and comments above table
                #------------------------------------------
                if line == "" or line[0] == '#':
                    if read_table:
                        if line != "":
                            comments.append(line)
                    else:
                        prolog.append(line)
                    continue

                # look for beginning of obsclass table
                #-------------------------------------
                if not read_table:
                    if begin_pattern.match(line):
                        (obsclass, outtmpl) = begin_pattern.match(line).groups()

                        if count.has_key(obsclass):
                            msg = "Duplicate obsclass found: {0}"
                            raise ValueError(msg.format(obsclass))
                        else:
                            count[obsclass] = 1
                        read_table = True
                    continue

                # read obsclass table info
                #-------------------------
                if row_pattern.match(line):
                    rows.append(line)
                    continue

                # look for end of obsclass table
                #-------------------------------
                if end_pattern.match(line):
                    recvals= {}
                    recvals["outtmpl"] = outtmpl
                    recvals["comments"] = comments
                    recvals["prolog"] = prolog
                    recvals["rows"] = rows

                    record = (obsclass, recvals)
                    self._obsinfo.append(record)

                    comments = []
                    prolog = []
                    rows = []
                    read_table = False

    #.......................................................................
    def filename(self):
        """
        Return self._filename.
        """
        return self._filename

    #.......................................................................
    def has(self, obsclass):
        """
        Return True if obsclass listed in self._obsinfo; otherwise False.
        """
        return dict(self._obsinfo).has_key(obsclass)

    #.......................................................................
    def obsinfo(self):
        """
        Return self._obsinfo records sequentially.
        """
        for record in self._obsinfo:
            yield record

