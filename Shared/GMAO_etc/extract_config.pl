#!/usr/bin/env perl
#=======================================================================
# name - extract_config.pl
# purpose - Wrapper for extract_config subroutine in Extract_config.pm
#
# !Revision History
# 20110812  TOwens Initial Version 
#=======================================================================
use strict;
use warnings;


# main program
#-------------
{
    use FindBin;
    use lib "$FindBin::Bin";
    use Extract_config;
    my @result;

    usage() unless @ARGV;
    @result = extract_config @ARGV;
    print "@result\n";
}

#=======================================================================
# name - usage
# purpose - print usage information
#=======================================================================
sub usage {

  print <<"EOF";
NAME
     extract_config  - extracts config values from rc files 

SYNOPSIS

   extract_config.pl value rcfile
   where
     value = configuration value name
     rcfile = rcfile containing value


DESCRIPTION
       extracts config values from rc files
       a command line front end to Extract_config.pm
NOTES
     1. RC file must be in format "valuename = value"     
     2. does not respect # as comments 
     3. matches on substrings  - all valuenames must be unique
     4. will keep matching to end of file - returning last match found

EXAMPLES

AUTHOR
    Tommy Owens Tommy.Owens\@nasa.gov 
    based on Extract_config.pm written by Dave Lamich
    and adapted from tick written by Joe Stassi
EOF

exit(1);
}
