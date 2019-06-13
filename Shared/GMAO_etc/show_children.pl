#!/usr/bin/env perl
#=======================================================================
# name - show_children.pl
# purpose - show children processes of a specified parent process
#
# !Revision History
# 04May2016  Stassi    Initial version
#=======================================================================
use strict;
use warnings;

# global variables
#-----------------
my ($PID, $PPID);


# main program
#-------------
{
    my (@ps_lines, @parts, $pid, $ppid, @children);

    init();
    @ps_lines = `ps -U $ENV{USER} -lf`;
    foreach (@ps_lines) {
        @parts = split;
        $pid = $parts[3];
        $ppid = $parts[4];

        next if $pid eq "PID";
        unless ($pid == $PID) { push @children, $pid if $ppid == $PPID }
    }
    foreach (@children) { print "$_\n" }
}

#=======================================================================
# name - init
# purpose -
#=======================================================================
sub init {
    usage() unless @ARGV;

    $PPID = shift @ARGV;
    usage() unless $PPID =~ m/^\d+$/;

    $PID = $$;
}

#=======================================================================
# name - usage
# purpose - print usage information to STDOUT
#=======================================================================
sub usage {
    use File::Basename;
    my $name = basename($0);
    print <<"EOF";
usage: $name ppid
description: 
This script prints the list of children process IDs
for ppid, where ppid is the parent process ID.

EOF
;
    exit();
}
