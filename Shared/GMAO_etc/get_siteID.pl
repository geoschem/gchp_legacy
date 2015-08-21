#!/usr/bin/env perl
#=======================================================================
# name - get_siteID.pl
# purpose - wrapper script for sub get_siteID() in GMAO_utils.pm package
#=======================================================================
use strict;
use warnings;
use FindBin;
use lib ("$FindBin::Bin");
use GMAO_utils ("get_siteID");

my $siteID;
$siteID = get_siteID();
print "$siteID\n";
