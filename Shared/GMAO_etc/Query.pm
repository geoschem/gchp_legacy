package Query;
require 5.000;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(query yes neg blank);
             
#=======================================================================
# name - query
# purpose - query user for a response and return the response
#
# input parameters
# => $str: use this line to prompt for a response
# => $dflt: (optional) default value to use for <cr> response
# 
#=======================================================================
sub query {
    my ($str, $dflt, $prompt, $ans);

    $str  = shift @_;
    $dflt = shift @_;

    # prepare prompt
    #---------------
    $prompt  = "$str ";
    $prompt .= "[$dflt] " unless blank($dflt);

    # get user response
    #------------------
    print $prompt;
    chomp($ans = <STDIN>);
    $ans =~ s/^\s+|\s+$//g;     # remove leading/trailing blanks from response
    $ans = expand_EnvVars($ans);
    if ( blank($ans) ) { $ans = $dflt unless blank($dflt) }

    return $ans;
}

#=======================================================================
# name - expand_EnvVars
# purpose - expand Environment Variables within a string
#=======================================================================
sub expand_EnvVars {
    my ($string, $cnt, $var, $name);

    $string = shift @_;
    $cnt = 0;

    # look for ${var} format
    #-----------------------
    while ($string =~ m/(\${(\w+)})/)   {
        $var = $1; $name = $2;
        $var =~ s/\$/\\\$/;
        $string =~ s/$var/$ENV{$name}/;
        die "Error. Infinite loop condition;" if ++$cnt > 100;
    }

    # look for $var format
    #---------------------
    while ($string =~ m/(\$\b(\w+)\b)/) {
        $var = $1; $name = $2;
        $var =~ s/\$/\\\$/;
        $string =~ s/$var/$ENV{$name}/;
        die "Error. Infinite loop condition;" if ++$cnt > 200;
    }
    return $string;
}

#=======================================================================
sub yes {
    my $str;
    $str = shift @_;
    $str = lc $str;           # make lowercase
    $str =~ s/^\s*|\s*$//g;   # remove leading/trailing blanks
    return 1 if $str eq "y" or $str eq "yes"
}

#=======================================================================
sub neg {
    my $str;
    $str = shift @_;
    $str = lc $str;           # make lowercase
    $str =~ s/^\s*|\s*$//g;   # remove leading/trailing blanks
    return 1 if $str eq "n" or $str eq "no"
}

#=======================================================================
sub blank {
    my $str;
    $str = shift @_;
    return 1 if $str =~ /^\s*$/;
}
1;
