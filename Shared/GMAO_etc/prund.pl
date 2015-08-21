#!/usr/bin/env perl
#
# Simple daemon (and client) for controlling parallel execution of
# single processor jobs. Really simple, see usage for details. 
#
#------------------------------------------------------------------------

use Env;                 # make env vars readily available
use Getopt::Std;         # command line options
use IO::Socket;          # Sockets
use File::Basename

# Command line options
# --------------------
  getopts('dhvH:P:L:');
  usage() if ( $opt_h || $#ARGV < 0 );

# Defaults
# --------
  $localhost = 'localhost' unless ( $localhost = $opt_H );
  $localport = 1200        unless ( $localport = $opt_P );
  $listen    = 512         unless ( $listen    = $opt_L );

# Remove '.' from end of $localhost
# ---------------------------------
  $localhost =~ s/\.*$//;

#                     -----------
#                     Server mode
#                     -----------

  Server() if ( $opt_d );

#                     -----------
#                     Client mode
#                     -----------

# Command template
# ----------------
  $_ = "@ARGV";
  if ( /%s/ ) { $fmt = "$_";    } 
  else          { $fmt = "$_ %s"; }

# While file names are available...
# ---------------------------------
  print "Starting CLIENT on host $HOSTNAME ...\n" if ( $opt_v );
  while ( $filen = Client("<next>") ) {
   $cmd = $fmt;
   $cmd =~ s|%s|$filen|g;
   print "Running: $cmd\n";
   $rc = system("$cmd"); # for now, does not stop on errors
   Client("<done>");
  }

# Hang in there while others are still working...
# -----------------------------------------------
  while ( Client("<NotAllDone>") ) {
      sleep(3); 
  }

  print "Finishing CLIENT on host $HOSTNAME ...\n" if ( $opt_v );

exit(0);

#...................................................................

sub Server {

    print "Starting SERVER on host $HOSTNAME \n" if ( $opt_v );

    my $sock = new IO::Socket::INET ( LocalHost => $localhost,
				   LocalPort => $localport,
				   Proto     => 'tcp',
				   Listen    => $listen,
				   Reuse     => 1
				   );
    die "     SERVER: socket could not be created. Reason: $!" unless $sock;
    
    $nFiles         = $#ARGV + 1; 
    $nFiles_NotDone = $nFiles;     # Files not yet finished

    print "     SERVER: socket created, listening...\n" if ( $opt_v );

    while ( $new_sock = $sock->accept()) {

        chomp ($question = <$new_sock>);

	print 
          "     SERVER: received $question, sending ... "
              if ( $opt_v );


        if ( "$question" eq "<next>" ) {
             $nFiles_NotServed = $#ARGV + 1;
	     if ( $nFiles_NotServed > 0 ) {
		 $answer = $ARGV[0];
		 shift @ARGV;
             } else {
                 $answer = undef;
	     }
	 } elsif ( "$question" eq "<done>" ) {
	     $nFiles_NotDone--;
             $answer = "ok";
         } else {
             $answer = $nFiles_NotDone;
         }

	print "<$answer>\n" if ( $opt_v );

	print $new_sock "$answer\n";
	$new_sock->flush();
	close $new_sock;

#       Exit if all done
#       ----------------
        if ( $nFiles_NotDone < 1 ) {
	    print "Finishing SERVER on host $HOSTNAME \n" if ( $opt_v );
	    exit(0);
        } 

    }
}

#....................................................................

sub Client {
    my ( $question ) = @_;
    my $sock = new IO::Socket::INET ( PeerAddr => $localhost,
				      PeerPort => $localport,
				      Proto     => 'tcp'
				    );
    return undef unless $sock;

    print "     CLIENT: sent for $question, received " if ( $opt_v );
    print $sock "$question\n"; 
    $sock->flush();
    chomp($answer = <$sock>);
    close $sock;
    print "<$answer>\n" if ( $opt_v );
    return $answer;


}

sub usage {

   print <<"EOF";

#......................................................................

NAME
     prund - Client/Server daemon for parallel execution single PE jobs
          
SYNOPSIS

     prund.pl [-v] [-H host] -d  file_names 
     prund.pl [-v] [-H host]     program
          
DESCRIPTION

     With the -d option, it starts the server; one must specify a list
     of file names (tokens) to be provided to each client. In this case it 
     binds a socket to an address and waits for an incoming connection 
     request. When that happens, it sends the next file name on the list.

     Without the -d option it starts in "client mode" where it runs
     "program <filename>" for as long as the server provides a file
     name. The string "program" can contain "%s" in the middle; in
     this case "%s" is expanded with the file name. See EXAMPLES.

OPTIONS

     -d        daemon (server) mode; default is client
     -h        prints this usage notice
     -H host   specify hostname for connection; default is localhost
     -L n      maximum number of clients to be put on hold; default: 512
     -P port   port number; default is 1200
     -v        verbose mode, good for debuging

EXAMPLES

     % prund.pl -H $HOSTNAME -d a b c d e f g h i j k l &
     % prund.pl -H $HOSTNAME echo Got %s as the file name

     % prund.pl -H $HOSTNAME -d a b c d e f g h i j k l &
     % mpirun -np 8 prund.pl -H $HOSTNAME /bin/ls

     Note: "/bin/ls" is equivalent to "/bin/ls %s"

AUTHOR
     arlindo.dasilva@nasa.gov

EOF

  exit(1)

 }


