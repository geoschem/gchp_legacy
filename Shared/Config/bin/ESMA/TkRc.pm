package TkRc;
#
#  This Class implements a Perl interface to a Tk GUI for Resource files. 
#  It is created from a list of RC objects which have possibly registered
#  "critique" methods. It can also be created from a list of files.
#
#---
#
#  This program is free software; you can redistribute it and/or modify it 
#  under the same terms as Perl itself.
#
#  THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
#  WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES
#  OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE
#
#  REVISION HISTORY:
#
#  23May2007 da Silva  Initial implementation based on code fragments
#                       from the original "red" script.
#
#--------------------------------------------------------------------------

require 5.000;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(new dump write);

#.........................................................................

 use FileHandle;
 use Carp;
 use Env;                 # Environment variables
 use ESMA::Rc;            # Resource file class

 use Getopt::Std;         # command line options
 use Tk;                  # Tk GUI
 use Tk::DirTree;         # Tk Directory
 use FindBin;             # so we can find where this script resides
 use File::Path;          # for mkpath()
 use File::Copy "cp";     # for cp()
 use File::Basename;      # for basename(), dirname()
 use MIME::Base64 ();	  # for MIME base 64 nasa logo

 $VERSION = 1.00;

# The public RC object
# --------------------
my %tkrc = {

    Files  => [],     # List of file names
    Rcs    => [],     # List of already parsed resources
    debug  => undef,  # print things as it goes along; mostly for debugging
};

# Class scope data, for now
# -------------------------
  $ROOTDIR  = "";

# $rightwidget_width = 1020;
  $textentrywidth   = 18;
  $textqcolor 	    = "#FEFEFE";
  $textentrycolor   = "#EFEFDF";
  $textitemcolor 	  = "#DFCFCF";
  $textframecolor   = "#FFFEF0";

  $titlecolor  	    = "#DCF1dF";
  $radioqcolor 	    = "#FEFEFE";

# $radioframecolor  = "#FFEEEE";
  $radioframecolor  = "#FFFEF0";
  $radioitemcolor   = "#DFCFCF";
  $radiobuttoncolor = "#EFEFDF";

  $checkframecolor  = "#FFFEF0";
  $checkbuttoncolor = "#EFEFDF";
  $checkbackground  = "#FEFEFE";

  $filebrowsecolor  = "#FEFEFE";
  $filebrowseframe  = "#FFEEEE";
  $filelabelcolor   = "#FEFEFE";
  $fileframecolor	  = "#FFFEF0";
  $fileentrycolor   = "#EFEFDF";

  $dirbrowsecolor   = "#FEFEFE";
  $dirbrowseframe   = "#FFEEEE";
  $dirlabelcolor    = "#FEFEFE";
  $dirframecolor    = "#FFFEF0";
  $direntrycolor    = "#EFEFDF";

  $buttoncolor 	    = "#EFE4CF";
  $buttonframecolor = "#EEEEF1";

  $mainframecolor   = "#E1DAE0";

  $logo = ___LoadLogo();

#.........................................................................

sub new {                                                 # Constructor

    my $that = shift;
    my $class = ref($that) || $that;

    my ( $argv ) = @_;

    my $Files = $argv->{Files};
    my $Rcs   = $argv->{Rcs};
    my $debug = $argv->{debug};

#   If file list specified, load those files
#   ----------------------------------------
    $Rcs = ___ReadFiles($Files) if ( $Files );

    print "After ReadFiles\n";

#   Record input args
#   -----------------
    $tkrc{Files} = $Files; # list of files
    $tkrc{Rcs}   = $Rcs;   # list of RC objects
    $tkrc{debug} = $debug;

    print "Before blessing\n";

#   Bless it
#   --------
    $self = { %tkrc, };
    bless $self, $class;

    print "Done with new TkRc\n";
   
    return $self;

}

#..........................................................................

sub DESTROY {                                               # Destructor

   my $self = shift;

   print "Destroyed TkRC object\n"
         if ( $self->{debug} );

   $self->{Files} = undef;
   $self->{Rcs}   = undef;
   $self->{debug} = undef;
   
}

#..........................................................................

sub MainLoop {                         # Enter Tk Main Loop, no returns

    $main = MainWindow -> new;
    my $name, $default, $value, $widget, $args;

#   Setup widgets
#   -------------
  print "before new MainGUI\n";
    ___MainGUI($RCfilename[0]);
  print "before Create Widget\n";
    ___CreateWidget($RCfilename[0]);

#   Go for it
#   ---------
  print "before MainLoop\n";
    MainLoop;  # there will no return from here

}

#..........................................................................

sub ___ReadFiles { 
    my $Files = shift;
    my $rc = undef;
    my @Rcs = ();
    foreach $file ( @{$Files} ) {
       $rc = new Rc { file=>"$file"} or
	   confess "Cannot load RC from file $file";
       push @Rcs, $rc; 
   }
    return \@Rcs;
}

#..........................................................................

sub ___MainGUI {
  my ($file) = @_;

  print "create_mainGUI\n" if ($debug);

  $main -> title("RED - Resource File EDitor");
  $filemenuframe = $main -> Frame(-height=>35);  

  Tk_FileMenu($file);

  $filemenuframe -> pack(-side=>'top',-anchor=>'w');
  $submain   = $main 	-> Frame(-relief => 'ridge', 
  		-borderwidth=>2, -background => '#FFAAAA', );
  $leftframe = $submain -> Frame(-relief => 'ridge',
  		-background => '#FFDDDD',-width=>100);		
		
  Tk_LeftWidget($file);
  
  $leftframe 	-> pack (	-side=>'left', -fill => 'y', 
  				-anchor=>'w', -padx => 1, -pady => 1); 
  $rightframe = $submain -> Frame(-relief => 'ridge', -width=>800,
  		-borderwidth=>1, 
		-background => '#FFAAAA',);
  
  Tk_RightWidget($file);  

  $rightframe 	-> pack(-anchor => 'nw',
  			-side   => 'top',
  			-fill   => 'both',  
			-expand => 1,
			-padx 	=> 1, 
			-pady 	=> 0 	
			);
					
  $submain 	-> pack(-anchor => 'nw',
  			-side   => 'top',
  			-fill 	=> 'both',  
			-expand => 1,	
  			-padx 	=> 1, 
			-pady 	=> 1,
						
			);

 $buttonframe =$main-> Frame(
 			-background=> $buttonframecolor, 
			-relief => 'groove',
			-height => 40
		);
 $buttonframe -> pack(	
 			-side =>'bottom',
 			-anchor=>'s',
 			-padx => 2, 
			-pady => 3,
			-fill =>'both',
			-expand =>1
		);
 

}

#..........................................................................

sub ___CreateWidget {                 # Create Tk Widgets
    my ($file) = @_;
    print "createwidget: file = $file \n" if ($debug);
    print "\n\n Editing ",$RCtitle{$file},"\n" if ($nUI{$file} > 0  && $debug);

    Tk_TextEntry(-1,"OUTPUT FILE NAME", $outfn{$file}, $file);

for ( my $i=0; $i < $nUI{$file}; $i++ ) {

    $name    = $UI[$i]{'name'  }{$file};
    $default = $UI[$i]{'value' }{$file};
    $widget  = $UI[$i]{'widget'}{$file};
    $args    = $UI[$i]{'args'  }{$file};
    $value = $default;


    for ( $widget ) {

SWITCH: {

    /^\&Label/        && do {
                             Tk_Label($args);
                             print "TK:$args\n" if ($debug);
                             last SWITCH;
                      };

    /^\&Str/          && do {
                             $value = $default;
			     Tk_TextEntry($i,$args, $default, $file);
                             print "TK:$args [$default]\n" if ($debug);
                             #chomp($ans = <STDIN> ); 
                             #$ans and $value = $ans;
                             #print "";  # cosmetic
                             last SWITCH;
                      };

    /^\&Dir/          && do {
                             $default =~ s/[ ]*//g; 
                             $value = $default;
                             print "TK:$args [$default]\n" if ($debug);
                             Tk_Dir($i,$args,$default, $file);
			     last SWITCH;
                      };

    /^\&File/         && do {
                             $default =~ s/[ ]*//g; 
                             $value = $default;
                             print "TK:$args [$default]\n" if ($debug);
			     Tk_File($i,$args,$default, $file);
                             last SWITCH;
                      };

    /^\&YesNo/        && do {
                             $default =~ s/[ ]*//g; 
                             $value = $default;
			     Tk_YesNo($i, $args, $default, $file);
                             last SWITCH;
                      };

    /^\&CheckBox/     && do {
                             $default =~ s/[ ]*//g;
			     Tk_CheckBox($i, $args, $file);
                             last SWITCH;
                      };
    }} # SWITCH
    # updated resource file
  } # for each UI request
  
  Tk_Buttons($file);
  my $y = ($nUI{$file}) * $elementfontsize * 2 + 140;
  my $x = 555;
  my $geometry = $x . "x" . $y;
  print "geometry = $geometry" if ($debug);
  $main->geometry($geometry);
} 

#..........................................................................

sub Tk_DirBrowse {
	my ($i,$text, $path, $file) = @_;
	#my ($direntry);

   if ($path =~ /^\$/gi) { 	
      $evalline = $path;
      eval "\$path = \"$evalline\"" ;   # expand environmnet
   } 

	$ok = 0; # flag: "1" means OK, "-1" means cancelled
	$newtoplevel -> destroy() if (Exists $newtoplevel); 
       	$newtoplevel = MainWindow -> new(-relief=>'ridge',
        	-borderwidth=> 5, -background=>$filebrowseframe); 
	$newtoplevel -> Label ( -background => $filelabelcolor, 
		-text => $text, -font=> "Arial 16 bold",
	)-> pack(-side => 'top', -padx => 6, -pady=>1 );
        $dirtree = $newtoplevel -> Scrolled('DirTree',
                 -background=> $filebrowsecolor,
                 -scrollbars => 'osoe'  , -width => 44, -height => 10,
                 -selectmode => 'browse', -exportselection => 1,
                 -browsecmd  => sub {
                     chomp($path); 
                     #print "dirtree: var $var , value $path\n";
                     #$UI[$i]{'value'}{$file} = $path; 
                     $path = shift;
                     $path =~ s/\/\//\//g;
                     #print "path = $path \n";
	             $direntry{$i}-> delete(0, 'end');
                     $direntry{$i}-> insert(0, "$path");

                  },
                 # With this version of -command a double-click will
                 # select the directory
                 #-command   => sub { $ok = 1 },

                 # With this version of -command a double-click will
                 # open a directory. Selection is only possible with
                 # the Ok button.
                 #-command   => sub { $d->opencmd($_[0]) },
                 -command    => sub { $newtoplevel -> destroy(); },
          )->pack(-side => 'top', -fill => "both", -expand => "both");
       # Set the initial directory
       if (-d $path) { $dirtree -> chdir($path); 
                       $dirtree -> update(); 
       } else { 
            $newtoplevel -> Label(-text => "$path does not exist"); 
            #print "path $path does not exist\n"; 
            $dirtree -> chdir( dirname($path) ) if (Exists $dirtree);
       }
       $newtoplevel -> Button(-text => "Apply", 
              -command => sub { 
		     $path = $direntry{$i} -> get(); 
#      	             $path = shift;
                     $path =~ s/\/\//\//g;
		     chomp($path); 
                     #print "dirtree: var $var , value $path\n";
                     #$UI[$i]{'value'}{$file} = $path; 
                     

                     print "path = $path \n";
                     $UI[$i]{'value'}{$file} = $path; 
                     $newtoplevel -> destroy(); 
	      }
        )-> pack (-side=>'top', -padx=>6, -pady=>1);
       $newtoplevel -> update();
       #$newtoplevel->geometry("310x300+$xpos+$ypos");
       #$newtoplevel->positionfrom("program");
       $newtoplevel->title("RED DIR Browser GUI");

  return 0;
}

#..........................................................................

sub Tk_Dir{ 
   my ($i,$question,$default, $file) = @_;

   my $dirframe = $rightwidget -> Frame(-background=>$dirframecolor,-height=>35);

   $width = 20 if (! defined $width);
   my $leftframe = $dirframe -> Frame(-background=>$dirframecolor);
   my $rightframe = $dirframe -> Frame(-background=>$dirframecolor);

   $leftframe -> Label (-background => $dirlabelcolor, -text => $question,-font=>$elementfont,
         )-> pack(-side => 'left',-padx => 4, -pady => 1);
   $rightframe -> Button(-text => "Browse", 
       	-command => sub { &Tk_DirBrowse($i,$question,$default,$file); } 
	 )-> pack(-side => 'left',-padx=>3);
   $direntry{$i}  = $rightframe -> Entry(-background=> $direntrycolor,-width=> $textentrywidth )-> pack(-side => 'right');
   $direntry{$i}  -> insert(0,"$default");
   $leftframe -> pack(-side => 'left', -anchor=> 'w', -padx => 2, -pady => 1);
   $rightframe-> pack(-side => 'right',-anchor=> 'e', -padx => 3, -pady => 1);
   $dirframe -> pack(-side => 'top',  -padx => 2, -pady => 1, -fill=>'both');

# Set the initial directory
#   if (! -e $path)  { ("$path does not exist"); }#
#
   $direntry{$i} -> bind('<KeyPress>', sub { 
	print "direntry: $checkbox " if ($debug); 
	$path = $direntry{$i} -> get(); 
	chomp($path);  print "dirtree: var $var , value $path\n" if ($debug);
	$UI[$i]{'value'}{$file} = $path; 
       	}
   );
   return 0;
}
sub Tk_Dir2{ 
   my ($i,$question,$default, $file) = @_;
   my $dirframe = $rightwidget -> Frame(-background=>$dirframecolor, -height=>30);
   my ($direntry);
   $width = 20 if (! defined $width);
   my $leftframe = $dirframe -> Frame(-background=>$dirframecolor);
   my $rightframe = $dirframe -> Frame(-background=>$dirframecolor);
   $leftframe -> Label (-background => $dirlabelcolor, -font=>$elementfont,
                        -text => $question
         )-> pack(-side => 'left',-padx => 4, -pady => 0);
	 
   
   $rightframe -> Button(-text => "Browse",
       	-command => sub { 
	       $path = $rightframe -> getOpenFile(
 			-defaultextension => "",
		     # -filetypes        =>
		     # [ ['All Files',        '*',             ],
		     # 	['Perl Scripts',     '.pl'            ],
  	       	#	['Text Files',       ['.txt', '.text']],
		#       	['C Source Files',   '.c',      'TEXT'],
		#       	['GIF Files',        '.gif',          ],
		#       	['GIF Files',        '',        'GIFF'],
		#      ],
		      -initialdir       => $default,
		     # -initialfile      => "$default",
		      -title            => "$question",
		      );
		chomp($path);
		$UI[$i]{'value'}{$file} = $path;
		$direntry-> delete(0, 'end');
                $direntry-> insert(0, "$path");
	} 
	 )-> pack(-side => 'left',-padx=>3);
   $direntry  = $rightframe -> Entry(
   				-background=> $fileentrycolor,
   				-width=> $textentrywidth 
			)-> pack(-side => 'right');
   $direntry  -> insert(0,"$default");
   $leftframe -> pack(-side => 'left', -anchor=> 'w', -padx => 2, -pady => 0);
   $rightframe-> pack(-side => 'right',-anchor=> 'e', -padx => 3, -pady => 0);
   $dirframe -> pack(-side => 'top',  -padx => 2, -pady => 0, -fill=>'both');

   # Set the initial directory
#   if (! -e $path)  { ("$path does not exist"); }#

   $direntry -> bind('<KeyPress>', sub { 
	print "direntry: $checkbox " if ($debug); 
	$path = $direntry -> get(); 
	chomp($path);  print "dirtree: var $var , value $path\n" if ($debug);
	$UI[$i]{'value'}{$file} = $path; 
       	}
   );
   return 0;
}

#..........................................................................
sub Tk_File{ 
   my ($i,$question,$default, $file) = @_;
   my $fileframe = $rightwidget -> Frame(-background=>$fileframecolor, -height=>30);
   my ($direntry);
   $width = 20 if (! defined $width);
   my $leftframe = $fileframe -> Frame(-background=>$fileframecolor);
   my $rightframe = $fileframe -> Frame(-background=>$fileframecolor);
   $leftframe -> Label (-background => $filelabelcolor, -font=>$elementfont,
                        -text => $question
         )-> pack(-side => 'left',-padx => 4, -pady => 0);

   $rightframe -> Button(-text => "Browse",
       	-command => sub { 
	       $path = $rightframe -> getOpenFile(
 			-defaultextension => "",
		      -filetypes        =>
		      [ ['All Files',        '*',             ],
		      	['Perl Scripts',     '.pl'            ],
  	       		['Text Files',       ['.txt', '.text']],
		       	['C Source Files',   '.c',      'TEXT'],
		       	['GIF Files',        '.gif',          ],
		       	['GIF Files',        '',        'GIFF'],
		      ],
		      -initialdir       => dirname($default),
		      -initialfile      => "$default",
		      -title            => "$question",
		      );
		      
		chomp($path);
		$UI[$i]{'value'}{$file} = $path;
		$direntry-> delete(0, 'end');
                $direntry-> insert(0, "$path");
	} 
	 )-> pack(-side => 'left',-padx=>3);
   $direntry  = $rightframe -> Entry(
   				-background=> $fileentrycolor,
   				-width=> $textentrywidth 
			)-> pack(-side => 'right');
   $direntry  -> insert(0,"$default");
   $leftframe -> pack(-side => 'left', -anchor=> 'w', -padx => 2, -pady => 0);
   $rightframe-> pack(-side => 'right',-anchor=> 'e', -padx => 3, -pady => 0);
   $fileframe -> pack(-side => 'top',  -padx => 2, -pady => 0, -fill=>'both');

   # Set the initial directory
#   if (! -e $path)  { ("$path does not exist"); }#

   $direntry -> bind('<KeyPress>', sub { 
	print "direntry: $checkbox " if ($debug); 
	$path = $direntry -> get(); 
	chomp($path);  
	print "dirtree: var $var , value $path\n" if ($debug);
	$UI[$i]{'value'}{$file} = $path; 
       	}
   );
   return 0;
}

#..........................................................................
sub Tk_Label { 
   my ($LABEL) = @_; 
   print "doLABEL : $LABEL \n"  if ($debug);
   my ($labelframe) = $rightwidget -> Frame(-height=>30,
   				);
   $labelframe ->  Label ( 
      -background => $titlecolor,
      -font=>"Helvetica 14 italic",
      -text => $LABEL, 
   )-> pack(-side => 'top', -fill=>'both',-expand=>'both',-pady => 0, -fill=>'both',-expand=>1);
   $labelframe -> pack(-side=>'top',-padx=>3, -pady=>0,-fill=>'both',-expand=>1);
   return 0;
}

sub Tk_TextEntry {
   my ($i,$question,$default, $file) = @_;
   my ($inputentry, $textentry, $temp,$pad); 
   if ($i> -1) { $pad = 0;} else { $pad=3;}
   print "doTEXTENTRY   i, question, default = ($i,$question,$default)\n" if ($debug);   

   $textentry = $rightwidget -> Frame(-background=>$textframecolor);
   $textentry -> Label(	-background => $textqcolor,
   			-text => $question, -font=>$elementfont,
		)-> pack(-side => 'left',-anchor=> 'w', -pady=>0,-padx=>1);

   $inputentry = $textentry -> Entry( -width=> $textentrywidth, 
                                      -background=> $textentrycolor,
                   ) -> pack(-side => 'right');
   $inputentry    -> insert(0, "$default");
   $textentry	-> pack(-side => 'top', -anchor=>'e',
   			-padx => 3, -pady => $pad,-fill=>'x');

   $inputentry -> bind('<KeyPress>', 
          sub { 
          	$temp = $inputentry -> get();
          	chomp($temp); 
		if ($i> -1) { 
	  	$UI[$i]{'value'}{$file} = $temp;
          	print "i = $i, \$temp= ",$temp, "\n" if ($debug);
		} else { 
			$outfn{$file} = $temp;
		} 
	  }
   );
   
   return 0;  
}

#..........................................................................
sub Tk_YesNo { 
   my ($i,$question,$default, $file) = @_;
   my ($var,$radioframe,$radio,$rttl);
   print "Tk_YesNo i, question,default = ($i,$question,$default)\n" if ($debug);   
   chomp($default); $var = $default;

   $radioframe = $rightwidget -> Frame(	-background=>$radioframecolor,
   					-height=>20,
				);
   $rttl = $radioframe -> Frame;
   $rttl -> Label(-background => $radioqcolor,
   		-text=>$question, 
   		-font=>$elementfont )-> pack(-side => 'left',-anchor=>'w');
   $rttl -> pack(-side => 'left', 
   		-anchor=>'w', 
		-padx => 1, 
		-pady => 0,
#		-expand=>1
		);	
   $radio = $radioframe -> Frame; 
   $radio -> Radiobutton(-text       => "Yes", -width=>3,
		-background => $radiobuttoncolor, -value      => "yes", 
                -variable   => \$var, -relief=>'raised',
  	        -command => sub { 
	           $UI[$i]{'value'}{$file} = $var;
                   print "i = $i, name = ", $UI[$i]{'name'}{$file}, ", \$var = ",$var, "\n" if ($debug);
		   return;
		},
		)-> pack(-side => 'right');
   $radio -> Radiobutton(-text       => "No", -width=>3,
     		-background => $radiobuttoncolor, -value      => "no", 
                -variable   => \$var,-relief=>'raised',
 	        -command => sub { 
		    $UI[$i]{'value'}{$file}= $var;
  	            print "i = $i,name = ", $UI[$i]{'name'}{$file}, ", \$var = ",$var, "\n" if ($debug);
		    return;
		},
		)-> pack(-side => 'right');
   $radio -> pack(-side => 'right', -padx => 1, -pady => 0);
   $radioframe -> pack(-side => 'top', 
   			-padx => 3, 
			-pady => 0, 
			-fill=>'x', 
   			#-expand=>'x'
			);
  return 0;
}


#..........................................................................
sub Tk_CheckBox { 

   my ($i,$default, $file) = @_;
   my ($var,$checkframe,$check,$rttl,$blank);
   print "Tk_CheckBox i, args, default = ($i,$args,$default)\n" if ($debug);   
   chomp($default); 
   if ($default =~ /on/i) { $var = 1;}
   else { $var = 0;}
   Tk_markCB($i, $var, $file);
   $UI[$i]{'name'}{$file} =~ s/\://gi;
   $UI[$i]{'name'}{$file} =~ s/\?//;
 
   ($labeltext = $UI[$i]{'name'}{$file}) =~ s/\#//gi;;
    
   $checkframe = $rightwidget -> Frame(-background=>$checkframecolor,
   				-height=>30, );

   $check = $checkframe -> Frame(-height=>30, -background=>$checkframecolor); 
   $check -> Checkbutton( -width=>1,
		-background => $checkbuttoncolor, 
		-variable   => \$var, -wraplength => 2, -relief=>'raised',
  		-command => sub { 
			Tk_markCB($i, $var, $file);  return;
		},)-> pack();
   $check -> pack(-side => 'left', -padx => 10, -pady => 0);


   $rttl = $checkframe -> Frame(-height=>30,-background=>$checkframecolor,);
   $rttl -> Label(-background => $checkbackground, 
   		-text=>$labeltext, -font=> $elementfont )
         -> pack();
   $rttl -> pack(-side => 'left', -anchor=>'w', -padx => 1, -pady => 0 );	
   
   $checkframe -> pack(	-side => 'top',
   			-anchor=>'w',
			-padx => 3, 
			-pady => 0, 
#			-expand=>'x',
			-fill=>'x'
		);
#   $checkframe -> update;
}

#..........................................................................
sub Tk_markCB { 
   my ($i, $var, $file) = @_;
   print "before i = $i, name = ", $UI[$i]{'name'}{$file}, ", \$var = ",$var, "\n" if ($debug);
   $UI[$i]{'name'}{$file} =~ s/^\ //gi;
   if ($var == 1) {  
   	$UI[$i]{'name'}{$file} =~ s/\#//gi; 
	$UI[$i]{'args'}{$file} = "on";
   } else { 
   	$UI[$i]{'name'}{$file} = "#" . $UI[$i]{'name'}{$file} if ($UI[$i]{'name'}{$file} !~ /\#/); 
	$UI[$i]{'args'}{$file} = "off";
   }
   print "after i = $i, name = ", $UI[$i]{'name'}{$file}, ", \$var = ",$var, "\n" if ($debug);
 
   return ;
} 

#..........................................................................
sub Tk_HelpAboutRed { 
   $newtoplevel -> destroy() if (Exists $newtoplevel); 
	
   $newtoplevel = MainWindow -> new(-relief=>'ridge',
        				-borderwidth=> 5, 
					-background=>$filebrowseframe); 
   $newtoplevel -> Label ( -background => $filelabelcolor, 
				-text => "About RED", 
				-font=> "Arial 20 bold",
		)-> pack(-fill=>'x',
			-side => 'top', 
			-padx => 6, 
			-pady=>1 
			);
    my (@ARR) = ("About RED", "Resource File Editor",
    		"Global Modeling Assimilation Office", "Created 3/30/2005 "," ",
		
		"[SAVE] - saves current configurations to *.rc file",
		"[NEXT] - goes to next RC file if available",
		"[BACK] - goes back to the previous file if available",
		"[FINISH] - saves all info in memory to *.rc files",
		"This tool holds all config info in memory hash-table", 
		"and saves to rc file at the end or when [save] is pressed",
		);
   foreach my $a (@ARR) { 
     $newtoplevel -> Label(-text=> $a, -background=>$filebrowseframe
      		)->pack(-expand=>1,
			-anchor=>'w',
      			-side=>'top');
   } 
   $newtoplevel -> Button(-text => "Close", 
              -command => sub { $newtoplevel -> destroy(); }
        	)-> pack (-side=>'top', -padx=>6, -pady=>1);

   $newtoplevel->title("RED Help - About RED");
}

#..........................................................................
sub Tk_HelpAbout { 
    my ($file) = @_;
   $newtoplevel -> destroy() if (Exists $newtoplevel); 
   $newtoplevel = MainWindow -> new(-relief=>'ridge',
        				-borderwidth=> 5, 
					-background=>$filebrowseframe); 
   $newtoplevel -> Label ( -background => $filelabelcolor, 
				-text => "About ". $RCtitle{$file}, 
				-font=> "Arial 20 bold",
		)-> pack(-fill=>'x',
			-side => 'top', 
			-padx => 6, 
			-pady=>1 
			);
	for ( $i=0; $i < $nHelp{$file}; $i++ ) { 
     	 $newtoplevel -> Label(	-text => $Help[$i]{$file}, 
				-background => $filebrowseframe
      		)->pack(	-expand=>1,
				-anchor=>'w',
      				-side=>'top'
			);
   	} 
   $newtoplevel -> Button(	-text => "Close", 
              			-command => sub { 
				$newtoplevel -> destroy(); 
				}
        	)-> pack (-side=>'top', -padx=>6, -pady=>1);

   $newtoplevel->title("RED Help - About " . $RCtitle{$file});
   
} 

#..........................................................................
sub Tk_HelpRevision { 
   my ($file) = @_;
   $newtoplevel -> destroy() if (Exists $newtoplevel); 
	
   $newtoplevel = MainWindow -> new(	-relief=>'ridge',
        				-borderwidth=> 5, 
					-background=>$filebrowseframe
				); 
   $newtoplevel -> Label ( 	-background => $filelabelcolor, 
				-text => "Revision History " . $RCtitle{$file}, 
				-font=> "Arial 20 bold",
		)-> pack(	-expand=>1,
				-side => 'top', 
				-padx => 6, 
				-pady=>1 
			);
   for ( $i=0; $i < $nHist{$file}; $i++ ) { 
#   foreach my $a (@Hist{$file}) { 
      $newtoplevel -> Label(	-text=> $Hist[$i]{$file}, 
      			 	-background=>$filebrowseframe
      		)->pack(
			-anchor=>'w',
      			-side=>'top');
   } 
   $newtoplevel -> Button(-text => "Close", 
              	-command => sub { $newtoplevel -> destroy(); }
        	)-> pack (-side=>'top', -padx=>6, -pady=>1);
   $newtoplevel->title("RED Help - Revision History");
} 

#..........................................................................
sub Tk_FileMenu { 
  my ($file) = @_;  
  if (! $file) { $file = $RCfilename[0]};
  $filemenuwidget = $filemenuframe -> Frame(-height=>40);
  $filemenu = $filemenuwidget -> Menubutton('-text' => 'File')-> pack(-side=> 'left');
  $filemenu -> command('-label' => 'Save As', 
  			-command => sub { 
	       $outfn = $filemenu -> getSaveFile(
		      -filetypes        =>
		      [ ['Resource Files','*.rc',],
		      	['All Files', '.*' ],
		      ],
		      );
 		});
  $filemenu -> command('-label' => 'Reload', '-command' => sub {Tk_Reload($file);});
  $filemenu -> separator();
  $filemenu -> command(-label => 'Exit', -command=> sub { exit; } );
  
  $editmenu = $filemenuwidget -> Menubutton(-text => 'Edit')-> pack(-side=> 'left');
  $editmenu -> command(-label => 'vi', 
  		-command=> sub {system("vi $file");     Tk_Reload($file);});	 
  $editmenu -> command(-label => 'nedit', 
  		-command=> sub {system("nedit $file");  Tk_Reload($file);});	
  $editmenu -> command(-label => 'emacs', 
  		-command=> sub {system("emacs $file");  Tk_Reload($file);});   
  $editmenu -> command(-label => 'xemacs', 
  		-command=> sub {system("xemacs $file"); Tk_Reload($file);});	
		
  $helpmenu = $filemenuwidget -> Menubutton(-text => 'Help')-> pack(-side=> 'left');
  $helpmenu -> command(-label => 'About RED', 
  		-command=> sub { Tk_HelpAboutRed($file);} );

  $helpmenu -> command(-label => "About $file", 
  		-command=> sub { Tk_HelpAbout($file);} );

  $helpmenu -> command(-label => 'Revision History', 
  		-command=> sub { Tk_HelpRevision($file);} );	

  $filemenuwidget ->pack();

} 

#..........................................................................
sub Tk_Reload { 
   my ($file) = @_;
     print "Reload for $file" if ($debug);
     $buttonwidget	-> destroy() if ( Exists $buttonwidget	);
     $filemenuwidget 	-> destroy() if ( Exists $filemenuwidget);
     $rightwidget 	-> destroy() if ( Exists $rightwidget	);
     $leftwidget	-> destroy() if ( Exists $leftwidget	);
     Parse_RCfile($file);
     Tk_FileMenu($file);
     Tk_RightWidget($file);
     Tk_LeftWidget($file);
     ___CreateWidget($file);
     return 0;
}

#..........................................................................
sub Tk_LeftWidget { 
  my ($currentfile) = @_;
  if (! $currentfile) { $currentfile = $RCfilename[0]};
  
  $leftwidget = $leftframe -> Frame(-background => '#FFAAAA');
  
  $nasalogo = $leftwidget -> Photo(-format=>"gif", -data => $logo);
#  $nasalogo = $leftwidget -> Photo(-format=>"gif", -file=>"red_nasalogo.gif");
  $label = $leftwidget -> Label(-background => '#FFDDDD', 
  			-text => 'NASA LOGO!',
      			-image => $nasalogo, 
		 )->pack(-side => 'top',-fill=>'both');
  #} 
  my %label;
  
  foreach my $file (@RCfilename) { 
    
    if ($currentfile eq $file) { $labelcolor = "#EEEE00"; } 
    else { $labelcolor = "#EEFFEE"; } 
      
    $label{$file} = $leftwidget -> Label (  -text => $RCname{$file}, -background=> $labelcolor,
		 )->pack(-side=> 'top' , -anchor=>'w', -padx=> 5 , -pady=> 2);

    $label{$file} -> bind ('<Button>', sub { 
			     $buttonwidget	-> destroy() if ( Exists $buttonwidget	);
			     $filemenuwidget 	-> destroy() if ( Exists $filemenuwidget);
			     $rightwidget 	-> destroy() if ( Exists $rightwidget	);
			     $leftwidget	-> destroy() if ( Exists $leftwidget	);
			     #Parse_RCfile($file);
			     Tk_FileMenu($file);
			     Tk_RightWidget($file);
			     Tk_LeftWidget($file);
			     ___CreateWidget($file);
			     
    			}
    );

  } 
  $leftwidget 	-> pack (	-side=>'left', -fill => 'y', 
  				-anchor=>'w', -padx => 1, -pady => 1); 
}

#..........................................................................
sub Tk_RightWidget { 
  my ($file) = @_;
  if (! $file) { $file = $RCfilename[0]};
  
  $rightwidget = $rightframe 	-> Frame(#-relief => 'sunken', 
  		-borderwidth=>0,
		-background => "#FFDDDD",
		);

  $rightwidget -> Label(-text=> $RCtitle{$file}, -background=>"#FFE0E0",
  			-font=>"Arial 29",
 		)->pack(-side=>'top',
			-fill=>'x',-pady=>12,
			#-expand=>1
			);

  $rightwidget  		-> pack(-anchor => 'nw',
  					-side   => 'top',
  					-fill   => 'both',  
#					-expand => 1,
  					-padx 	=> 1, 
 					-pady 	=> 0 	);

#  $leftframe -> update;  

}

#..........................................................................
sub Tk_Buttons { 
  my ($file) = @_;
  my ($backfile,$nextfile, $text, $b,$c);


# Calculate location of current $file
# ----------------------------------
foreach my $a (@RCfilename) { 
	$b++; last if ($a eq $file); 
} 


# Next or Finish? 
# -------------------------------

if ( scalar(@RCfilename) == 1 ) { 
      $text = "Finish";
} elsif ( $file eq $RCfilename[$#RCfilename] ) { 
      $text = "Finish";
} else { 
      $nextfile = $RCfilename[$b];
      $text = "Next";
}


#     Figure out previous file for BACK
#     --------------------------------

$c = $b - 2; 
if ($c < 0) { 
     $backfile = "";
} else { 
     $backfile = $RCfilename[$c];
}
 
$buttonwidget = $buttonframe-> Frame(-height=>35);
$buttonwidget -> Button (-text => "Cancel", -background=> $buttoncolor,
			-command => sub {   exit;	}
		)-> pack(-side => 'left', -padx => 5, -pady => 2, -fill => 'both');
					
$buttonwidget -> Button ( -text => "Save", -background=> $buttoncolor,
			-command => sub { 
			   Update_RC($file);

                           # Write updated resources to file
			   # -------------------------------
			   Write_RC($file,$outfn);  
			   print "wrote to outfn $outfn\n" if ($debug);			
			}

		)-> pack(-side => 'left', -padx => 5, -pady => 2,-fill   => 'both');

if ($backfile) { 
     $buttonwidget -> Button ( -text => "Back", -background=> $buttoncolor,
                      -command => sub { 
		           print "file = $file\n " if ($debug);
	        	   Update_RC($file);
 		 	   print "updated --------------- \n" if ($debug);
			     $buttonwidget	-> destroy() if ( Exists $buttonwidget	);
			     $filemenuwidget 	-> destroy() if ( Exists $filemenuwidget);
			     $rightwidget 	-> destroy() if ( Exists $rightwidget	);
			     $leftwidget	-> destroy() if ( Exists $leftwidget	);
			     #Parse_RCfile($file);
			     Tk_FileMenu($backfile);
			     Tk_RightWidget($backfile);
			     Tk_LeftWidget($backfile);
			     ___CreateWidget($backfile);
			     
		      }
     ) -> pack(-side => 'left', -padx => 5, -pady => 2 ,-fill   => 'both',   );
} 


$buttonwidget -> Button ( -text => $text, -background=> $buttoncolor,

                      -command => sub { 
		           print "file = $file\n " if ($debug);
			   #chomp($currentfile);
		           
	        	   Update_RC($file);
 		 	   print "updated --------------- \n" if ($debug);

		      	   if ($text eq "Finish" ) { 
			   
			      foreach my $rcfile (keys %nUI) { 
				print "rcfile = $rcfile --------------- \n" if ($debug);
				print " --------------- \n" if ($debug);
				
				# Write updated resources to file
			        # -------------------------------
			        Write_RC($rcfile, $outfn);  

				print " --------------- \n" if ($debug);


			       
			        
			      } 
			      exit;

		           } else { 

			     $buttonwidget	-> destroy() if ( Exists $buttonwidget	);
			     $filemenuwidget 	-> destroy() if ( Exists $filemenuwidget);
			     $rightwidget 	-> destroy() if ( Exists $rightwidget	);
			     $leftwidget	-> destroy() if ( Exists $leftwidget	);
			     #Parse_RCfile($file);
			     Tk_FileMenu($nextfile);
			     Tk_RightWidget($nextfile);
			     Tk_LeftWidget($nextfile);
			     ___CreateWidget($nextfile);
			     			     
			   } 
		      }
       ) -> pack(-side => 'left', -padx => 5, -pady => 2 ,-fill   => 'both',   );
$buttonwidget -> pack(-side=>'bottom',-anchor=>'s');
#$buttonwidget ->update();
}

#..........................................................................
sub ___LoadLogo { 

my $logo = qq~
R0lGODlhUwBMANUAANnZ2cDAwKCgwICAgGBggCAAQGBAgMCgwICAwEAggOCAgOBgQOCAQKCg
pIBggOCggMAgAKCAwP/78OBAQGAgQMAgQEAAQIAgQKAgQEAgQMBgQOCgwODAgKBggMCAgMBA
QKBAQMCggMBggIBAgP//////////////////////////////////////////////////////
/////////////////////////////////////////////////////////yH5BAEAAAAALAAA
AABTAEwAAAb/QIBwSCwaj8ikcslsOp/QqHRKrVqv2Kx2y+1KAwGv1xkQDAjAgnAoNBAEgQAQ
IBwSi8YjMqk8Bg4DYEE4JBaHBEEACBAOiUXjEZlUCgOIBLAgHBITBCJxSDgAAcIhsWgcBhTH
49EYQAALwiFxmCAQDYmi0HAAAoRDYtEIWDCOxyMx0AAWhENi0egwGIeEABAgHBKLxAfkYTQa
jQEDsCAcEokJBNEQCRwiiSKxAQQIh8SiMACBBIxGY7EBLAiHxGLBcBBGDIVEJJIwFgkBIEA4
JBIDEAgkUCwWi4AAAVgQDolFI9FgOBoTASBAOCQKAxAIhFEsFouAAAFYEA6JxUhC/5iIFA2G
YrF4AAKEQ2IAAoFAAkQikUgMEIAF4ZBYLCQQCKIhYTQahQcgQDgUPiAQCGRBJBKJxAABWBAO
iUVhxFA0GIxG4zAABAiHCiBEKAwAAcIhsVgkAAvCIdEhCAQCAkciQSQKEwYikUgsJAIAACCw
AEKEQgUQIBwSi0UBsCAcDhMBiSNRMCACAEGCSCw4DkQikSg0BACBCRAiFC6AAOGQWCwGgAXh
kBiRJIiJgiEAiCSIRCKRSCQOFECIcDgJAAHCIbFIDBiABeGQKHEIDULE0BCQEIhEIpFIFFIq
QIhwCAkAAcIhsVgUAAvCIdEAMAgdCUIAkRgiJIIEkUgkEv+FlgsQIhwKA0CAcEgsFgPAgnBI
LBgAxEgiUiwkAgFDsVgsXipAiHAoDAABwiGxaEQAC8IhsWAAJIqE4jAiIRSLRSEFA4QIh8MA
ECAcEovGALAgHBKHAEOxWHRIHMUiMYMBQoRDYgAIEA6JRSOgASwIh8ShxFEsFgsGSaRYLFgu
FSBEOBxWDECAcEgsGoUJYEE4JA4PgmKxKEwEBMWh5VIBQoRDYoVSOAABwiGxaAwAC8IhkeiQ
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
~;

return $logo;

}
1;




