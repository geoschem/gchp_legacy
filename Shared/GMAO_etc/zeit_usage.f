!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!
!
! !PROGRAM:      zeit_usage
!
! !DESCRIPTION:  This program returns usage information about the particular
!                program that was called
!
! !USAGE:        zeit_usage(progname)
!                where:  progname    Name of called program
!                                    
      subroutine zeit_usage(progname)
      implicit none
      character*7 progname
      if(progname.eq.'zeit_pr')then
      print*,'Usage:     zeit_pr.x [-i fname] [-o out_fname] [-d] [-h]'
         print*,'-i         Indicate use of a users registry file'
         print*,'fname      Input registry file. Default is .zeit'
         print*,'-o         Indicate name of output file'
         print*,'out_fname  Output file to write current timings'
         print*,'           Default is standard output'
         print*,'-d         Option to remove registry when finished'
         print*,'-h         Option to print usage message'
      elseif(progname.eq.'zeit_ci')then
         print*,'Usage is:   zeit_ci.x [-r fname] [-v] [name]'
         print*,'-r fname    Time register file name,' 
         print*,'            default is .zeit'
         print*,'-v          Verbose mode: prints wallclock'
         print*,'            time'
         print*,'name        Name of program to be timed'
      elseif(progname.eq.'zeit_co')then
         print*,'Usage is:   zeit_co.x [-r fname] [-v] [name]'
         print*,'-r fname    Time register file name,' 
         print*,'            default is .zeit'
         print*,'-v          Verbose mode: prints wallclock'
         print*,'            time'
         print*,'name        Name of program to be timed'
      endif
      return
      stop
      end subroutine zeit_usage
      
