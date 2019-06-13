!-------------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP        
!
! !ROUTINE: echorc: returns a variable from an rc file

   program echorc

!
! !USAGE: see usage
!
! !USES:
!
   use m_inpak90
   use m_StrTemplate
   use m_die

   implicit NONE
!
! !TODO:
!
!  This could/should be generalized as needed to read different quatities
!  from the rc file, including tables, etc.
!
! !REVISION HISTORY:
!
!  07nov2003 Todling  Created this program.
!  21mar2005 Todling  Modified to do the reverse of pasta.
!  14nov2007 Todling  Expanded to allow reading various entries from single line
!  07may2013 Todling  Add ability to echo table contents
!  03oct2016 Todling  Ability to read specified column of table
!  12dec2016 Todling  Skip comment line start w/ exclamation mark
!
!-------------------------------------------------------------------------
!EOP

   character(len=*), parameter :: myname = 'echorc'

   integer, parameter :: MAXENTRIES = 20
   character*255  :: rcfile
   character*255  :: var, fullvar, varentry, fld, expid
   character*255  :: xentry,redirect
   character*255  :: syscmd1, syscmd2
   character*255  :: manyentries(MAXENTRIES)
   logical        :: template, norc, fexist
   integer        :: nymd, nhms
   integer        :: i, ii, ic, rc, irow, ncol, ier
 
   call init_ ( rcfile, var, template, norc, ncol, expid, nymd, nhms )

!  Load resources
!  -------------
   if ( norc ) then

        if (template) then
            call strTemplate ( fld, var, 'GRADS', trim(expid), &
                               nymd, nhms, ier )
            write(6,'(a)') trim(fld)
         else
            write(6,'(a)') trim(var)
        endif

   else

        call i90_loadf ( trim(rcfile), ier )
        if ( ier .ne. 0 ) then
          write(6,'(2a)') 'cannot find rc file ' // trim(rcfile)
          stop 1
        endif

       fullvar = trim(var) // '::'
       call i90_label ( trim(fullvar), ier )
       if ( ier .eq. 0 ) then ! first try reading a table label

          irow = 0
          do while (ier==0)                   ! read table entries
             call I90_GLine ( ier )           ! ier=-1: end of file; +1: end of table
             if (ier==0) then                 ! OK, we have next row of table
                 call I90_GToken(varentry, ier ) ! get this row
                 if (varentry(1:1)=="!") cycle   ! this is a comment line
                 if (ncol>1) then
                    do ii=2,ncol
                       call I90_GToken(varentry, ier ) ! get this row
                    enddo
                 endif
                 if (ier/=0) then
                     write(6,'(2a,i5)') 'echorc.x: I90_GToken error, ier=', ier
                     stop 1
                 end if
                 if (template) then
                     call strTemplate ( fld, varentry, 'GRADS', trim(expid), &
                                        nymd, nhms, ier )
                     write(6,'(a)') trim(fld)
                 else
                     write(6,'(1x,a)') trim(varentry)
                 endif
                 irow = irow+1
             end if
          end do
     

       else  ! if not a table label, try a variable label

         fullvar = trim(var) // ':'
         call i90_label ( trim(fullvar), ier )
         if ( ier .ne. 0 ) then
             write(6,'(a)') 'cannot find variable string in rc file '
             stop 1
         else
            call i90_gtoken ( varentry, ier )
               if ( ier .ne. 0 ) call die (myname, 'premature end of rc file')
            if (template) then
               call strTemplate ( fld, varentry, 'GRADS', trim(expid), &
                                  nymd, nhms, ier )
               write(6,'(a)') trim(fld)
            else
               ic = 1; ier=0
               manyentries(ic) = trim(varentry)
               do while ( ier==0 .and. ic<= MAXENTRIES )   ! try read more entries
                  call i90_gtoken ( varentry, ier )
                  if(ier==0) then
                     ic=ic+1
                     manyentries(ic) = trim(varentry)
                  endif                  
               enddo
               write(6,'(a)') (trim(manyentries(i)),i=1,ic)
            endif
         endif

       endif

   endif

   stop 0

   CONTAINS

!-------------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: init --- 
!
! !DESCRIPTION: parses command line.
!
! !INTERFACE:
!
   subroutine init_ ( rcfile, var, template, norc, ncol, expid, nymd, nhms )

   implicit NONE

   character*255, intent(out) :: rcfile
   character*255, intent(out) :: var
   character*255, intent(out) :: expid
   logical, intent(out)       :: template
   logical, intent(out)       :: norc
   integer, intent(out)       :: nymd, nhms
   integer, intent(out)       :: ncol

!
! !REVISION HISTORY:
!
! 06Nov2003   Todling    Initial code.
! 14Jan2005   Todling    Implementation of -rc opt was missing.
! 21Mar2005   Todling    Implemented -fill opt.
!
!EOP

!BOC

   character(len=*), parameter :: myname = 'init_'

   character*255 :: argv
   integer       :: i,argc,iarg,iargc

!  Defaults
!  --------
   var      = 'NONE'
   template = .false.   ! when var requested not a template type
   rcfile   = 'fvpsas.rc'  ! default rc file
   norc     = .false.   ! default: read template from resource file
   ncol     = 1

   argc =  command_argument_count()
   if ( argc .lt. 1 ) call usage_()
   iarg = 0
lp:  do i = 1, 32767
        iarg = iarg + 1
        if ( iarg .gt. argc ) then
             exit lp
        endif 
        call Get_Command_Argument ( iArg, argv )
        if (index(argv,'-template') .gt. 0 ) then
             if ( iarg+3 .gt. argc ) call usage_()
             iarg = iarg + 1
             call Get_Command_Argument ( iArg, argv )
             read(argv,*) expid
             iarg = iarg + 1
             call Get_Command_Argument ( iArg, argv )
             read(argv,*) nymd
             iarg = iarg + 1
             call Get_Command_Argument ( iArg, argv )
             read(argv,*) nhms
             template = .true.
        elseif (index(argv,'-fill') .gt. 0 ) then
           norc = .true.
        elseif (index(argv,'-ncol') .gt. 0 ) then
           if ( iarg+1 .gt. argc ) call usage_()
           iarg = iarg + 1
           call Get_Command_Argument ( iArg, argv )
           read(argv,*) ncol
        elseif (index(argv,'-var') .gt. 0 ) then
           if ( iarg+1 .gt. argc ) call usage_()
           iarg = iarg + 1
        elseif (index(argv,'-rc') .gt. 0 ) then
             if ( iarg+1 .gt. argc ) call usage_()
             iarg = iarg + 1
             call Get_Command_Argument ( iarg, rcfile )
        else
           var = trim(argv)
        end if
     end do lp

   return
   end subroutine init_

     subroutine usage_()
      print *
      write(6,'(a)') ' Usage : echorc [-template expid nymd nhms] [-rc rcfile] rcstring'
      print *
      write(6,'(a)') ' rcstring                    string in rcfile that value is to be echoed '
      print *
      write(6,'(a)') ' -template expid nymd nhms  specify when var is expected to be a GRADS template'
      write(6,'(a)') ' -rc       rcfilename       rc file name (default: fvpsas.rc)'
      write(6,'(a)') ' -ncol     column_number    when reading table, allows getting given column (default: 1)'
      print *
      write(6,'(a)') ' Example (using rc file): echorc.x -template myexp 20040101 060000 -rc myfile.rc templatename '
      write(6,'(a)') '  with myfile.rc containing the following line: ' 
      write(6,'(a)') '       templatename: %s.myfile.something.%y4%m2%d2_%h2z.bin '
      write(6,'(a)') '  the result of echorc.x would be:  myexp.myfile.something.20040101_06z.bin ' 
      print *
      write(6,'(a)') ' Example (direct template fill): '
      write(6,'(a)') '    echorc.x -template myexp 20040101 060000 -fill %s.myfile.something.%y4%m2%d2_%h2z.bin '
      print *
      stop
     end subroutine usage_

   end program echorc


