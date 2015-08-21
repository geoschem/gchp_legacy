      program dyndiff

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: dyndiff: find the dynamic vector difference between two hdf files
!
! !USAGE: see the routine usage() below
!
! !USES:
!
      use m_dyn

      implicit NONE

! !DESCRIPTION: Uses statistic subroutine dyn_stat to check the dynamic
!               vector dfference between two alike hdf files.
!
! !REVISION HISTORY:
!
!  2002.01.07  E. Yeh:    Initial code.
!  2005.03.28  Elena N.:  Added an option to save resulting dynamic vector in a file.
!  01May2007   Todling    Support to new GEOS-5 dyn-vector files
!  06May2007   Todling    Add opt to print ave/min/max when one file is given as input.
!  05Mar2009   Todling    Add land fractions
!  08Mar2012   Todling    Add acoeff [to allow calc of inc as -(b-a)]
!  12Sep2012   Todling    Quick fix for LM issue in dyn-vector
!
!-------------------------------------------------------------------------
!EOP

      character(len=*), parameter :: myname = 'dyndiff'

!     File names
!     ----------
      integer, parameter :: MFILES = 2 ! max.   number of input files
      character(len=255) :: files(MFILES), binfiles(MFILES), etafile, st
      integer            :: nfiles       ! actual no. of input files

      character(len=255) :: dyn_dout      ! difference output file name


!     Dynamics/simulator vectors
!     --------------------------
      type(dyn_vect) dyn(MFILES)   ! dynamics vector in eta

!     Locals
!     ------
      integer, parameter :: READ_ONLY = 1
      integer fid, nvars, ngatts
      integer ios, rc, iopt, ifile
      integer ntimes, n, freq, nymd, nhms, prec
      integer freq_d, nymd_d, nhms_d, prec_d    !Timetag for newly created diff in *.hdf format  
      integer im, jm, km, lm, system, dyntype
      logical dominmax,verb
      real    acoeff
      
!  Initialize
!  ----------     
   call Init_ ( dyntype, mfiles, files, dominmax, verb )
   nfiles = 1
!  Loop over input eta files
!  -------------------------
   do ifile = 1, nfiles

      etafile = files(ifile)

!     Determine how many time levels on file
!     --------------------------------------
      call GFIO_Open ( etafile, READ_ONLY, fid, rc )
      if ( rc .ne. 0 ) then
         call die(myname,'cannot open GFIO file '//trim(etafile))
      end if
      call GFIO_DimInquire ( fid, im, jm, km, ntimes, nvars, ngatts, rc)
      if ( rc .ne. 0 ) then
         call die(myname,'problems getting dimensions' )
      end if
      call GFIO_Close ( fid, rc )
      
!     For each time on file...
!     ------------------------
      do n = 1, ntimes

!        Get ETA data for this time
!        --------------------------
         call dyn_get ( etafile, nymd, nhms, dyn(1), rc, timidx=n, freq=freq, vectype=dyntype )
         nymd_d = nymd     !Newly created diff file with nymd from first file dyn(1)
         nhms_d = nhms     !Newly created diff file with nhms from first file dyn(1)

         if ( rc .ne. 0 ) then
            call die(myname,'cannot read dynamics vector file')
         end if
         call dyn_get ( files(2), nymd, nhms, dyn(2), rc, timidx=n, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read dynamics vector file')
         end if

         print *, "> nymd, nhms: ", nymd, nhms, " (diff)"
         lm = min(dyn(1)%grid%lm,dyn(2)%grid%lm)
         if ( .not. dominmax ) then
           print *, "scaling difference by: ", acoeff
           dyn(1)%ps     = acoeff*(dyn(1)%ps - dyn(2)%ps)
           dyn(1)%ts     = acoeff*(dyn(1)%ts - dyn(2)%ts)
           dyn(1)%phis   = acoeff*(dyn(1)%phis - dyn(2)%phis)
           dyn(1)%lwi    = acoeff*(dyn(1)%lwi - dyn(2)%lwi)
           dyn(1)%frland = acoeff*(dyn(1)%frland - dyn(2)%frland)
           dyn(1)%frlandice = acoeff*(dyn(1)%frlandice - dyn(2)%frlandice)
           dyn(1)%frlake =  acoeff*(dyn(1)%frlake - dyn(2)%frlake)
           dyn(1)%frocean=  acoeff*(dyn(1)%frocean - dyn(2)%frocean)
           dyn(1)%frseaice= acoeff*(dyn(1)%frseaice - dyn(2)%frseaice)
           dyn(1)%hs_stdv = acoeff*(dyn(1)%hs_stdv - dyn(2)%hs_stdv)
           dyn(1)%delp    = acoeff*(dyn(1)%delp - dyn(2)%delp)
           dyn(1)%u       = acoeff*(dyn(1)%u - dyn(2)%u)
           dyn(1)%v       = acoeff*(dyn(1)%v - dyn(2)%v)
           dyn(1)%pt      = acoeff*(dyn(1)%pt - dyn(2)%pt)
           dyn(1)%q(:,:,:,1:lm) = acoeff*(dyn(1)%q(:,:,:,1:lm) - dyn(2)%q(:,:,:,1:lm))
         endif

!       If so, echo result to standard out
!       ----------------------------------
         if (verb) then
            call dyn_stat(6, dyn(1), rc)
            if ( rc .ne. 0 ) then
               call die(myname,'cannot process dyn_stat')
            endif
         endif

!       If requested write *.hdf file with a header from dyn(1)
!       -------------------------------------------------------
        if ( trim(dyn_dout) .ne. 'NONE' ) then
             dyn(1)%grid%lm = lm
             call dyn_put ( trim(dyn_dout), nymd_d, nhms_d, 0, dyn(1), rc, freq=freq, vectype=dyntype )
        endif 

!       Clean up mess
!       -------------
        call dyn_clean ( dyn(1) )
        call dyn_clean ( dyn(2) )

      end do

   end do ! loop over files

   st = "rm -f " // trim(binfiles(1)) // " " // trim(binfiles(2))  
   rc = system(st)      ! rc == 0 for success
   if (rc .ne. 0 ) then
     print *, "Unable to remove binary files."
   end if

!  All done
!  --------
!  All done
!  --------
   close(999)
   open (999,file='DYNDIFF_EGRESS',form='formatted')
   close(999)
   call exit(0)

CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: Init_ --- Initialize dyn2dyn
!
! !DESCRIPTION: parses command line.
!
! !INTERFACE:
!
      subroutine Init_ ( dyntype, mfiles, files, dominmax, verb )

      implicit NONE

      integer,       intent(out) :: dyntype ! 4=geos4, 5=geos5
      integer,       intent(in)  :: mfiles  ! max. number of eta files
                                            ! dynamics file names (eta)
      character*255, intent(out) :: files(mfiles) 
      logical, intent(out) :: dominmax
      logical, intent(out) :: verb
      
!
! !REVISION HISTORY:
!       2002.01.07  E. Yeh   Initial code.
!       05Oct2012   Todling  Add verb
!
!EOP
!BOC

      character*4, parameter :: myname = 'init'

      integer iret, i, iarg, argc, iargc
      character(len=255) :: etafile, argv
      logical dout

      dout = .false.
      verb = .false.

      dyn_dout = 'NONE'
      dyntype  = 4        ! default is GEOS-4 files
      dominmax = .false.
      acoeff = 1.0


      print *
      print *, '     ---------------------------------------------------------'
      print *, '     dyndiff - dynamic vector difference between two hdf files'
      print *, '     ---------------------------------------------------------'
      print *

!     Parse command line
!     ------------------
      argc =  iargc()
      if ( argc .lt. 1 ) call usage()

      iarg = 0
      nfiles = 0

      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iarg, argv )
         
         select case (argv)
           case ("-g5")
             dyntype = 5
           case ("-verb")
             verb = .true.
           case ("-a")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, argv )
             read(argv,*) acoeff
           case ("-o")
             dout = .true.
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, dyn_dout )
           case ("-h")
             if ( iarg+1 .gt. argc ) call usage()
           case default
             nfiles = nfiles + 1
             if ( nfiles .gt. mfiles ) call usage()
             files(nfiles) = argv

         end select

      end do

      if ( nfiles .lt. 1 ) call usage()
      if ( nfiles .eq. 1 ) then
           nfiles = 2
           files(2) = files(1)
           dominmax = .true.
      endif
      if ( nfiles .gt. 2 ) call usage()

!     Echo the parameters
!     -------------------
      print *
      print *, '------------------------------------------------------------------'
      print *, '  Eta     Dynamics state files: '
      do i = 1, nfiles
        print *, i, ': ', trim(files(i))
      end do

      end subroutine Init_

!.................................................................

      subroutine usage()
      print *
      print *,'Usage: '
      print *
      print *,'  dyndiff.x [-h] [-g5] [-verb] etafile_1 etafile_2 [-o diff_file]'
      print *
      print *, 'where'
      print *
      print *, '-h          Help (optional)'
      print *, '-verb       Echo different to standard out'
      print *, '              (default: FALSE) '
      print *, '-g5         Treats files as GEOS-5 files'
      print *, '-a  coeff   Scale difference by this coefficient (see note)'
      print *
      print *
      print *, 'Where etafile_1 and etafile_2 are two (required)'
      print *, 'input dynamics vector files in hybrid (eta) coordinates'
      print *
      print *, '-o diff_file      Optional to save binary output'
      print *, '                  where diff_file - output dynamics vector'
      print *, '                  file in hybrid (eta) coordinates'
      print *, '                  with timestamp from the etafile_1'
      print *, '                  and 32-bit precision'
      print *, '                  output = file1 - file2'
      print *, '                  (default: difference is shown at the screen in ascii)'
      print *
      print *, 'Notes: '
      print *, '  1. User can save the ascii output by the following command:'
      print *, '     dyndiff.x etafile_1 etafile_2 > OutPutFileName' 
      print *, '  2. Ability to scale dff by a coefficient must be exercized'
      print *, '     with caution, since all entries will be scaled - which is '
      print *, '     in general meaningless. This ability is added to compensate'
      print *, '     for the fact that sometimes file1-file2 not possible, but'
      print *, '     file2-file1 is possible due to nc4-header issues'
      call exit(1)
      end subroutine usage
      
!.................................................................

      subroutine die ( myname, msg )
      character(len=*) :: myname, msg
      write(*,'(a)') trim(myname) // ': ' // trim(msg)
      call exit(1)
      end subroutine die

!.................................................................

  end program dyndiff
