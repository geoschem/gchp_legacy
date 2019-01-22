!-------------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP        
!
! !ROUTINE: dyn2drs: convert dyn-like file to FVGCM restart file

   program dyn2drs

!
! !USAGE: see usage
!
! !USES:
!
   use m_dyn

   implicit NONE
!
!  2003.05.30  C. Cruz:   Initial Code
!  2004.09.10  Todling    Renamed program.
!  2004.23.11  Todling    Added skipo3 option.
!
!-------------------------------------------------------------------------
!EOP

   integer :: nymd=0, nhms=0
   integer, parameter :: mynstep = 15760
   character(len=*), parameter :: myname = 'dyn2drs'
   type(dyn_vect) :: w_f     ! fv dynamics vector in eta

!  Locals
   
   integer :: rc
   integer :: nstep, freq
   logical :: oldana, pick
   logical :: verbose, skipo3

!  File names

   character(len=255) :: etafile, binfile

! start

   call init( etafile, binfile, pick, nymd, nhms, oldana, skipo3, verbose )

   rc = 0

   ! read time index
   if(verbose) print *, 'Get eta fields' 
   if ( oldana ) then
        if ( pick ) then
             call dyn_get ( etafile, nymd, nhms, w_f, rc, timidx=0, freq=freq )
        else
             call dyn_get ( etafile, nymd, nhms, w_f, rc, freq=freq )
        endif
        nstep = mynstep
   else
        if ( pick ) then
             call dyn_get ( etafile, nymd, nhms, w_f, rc, &
                            timidx=0, freq=freq, nstep=nstep )
        else
             call dyn_get ( etafile, nymd, nhms, w_f, rc, &
                            freq=freq, nstep=nstep )
        endif
   endif
   if ( rc .ne. 0 ) then
     if ( rc > 1109 ) then
         print *, ' Ozone not found in eta file'  ! not really an error
     else
         print *, ' Cannot read eta file'
         call exit (1)
     endif
   end if
  
   if(verbose) print *, 'Write out restart (bin)'
   call put_fvrst ( binfile, w_f, nymd, nhms, nstep, skipo3, verbose )

! clean up

   if(verbose) print *, 'Clean up '
   call dyn_clean(w_f)

! done
   
   if(verbose) print *, ' -- dyn2drs.x has successfully ended -- '
   call exit(0)

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
   subroutine init ( etafile, binfile, pick, nymd, nhms, oldana, skipo3, verbose )

   implicit NONE

   character*255, intent(out) :: etafile
   character*255, intent(out) :: binfile
   integer, intent(inout) :: nymd, nhms
   logical, intent(out)   :: pick
   logical, intent(out)   :: oldana
   logical, intent(out)   :: verbose
   logical, intent(out)   :: skipo3

!
! !REVISION HISTORY:
!
! 2003.05.30  C. Cruz:   Initial Code
! 06Nov2003   Todling    Cleaned up and generalized.

!
!EOP

!BOC

   integer, parameter :: nfiles_max = 1
   character(len=255) :: infile(nfiles_max)
   character*4, parameter :: myname = 'init'
   integer :: i,argc,iarg,iargc,nfiles,leta
   character*255 :: argv

! defaults

   pick    = .false.
   oldana  = .false.
   binfile = 'NONE'
   verbose = .false.
   skipo3  = .false.

   argc =  iargc()
   if ( argc .lt. 1 ) call usage()
   nfiles = 0
   iarg = 0
lp:  do i = 1, 32767
      iarg = iarg + 1
      if ( iarg .gt. argc ) exit lp
      call GetArg ( iArg, argv )
      if (index(argv,'-oldana') .gt. 0 ) then
         oldana = .TRUE.
      elseif (index(argv,'-verbose') .gt. 0 ) then
         verbose = .TRUE.
      elseif (index(argv,'-skipo3') .gt. 0 ) then
         skipo3  = .TRUE.
      elseif (index(argv,'-pick') .gt. 0 ) then
           if ( iarg+2 .gt. argc ) call usage()
           iarg = iarg + 1
           call GetArg ( iArg, argv )
           read(argv,*) nymd
           iarg = iarg + 1
           call GetArg ( iArg, argv )
           read(argv,*) nhms
           pick = .true.
      elseif (index(argv,'-o') .gt. 0 ) then
         if ( iarg+1 .gt. argc ) call usage()
         iarg = iarg + 1
         call GetArg ( iArg, binfile )
      else
         nfiles = nfiles + 1
         if ( nfiles .gt. nfiles_max ) then
            if(verbose) print *, 'Maximum number of input files = ', nfiles_max
            stop
         end if
         infile(nfiles) = argv
      end if
     end do lp
   if ( nfiles .lt. 1 ) call usage()

   etafile = trim(infile(1))

   if ( binfile == 'NONE' ) then
        leta    = len(trim(etafile))-3
        binfile = etafile(1:leta)//'bin'
   endif

   rc = 0

   return
   end subroutine init

     subroutine usage()
      print *
      print *, ' Usage : dyn2drs [-pick nymd nhms] [-oldana] [-o outfn] '
      print *, '                 [-verbose] [-skipo3] etafile'
      print *, '                        '
      print *, ' -pick nymd nhms  specify date and time (default: last in file)'
      print *, ' -oldana          needs to be used if old analysis files  '
      print *, '                  (that is, earlier than 1.4r2p2 or 1.5alpha4)'
      print *, ' -skipo3          ignores dyn-vector O3 and writes rst w/o O3'
      print *, '                  (usefull when O3 is zero and user wants to add clim O3)'
      print *, ' -verbose         prints out some info'
      print *, ' -o    filename   output filename (default: prefix.bin)'
      print *
      stop
     end subroutine usage

!-------------------------------------------------------------------------
   subroutine put_fvrst ( binfile, w_f, nymd,nhms,nstep, skipo3, verbose )
!-------------------------------------------------------------------------
!  use m_ioutil, only : luavail   !_RT: unwired file unit number
   implicit none
   character(len=*), parameter :: myname = 'put_fvrst'
   integer,          parameter :: lu=10
   character*255  :: binfile
   type(dyn_vect) :: w_f
   logical        :: skipo3, verbose
   integer        :: nymd,nhms,nstep
   integer        :: im,jm,km,lm
   integer        :: i ,j ,k ,l 
  
!  lu = luavail()
   im = w_f%grid%im
   jm = w_f%grid%jm
   km = w_f%grid%km
   lm = w_f%grid%lm
   if ( lm>1 .and. skipo3 ) lm = lm-1
   open ( unit=lu, file = binfile, form = 'unformatted' )
   write (lu) nstep,nymd,nhms
   write (lu) w_f%ps,w_f%delp,w_f%u,w_f%v,w_f%pt
   do l = 1, lm
      write(lu) (((w_f%q(i,j,k,l),i=1,im),j=1,jm),k=1,km)
   end do
   close (lu)
   if(verbose) print *, 'Number of tracers found (other than sphu) ', lm-1

   end subroutine

   end program dyn2drs


