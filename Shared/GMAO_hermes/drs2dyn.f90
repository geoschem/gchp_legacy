!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP        
!
! !ROUTINE: drs2dyn: converts d_rst GCM restarts to dyn-vect eta 

   program drs2dyn

! !USES:
!
   use m_dyn, only : dyn_vect
   use m_dyn, only : dyn_null
   use m_dyn, only : dyn_init
   use m_dyn, only : dyn_put
   use m_dyn, only : dyn_clean

   use m_StrTemplate        ! grads style templates

   use m_set_eta, only : set_eta

   use m_die, only : die

   implicit NONE
!
! !DESCRIPTION: Creates and dyn-vector eta file from GCM restarts and 
!               boundary conditions. 
!
! !REVISION HISTORY:
!
!  30Sep2004 Todling   Initial Code.
!  14Dec2004 Todling   Turned PREC into a command line option.
!
!---------------------------------------------------------------------------
!EOP

   character(len=*), parameter :: myname = 'drs2dyn'

   type(dyn_vect) :: w_f     ! fv dynamics vector in eta

   integer :: im, jm, km, lm, ks
   integer :: nymd, nhms, nstep, freq, prec
   integer :: ier
   real    :: ptop, pint
   real, allocatable :: ak(:), bk(:)

   character(len=255) :: dynftmpl, dynfile, expid, drsfile, phyfile, bcsfile

!  Parse command line
!  ------------------
   call init_ ( drsfile, phyfile, bcsfile, dynftmpl, expid, im, jm, km, lm, freq, prec )

!  Make sure space is clean
!  ------------------------
   call dyn_null ( w_f )

!  Set up grid
!  -----------
      allocate(ak(km+1),bk(km+1),stat=ier); if(ier/=0) call die(myname,'error alloc(ak,bk)')
   call set_eta ( km, ks, ptop, pint, ak, bk )

!  Assign pointers in w_f input arrays
!  -----------------------------------
   call dyn_init ( im, jm, km, lm, w_f, ier, ptop, ks, ak, bk ) 
       if ( ier .ne. 0 ) call die (myname,'cannot initialize dyn-vector')

!  Read GCM dynamics restart, GCM physics restart, and surface file
!  ----------------------------------------------------------------
   call get_fvrst ( drsfile, w_f, nymd, nhms, nstep )
   call get_phys  ( phyfile, im, jm, w_f%lwi, w_f%ts )
   call get_bcs   ( bcsfile, im, jm, w_f%phis, w_f%hs_stdv )

!  Construct file name
!  -------------------
   call strTemplate ( dynfile, dynftmpl, 'GRADS', xid=expid, &
                      nymd=nymd, nhms=nhms, stat=ier )
      if (ier/=0) call die(myname,'cannot determine file via template')

!  Write out vertically interpolated state vector
!  ----------------------------------------------
   call dyn_put ( dynfile, nymd, nhms, prec, w_f, ier, &
                           freq=freq, nstep=nstep )
        if ( ier/=0 ) call die(myname,'cannot write ETA file')

!  Clean up
!  --------
   call dyn_clean(w_f)

!  Completed
!  ---------
   write(6,'(3a,i8.8,a,i2.2,a)') ' Created ', trim(dynfile), & 
           ' with dyn-vect for ', nymd, ' at ',  nhms/10000,  'Z'
   print *, '--------------------------------------------------------------------'
   call exit(0)

   CONTAINS

!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
! !IROUTINE: Init_ --- Initialize dyn2dyn
!
! !DESCRIPTION: parses command line.
!
! !INTERFACE:
!
      subroutine Init_ ( drsfile, phyfile, bcsfile, dynftmpl, expid, &
                         in, jn, kn, ln, freq, prec )

      implicit NONE

      character*255, intent(out) :: drsfile ! dynamicss restart file
      character*255, intent(out) :: phyfile ! file containing lwi and ts
      character*255, intent(out) :: bcsfile ! surface bcs file containing phis and hs_stdv
      character*255, intent(out) :: dynftmpl! dyn-vect dynamics file name
      character*255, intent(out) :: expid   ! experiment name
      integer,       intent(out) :: in      ! number of longitude points
      integer,       intent(out) :: jn      ! number of latitude  points
      integer,       intent(out) :: kn      ! number of vertical levels for output
      integer,       intent(out) :: ln      ! number of tracers counting qwv and o3
      integer,       intent(out) :: freq    ! frequency for output eta file
      integer,       intent(out) :: prec    ! precision of output dyn file: 0=32; 1=64
      
!
! !REVISION HISTORY:
!       30Sep2004  Todling  Initial code
!       14Dec2004  Todling  Added -prec option
!
!EOP
!BOC

      character*4, parameter :: myname = 'init'

      integer nfiles, iret, i, iarg, argc, iargc
      integer ires
      character(len=255) :: etafile, argv, res
      character*10 str

      integer,               parameter :: MFILES = 3  ! d_rst, p_rst, and bcs(sfc)

      integer, dimension(4), parameter :: IMS = (/ 72, 144, 288, 576 /)
      integer, dimension(4), parameter :: JMS = (/ 46,  91, 181, 361 /)
      integer, dimension(3), parameter :: KMS = (/ 18,  32, 55 /)
      integer, dimension(2), parameter :: LMS = (/  1,   2 /)

!     Defaults
!     --------
      in   = 288           ! default is c55-2
      jn   = 181           ! default is c55-2
      kn   = 55            ! default is c55-2
      ln   = 2             ! default is c55-2
      freq = 060000        ! default 6-hr output eta file
      phyfile ='p_rst'     ! default filename of file containing LWI field
      expid   ='drs2dyn'   ! default head name for full output filename
      dynftmpl= '%s.prog.eta.%y4%m2%d2.nc4'  ! default filename for output file
      prec = 1             ! default is to write a 64-bit file 

!     Parse command line
!     ------------------
      argc =  iargc()
      if ( argc .lt. 1 ) call usage()

      iarg = 0
      nfiles = 0

      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) then
              exit
         end if
         call GetArg ( iarg, argv )
         select case (argv)
           case ("-o")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, dynftmpl )
           case ("-expid")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, expid )
           case ("-res")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, res )
             select case (res)
               case ("a")
                     ires=1
               case ("b")
                     ires=2
               case ("c")
                     ires=3
               case ("d")
                     ires=4
               case default
                     print *, 'Sorry this resolutio not supported'
                     call exit(1)
             end select
             in = ims(ires)
             jn = jms(ires)
           case ('-freq')
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iarg, str )
            read(str,*) freq
           case ('-prec')
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iarg, str )
            read(str,*) prec
           case ('-nlevs')
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iarg, str )
            read(str,*) kn
            if (count(kn==KMS).eq.0) then
                print *, 'Cannot handle this vertical number of levels'
                call exit(1)
            endif
           case ('-ntrac')
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iarg, str )
            read(str,*) ln
            if (count(ln==LMS).eq.0) then
                print *, 'Cannot handle this number of tracers'
                call exit(1)
            endif
           case default
             nfiles = nfiles + 1
             if ( nfiles .gt. mfiles ) call die(myname,'too many eta files')
             if(nfiles==1) drsfile = argv
             if(nfiles==2) phyfile = argv
             if(nfiles==3) bcsfile = argv
         end select
      end do

      if ( nfiles .lt. 1 ) call usage()
      if ( prec < 0 .or. prec > 1 ) call usage()

!     Always echoes the parameters
!     ----------------------------
      print *
      print *, '--------------------------------------------------------------------'
      print *, '  d_rst   Dynamics restart file ', trim(drsfile)
      print *, '  p_rst   Physics  restart file ', trim(phyfile)
      print *, '  sfbcs   Surface  bcs     file ', trim(bcsfile)
      print *, '....................................................................'

      end subroutine Init_
!.................................................................

      subroutine usage()
      print *
      print *, '  -----------------------------------------------------------'
      print *, '  drs2dyn - converts GCM rsts and bcs to dyn-vector eta file '
      print *, '  -----------------------------------------------------------'
      print *
      print *
      print *,'Usage: '
      print *,'  drs2dyn.x [-o dynfmpl ] [-expid XID] [-freq FREQ] [-prec PREC]'
      print *,'            [-res RES] [-nlevs NLEVS] [-ntrac NTRAC] '
      print *,'            d_rst p_rst sbcs'
      print *
      print *, 'where'
      print *
      print *, '-o dynftmpl   output dynamics vector template file name'
      print *, '              Default: %s.prog.eta.%y4%m2%d2.nc4'
      print *, '-expid XID    head name for output filename'
      print *, '              Default: drs2dyn'
      print *, '-freq  FREQ   frequency (HHMMSS) of time in output file '
      print *, '              Default: 060000'
      print *, '-prec  PREC   precision of output 0=32; 1=64'
      print *, '-res   RES    where RES= a, b, c, or d '
      print *, '-nlevs NLEVS  where NLEVS is the number of vertical levs out'
      print *, '-ntrac NTRAC  where NTRAC is the number of tracers (both qwv and o3)'
      print *
      print *, ' NOTE: d_rst and p_rst represent the dynamics and physics restart files '
      print *, '       (these can have arbitrary names); sbcs represent the surface '
      print *, '       boundary condition file, typically: surf_r1.data_IMxJM.usgs  '
      print *
      print *
      print *, ' Last updated: 30 Sep 2004; Todling '
      print *
      call exit(1)
      end subroutine usage
      
!-------------------------------------------------------------------------
   subroutine get_fvrst ( drsfile, w_f, nymd, nhms, nstep )
!-------------------------------------------------------------------------
   use m_ioutil, only : luavail
   implicit none
   character(len=*), parameter :: myname = 'get_fvrst'
   character*255  :: drsfile
   type(dyn_vect), intent(inout) :: w_f
   integer,        intent(out)   :: nymd,nhms,nstep

   integer :: im,jm,km,lm
   integer :: i ,j ,k ,l, lu 
  
   lu = luavail()
   im = w_f%grid%im
   jm = w_f%grid%jm
   km = w_f%grid%km
   lm = w_f%grid%lm
   open ( unit=lu, file = drsfile, form = 'unformatted' )
   read (lu) nstep,nymd,nhms
   read (lu) w_f%ps,w_f%delp,w_f%u,w_f%v,w_f%pt
   do l = 1, w_f%grid%lm
      read(lu) (((w_f%q(i,j,k,l),i=1,im),j=1,jm),k=1,km)
   end do
   close (lu)

   end subroutine get_fvrst

!-------------------------------------------------------------------------
   subroutine get_phys ( phyfile, im, jm, lwi, ts )
!-------------------------------------------------------------------------
   use m_ioutil, only : luavail
   implicit none
   character(len=*), parameter :: myname = 'get_phys'
   character(len=*), intent(in):: phyfile
   integer, intent(in)  :: im, jm
   real,    intent(out) :: lwi(im,jm)        
   real,    intent(out) :: ts (im,jm)        

   integer :: i, j, lu, irec

   lu = luavail()
   open ( unit=lu, file = phyfile, form = 'unformatted' )
   read (lu) lwi
   do irec = 2, 17
      read(lu) ts  ! ts is the 17-th record in the p_rst file
   end do
   close (lu)

   end subroutine get_phys

!-------------------------------------------------------------------------
   subroutine get_bcs ( bcsfile, im, jm, phis, hs_stdv )
!-------------------------------------------------------------------------
   use m_ioutil, only : luavail
   implicit none
   character(len=*), parameter :: myname = 'get_bcs'
   character(len=*), intent(in):: bcsfile
   integer, intent(in)  :: im, jm
   real,    intent(out) :: phis   (im,jm)
   real,    intent(out) :: hs_stdv(im,jm)

   integer :: i, j, lu

   lu = luavail()
   open ( unit=lu, file = bcsfile, form = 'unformatted' )
   read(lu) phis
   read(lu) hs_stdv
   hs_stdv = 2. * hs_stdv   ! see readsrf in FVGCM
   close (lu)

   end subroutine get_bcs

   end program drs2dyn



