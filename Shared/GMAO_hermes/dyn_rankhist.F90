      program dyn_rankhist

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: dyn_rankhist: rank-histogram dyn-vector
!
! !USAGE: see the routine usage() below
!
! !USES:
!
      use m_dyn, only: dyn_init
      use m_dyn, only: dyn_getdim
      use m_dyn, only: dyn_get
      use m_dyn, only: dyn_put
      use m_dyn, only: dyn_stat
      use m_dyn, only: dyn_vect
      use m_dyn, only: dyn_clean

      use m_maph, only : h_map
      use m_maph_pert, only: h_map_pert

      use m_random, only: zufalli
      use m_random, only: normalen

      use m_die, only: die
      implicit NONE

! !DESCRIPTION: Generated rank histogram from ensemble members and central.
!
! !REVISION HISTORY:
!
!  11jan2012   Todling    Initial code
!
! !REMRKS:
!   1) see TDB (to-be-done) along the code
!
!-------------------------------------------------------------------------
!EOP

      character(len=*), parameter :: myname = 'dyn_rankhist'

!     File names
!     ----------
      integer, parameter :: MFILES = 2   ! at least 3 files as input
      integer, parameter :: stride = 1   ! TBD: ought do better than this  
      character(len=255) :: files(MFILES)! hold names of files involved

      character(len=255) :: dyn_out      ! difference output file name
      character(len=255) :: fname        ! aux filename


!     Dynamics/simulator vectors
!     --------------------------
      type(dyn_vect) x_e  ! ensemble member
      type(dyn_vect) x_c  ! central analysis or background
      type(dyn_vect) x_x  ! extra type used when interpolation needed

      integer,allocatable :: hist(:)
      real,allocatable :: f_c(:,:)
      real,allocatable :: f_e(:,:,:)

!     Locals
!     ------
      integer, parameter :: READ_ONLY = 1
      integer lev, members, nfiles
      integer fid, nvars, ngatts
      integer rc
      integer ntimes, freq, nymd, nhms
      integer im, jm, km, lm, dyntype, ii, jj, nm
      integer imc, jmc, kmc, lmc
      integer j1, j2
      integer seed,icnt
      real    plev,mean, sigma
      logical verbose
      character(len=1) var
      character(len=3) reg
      character(len=80) fnout
      
!  Initialize
!  ----------
   call Init_ ( dyntype, mfiles, files, members, seed, mean, sigma, &
                plev, var, reg, verbose )

!  Determine how many time levels in file
!  --------------------------------------
   call GFIO_Open ( files(1), READ_ONLY, fid, rc )
   if ( rc .ne. 0 ) then
      call die(myname,'cannot open GFIO file '//trim(files(1)))
   end if
   call GFIO_DimInquire ( fid, imc, jmc, kmc, ntimes, nvars, ngatts, rc)
   if ( rc .ne. 0 ) then
      call die(myname,'problems getting dimensions' )
   end if
   call GFIO_Close ( fid, rc )

   if (ntimes>1) then
      call die(myname,'cannot handle files with multiple times')
   endif
   call dyn_getdim ( files(1), imc, jmc, kmc, lmc, rc ) ! as foolish as it seems this gives slightly diff info than what's above
   
   write(fname,'(a,i3.3,2a)') 'mem', 1, '/', trim(files(2)) ! somewhat wired for now
   call dyn_getdim ( fname, im, jm, km, lm, rc )
   if (rc/=0) then
       call die(myname,'cannot read first member fields ')
   end if
      
!  If not just testing ...
!  -----------------------
   if ( seed<0 ) then

!  Get first dyn-vector (central ana/bkg)
!  --------------------------------------
     if ( im/=imc .and. jm/=jmc ) then

!        Read hi-res state vector
!        ------------------------
         call dyn_get ( files(1), nymd, nhms, x_x, rc, timidx=1, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read central fields')
         end if

!        Initialize dimension of output (interpolated) vector
!        ----------------------------------------------------
         call dyn_init ( im, jm, km, lm, x_c, rc, &
                         x_x%grid%ptop, x_x%grid%ks, x_x%grid%ak, x_x%grid%bk, vectype=dyntype )
              if ( rc/=0 ) then
                   call die (myname, ': Error initializing dyn vector(x_c)')
              endif
 
!        Remap to required resolution
!        ----------------------------
         x_c%grid%lm = min(x_c%grid%lm,x_x%grid%lm) ! fix lm
         x_x%grid%lm = x_c%grid%lm ! fix lm
         call h_map_pert ( x_x, x_c, rc )
              if ( rc/=0 ) then
                   call dyn_clean ( x_x )
                   call dyn_clean ( x_c )
                   print *, 'remap error code = ', rc
                   call die(myname,' failed to remap')
              else
                   call dyn_clean ( x_x )
              endif
         x_c%grid%lm = lm ! reset lm-dim(x_c)
         print*, myname, ': interpolated member to desired resolution'
         print*, myname, ': from ', imc, 'x', jmc, ' to ', im, 'x', jm


     else

!        Read state vector
!        -----------------
         call dyn_get ( files(1), nymd, nhms, x_c, rc, timidx=1, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read central fields')
         end if

     endif
     if(verbose) print *, 'Read: ', trim(files(1))

!    Determine level
!    ---------------
     call find_level_(x_c,plev,lev)
   endif

!  Allocate fld
!  ------------
   allocate(f_c(im,jm),f_e(im,jm,members))
   j1=1;j2=jm
  
   if(seed>0) then
      print *, ' Running random test only ... '
      print *, ' Normal distribution mean & stdv: ', mean, sigma
      call zufalli (seed)       ! initialize random number generator
!     call random_seed()
   endif

!  Extract desired field and level from this central
!  -------------------------------------------------
   call getfield_ (var,x_c,f_c,0.0,1.0)

!  Clean up
!  --------
   call dyn_clean ( x_c )

   if(seed>0) then
      print *, ' Running random test only ... '
      print *, ' Normal distribution mean & stdv: ', mean, sigma
      call zufalli (seed+5779)      ! initialize random number generator
   endif

!  Loop over members
!  -----------------
   do nm = 1, members

!     Get ensemble member
!     -------------------
      if (seed<0) then ! need to read only once when testing
         write(fname,'(a,i3.3,2a)') 'mem', nm, '/', trim(files(2)) ! somewhat wired for now
         call dyn_get ( fname, nymd, nhms, x_e, rc, timidx=1, freq=freq, vectype=dyntype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read ensemble member file')
         end if
         if(verbose) print *, 'Read: ', trim(fname)
      endif

!     Extract desired field and level from this member
!     ------------------------------------------------
      call getfield_ (var,x_e,f_e(:,:,nm),mean,sigma)

!     Clean up mean
!     -------------
      if (seed<0) then

!        Define region
!        -------------
         call def_region_ (reg,j1,j2,x_e%grid%lat)

!        Clean up
!        --------
         call dyn_clean (x_e) 
      endif

   enddo

!  For each grid-point ...
!  -----------------------
   allocate(hist(members+1))
   hist=0
   icnt=0
   do jj = j1, j2, stride
    do ii = 1, im, stride
       icnt=icnt+1

!      rank central and construct part of history
!      ------------------------------------------
       call rhist_ (hist,f_c(ii,jj),f_e(ii,jj,:))

    enddo
   enddo

!  Show histogram
!  --------------
   if (sum(hist)/=icnt) then
      print*, 'Error: something is not adding up'
      print*, 'sum(hist)     = ', sum(hist)
      print*, 'total samples = ', icnt
      call exit(1)
   endif
   print *, 'total number of samples: ', icnt
   write(fnout,'(2a,i3.3,2a)')  &
         trim(var),'_',nint(plev),'_',trim(reg)
   open(7,file=trim(fnout)//'_rankhist.txt')

   write(7,'(2(2x,a))') trim(fnout), trim(fnout)
   do nm=1,members+1
      write(7,'(i6,2x,f8.5)') nm, hist(nm)/float(icnt)
   enddo
   close(7)

!  If requested write *.hdf file with a header from dyn(1)
!  -------------
!  if ( trim(dyn_out) .ne. 'NONE' ) then
!       call dyn_put ( trim(dyn_out), nymd, nhms, 0, x_e, rc, freq=freq, vectype=dyntype )
!  else ! by default: overwrite input files
!       call dyn_put ( trim(files(1)), nymd, nhms, 0, x_e, rc, freq=freq, vectype=dyntype )
!  endif 

!  Clean up
!  --------
   deallocate(hist)
   deallocate(f_c,f_e)

!  All done
!  --------
   close(999)
   open (999,file='DYN_RANKHIST',form='formatted')
   close(999)
   call exit(0)

CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: Init_ --- Initialize dyn_rankhist
!
! !DESCRIPTION: parses command line.
!
! !INTERFACE:
!
      subroutine Init_ ( dyntype, mfiles, files, members, seed, mean, sigma, &
                         plev, var, reg, verbose )

      implicit NONE

      integer,       intent(out) :: dyntype ! 4=geos4, 5=geos5
      integer,       intent(in)  :: mfiles  ! max. number of eta files
                                            ! dynamics file names (eta)
      character*255, intent(out) :: files(mfiles) 
      real   , intent(out) :: mean          ! parameters of Normal
      real   , intent(out) :: sigma         !  distribution used in test
      integer, intent(out) :: seed          ! random number seed
      integer, intent(out) :: members       ! number of ensemble members
      real   , intent(out) :: plev          ! pressure level
      character(len=*), intent(inout) :: var
      character(len=*), intent(inout) :: reg
      logical, intent(out) :: verbose
      
!
! !REVISION HISTORY:
!    
!  30sep2011   Todling    Initial code
!
!EOP
!BOC

      character*4, parameter :: myname = 'init'

      integer i, iarg, argc, iargc
      character(len=255) :: argv

      files = 'NONE'
      dyn_out = 'NONE'
      dyntype  = 4        ! default: GEOS-4 files
      verbose = .false.
      members = 1
      seed = -1
      plev = 500. ! (hPa) approximately
      mean = 0.0
      sigma= 1.0
      var= "t"
      reg= "glo"

      print *
      print *, '     --------------------------------------'
      print *, '     dyn_rankhist - full-field rank history'
      print *, '     --------------------------------------'
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
           case ("-o")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, dyn_out )
           case ("-mem")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, argv )
             read(argv,*) members
           case ("-plev")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, argv )
             read(argv,*) plev
           case ("-normal")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, argv )
             read(argv,*) mean
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, argv )
             read(argv,*) sigma
           case ("-reg")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, reg )
           case ("-seed")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, argv )
             read(argv,*) seed
           case ("-var")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, var )
           case ("-h")
             if ( iarg+1 .gt. argc ) call usage()
           case ("-verbose")
             verbose =.true.
           case default
             nfiles = nfiles + 1
             if ( nfiles .gt. mfiles ) call usage()
             files(nfiles) = argv

         end select

      end do

      if ( nfiles .lt. 1 ) call usage()

!     Echo the parameters
!     -------------------
      if (verbose) then
         print *
         print *, '  Eta     Dynamics state files: '
         print *, '  ------------------------------'
         print *, 'central field:      ', trim(files(1))
         print *, 'ensemble members:   ', trim(files(2))
      endif

      end subroutine Init_

!.................................................................

      subroutine usage()
      print *
      print *,'Usage: '
      print *
      print *,'  dyn_rankhist.x [options] x_c x_e' 
      print *
      print *, 'where [options]'
      print *
      print *, '-h              Help (optional)'
      print *, '-g5             Treats files as GEOS-5 files'
      print *, '-mem    N       number of ensemble members'
      print *, '-normal M S     Where M=mean & S=stdv of normal dist for members'
      print *, '                (CAUTION: only used when seed specified)'
      print *, '-plev  LEVEL    pressure level (hPa)'
      print *, '-reg   REGION   glo(default), tro, nhe, or she'
      print *, '-seed  NUMBER   seed to test scheme with random numbers'
      print *, '                (see option -normal above)'
      print *, '-var   VAR      variable (t,u,v,q,s=speed)'
      print *, '-verbose        echoes general information'
!     print *, '-o       fname  filename of resulting recentered fields'
      print *
      print *, ' Required inputs:'  
      print *, '  x_c  - central analysis or background (observation)'
      print *, '  x_e  - filename of members'
      print *
      print *, ' Remarks: '
      print *
      call exit(1)
      end subroutine usage

!.................................................................
      subroutine getfield_ ( var, x, fld, mean, sigma )
      implicit none 
      type(dyn_vect) x
      character(len=*) var
      real  fld(:,:)
      real  mean,sigma ! N(mean,sigma)
      real,allocatable::xp(:)
      integer id,jd,i,j
      id = size(fld,1)
      jd = size(fld,2)
      if(seed>0) then
         allocate(xp(id*jd))
         call normalen(id*jd,xp)
         xp = mean + sigma*xp
         fld = reshape(xp,(/id,jd/))
         deallocate(xp)
      else
         if(var=="u") fld = x%u (:,:,lev)
         if(var=="v") fld = x%v (:,:,lev)
         if(var=="t") fld = x%pt(:,:,lev)
         if(var=="q") fld = x%q (:,:,lev,1)
         if(var=="s") then
            do j=1,jd
             do i=1,id
                fld(i,j) = x%u(i,j,lev)*2+x%v(i,j,lev)*2 ! leave sqrt out on purpose
             enddo
            enddo
         endif
      endif
      end subroutine getfield_

      subroutine find_level_(x,plev,lev)
      implicit none 
      type(dyn_vect) x
      real,allocatable::pres(:)
      real plev
      integer lev
      integer k
      allocate(pres(x%grid%km+1))
      pres = (x%grid%ak+x%grid%bk*100000.)/100. ! approx pressure levels
      lev=-1
      do k=1,x%grid%km+1
         if(plev<pres(k)) then
            lev=k
            exit
         endif
      enddo
      if(lev<0)then
         call die('find_level_','failed to determined level index')
      endif
      print *, 'Level: ',plev, ' hPa at index= ', lev
      deallocate(pres)
      end subroutine find_level_

      subroutine rhist_ (h,yc,y)
      use m_MergeSorts, only: IndexSet
      use m_MergeSorts, only: IndexSort
      implicit none
      integer h(:)
      real yc
      real y(:)
      integer,allocatable::indx(:)
      real   ,allocatable::yy(:)
      integer m,i
      m=size(y)
      allocate(indx(m+1))
      allocate(yy  (m+1))
      yy(1:m) = y
      yy(m+1) = yc

      ! Get index
      call IndexSet (m+1,indx)
      call IndexSort(m+1,indx,yy,descend=.false.)

      ! Sorting
      yy(1:m) = yy( (/ (indx(i),i=1,m) /) )
      do i=1,m+1
         if(yc==yy(i)) then
            h(i)=h(i)+1
            exit
         endif
      enddo

      deallocate(yy)
      deallocate(indx)
      end subroutine rhist_

      subroutine def_region_ (reg,j1,j2,lats)
      implicit none
      character(len=*) reg
      integer j1,j2
      integer j
      real    lats(:)
      jm=size(lats)
! quick and dirty region definition (need to use lat array)
      if (reg=='glo') then
         j1=1
         j2=jm
      endif
      if (reg=='nhe') then
         do j=1,jm
            if(lats(j)>30.0) then
              j1=j-1
              exit
            endif
         enddo
         j2=jm
      endif
      if (reg=='tro') then
         do j=1,jm
            if(lats(j)>-30.0) then
              j1=j
              exit
            endif
         enddo
         do j=1,jm
            if(lats(j)>30.0) then
              j2=j-1
              exit
            endif
         enddo
      endif
      if (reg=='she') then
         j1=1
         do j=1,jm
            if(lats(j)>-30.0) then
              j2=j-1
              exit
            endif
         enddo
      endif
      end subroutine def_region_

  end program dyn_rankhist
