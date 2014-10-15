
      program maph_pert

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: maph_pert: Remap perturbation to a user-defined resolution 
!
! !INTERFACE:
!
!     See usage()
!
! !USES:
!
      use  m_dyn
      use  m_dynp
      use  m_maph_pert  
      use m_set_eta, only : set_eta

      implicit NONE

! !DESCRIPTION:  This application nterpolates a state perturbation to
!                requested resolution.
!
! !REVISION HISTORY:
!
!  14nov2002  Dee       Generalizes sbc.f
!  28Jul2003  RT/Dee    Making sure b_rst is written to a new file
!  07Apr2004  Todling   Added realp option to handle pert in delp.
!  25Mar2005  Todling   Added pureadd option
!  27Apr2006  Elena N.  Adopted dynp.f90 to create a stand-alone utility
!
!EOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname = 'maph_pert'

!     Local variables
!     ---------------
      integer :: rc, freq
      integer :: nymd_p, nhms_p
      integer :: in, jn
      logical :: fexists, zero
      logical :: verb, phis_flag, ts_flag

      character(len=255) :: dyn_pin       ! perturbation input file name
      character(len=255) :: dyn_pout      ! perturbation output file name

!     Dynamics vectors
!     ----------------
      type (dyn_vect) :: dw               ! perturbation
      type (dyn_vect) :: dw_in

      real, pointer ::   ak(:)
      real, pointer ::   bk(:)

      integer prec, ks
      real*8  ptop, pint

      prec = 0    ! 32-bit output files
      verb = .false.
      phis_flag = .false.
      ts_flag = .false.

!.......................................................................


!     Parse command line
!     ------------------
      call Init_ ()

!     Define state perturbation
!     -------------------------
      inquire ( file=trim(dyn_pin), exist=fexists )
      if ( .not.fexists ) then
              print *, myname//': dyn_get failed with rc=', rc
              call die ( myname, 'could not read perturbation from file '//trim(dyn_pin) )
      endif

!          Read from file
!          --------------
           call dyn_get ( dyn_pin, nymd_p, nhms_p, dw_in, rc, freq=freq )
           if ( rc .eq. 0 ) then
                print *, myname//': read perturbation from file '//trim(dyn_pin)
           else
                print *, myname//': dyn_get failed with rc=', rc
                call die ( myname, 'could not read perturbation from file '//trim(dyn_pin) )
           end if

!---------------------------------------------
!   Check that perturbations are interpolated:
!---------------------------------------------
      allocate (   ak(dw_in%grid%km+1)       )
      allocate (   bk(dw_in%grid%km+1)       )

      call set_eta ( dw_in%grid%km, ks, ptop, pint, ak, bk )

      call dyn_null (dw)

      call dyn_init ( in, jn, dw_in%grid%km, dw_in%grid%lm, dw, rc, ptop, ks, ak, bk )

      if ((dw_in%grid%im.ne.in).or.  &
          (dw_in%grid%jm.ne.jn) ) then

          print *, 'NEED TO h-INTERPOLATE PERT VECTOR'

          call h_map_pert ( dw_in, dw, rc, verbose = verb, phis_yes = phis_flag, ts_yes = ts_flag ) 

          call dyn_put ( trim(dyn_pout), nymd_p, nhms_p, prec, dw, rc, new=.true., freq=freq )
      else
          print *, 'REQUESTED RESOLUTION IS THE SAME. NOTHING TO DO'  
      endif


!     De-allocate state and perturbation
!     ----------------------------------

      deallocate (   bk )
      deallocate (   ak )

      call dynp_clean ( dw )
      call dynp_clean ( dw_in )


      print *
      print *, 'maph_pert: it''s been a pleasure'
      print *, '--------------------------'
      print *

      call exit(0)

!...................................................................................

      CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: init_ --- Initialize dynp
!
! !INTERFACE:
!
      subroutine init_ ()

! !USES:

      implicit NONE

! !DESCRIPTION: Parses command line.
!
! !REVISION HISTORY:
!
!     11nov2002   Dee       Initial code.
!     07Apr2004   Todling   Added realp option to handle pert in delp.
!
!EOP
!-----------------------------------------------------------------------

      integer i, iarg, argc, iargc
      character(len=255) argv

      logical pin, lon_res, lat_res, pick

      pin  = .false.
      lon_res = .false.
      lat_res = .false.
      pick = .false.
      nymd_p = 19980101
      nhms_p = 000000

      dyn_pin = 'NONE'
      dyn_pout = 'maph_pert.nc4'

      argc =  iargc()
      if ( argc .lt. 1 ) call usage_()

      iarg = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) then
              exit
         endif
         call GetArg ( iArg, argv )
         if (index(argv,'-op' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, dyn_pout )
         else if (index(argv,'-p' ) .gt. 0 ) then
            pin = .true.
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, dyn_pin )
         else if (index(argv,'-verbose' ) .gt. 0 ) then
            verb = .true.
         else if (index(argv,'-hs' ) .gt. 0 ) then
            phis_flag = .true.
         else if (index(argv,'-ts' ) .gt. 0 ) then
            ts_flag = .true.
         else if (index(argv,'-lon' ) .gt. 0 ) then
            lon_res = .true.
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) in 
         else if (index(argv,'-lat' ) .gt. 0 ) then
            lat_res = .true.
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) jn
         else if (index(argv,'-pick' ) .gt. 0 ) then
            pick = .true.
            if ( iarg+2 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) nymd_p
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) nhms_p
         end if
      end do

      if ( .not. pin) then
         print *, myname, ': missing perturbation input file name'
         call usage_()
      end if
      if ( (.not. lon_res).or.(.not. lat_res)) then
         print *, myname, ': missing resolution [-lon lon_num -lat lat_num] info'
         call usage_() 
      end if


      print *
      print *, '-------------------------------------------'
      print *, 'maph_pert: application to interpolate horizontally a perturbation'
      print *, '-------------------------------------------'
      print *
      print *, 'Requested resolution:  ', in, ' x ', jn 
      print *, 'perturbation input file name:          ', trim(dyn_pin)
      print *, 'output file for remapped perturbation: ', trim(dyn_pout)
      print *

      end subroutine Init_

!.......................................................................

      subroutine usage_()
      print *
      print *, 'dynp.x produces dw - an fvgcm perturbation vector,'
      print *, ' with a user-requested resolution '
      print *
      print *, 'USAGE:  '
      print *
      print *, '   maph_pert.x -lon lon_num -lat lat_num -p dyn_p'
      print *, '        [-op dyn_op] [-pick YYYYMMDD HHMMSS]'
      print *, '        [-verbose] [-phis] [-ts]'
      print *
      print *, 'where'
      print *
      print *, '   -lon lon_num      lon resolution'
      print *
      print *, '   -lat lat_num      lat resolution'
      print *
      print *, '   -p dyn_p          perturbation input file name'
      print *
      print *, '   -op dyn_pout     (optional) output file for remapped perturbation'
      print *, '                    (default: maph_pert.nc4)'
      print *
      print *, '   -pick YYYYMMDD HHMMSS    (optional) timestamp of remapped perturbation'
      print *, '                    (default: 19980101 000000)'
      print *
      print *, '   -verbose         (optional) to echo interpolation steps'
      print *
      print *, '   -phis            (optional) to interpolate both surface geopotential and hs_stdv'
      print *
      print *, '   -ts              (optional) to interpolate surface skin temperature'


      call die ( 'maph_pert.x','not enough arguments' )
      end subroutine usage_

!.......................................................................

      subroutine die ( myname, msg )
      character(len=*) :: myname, msg
      print *
      print *, trim(myname) // ': ' // trim(msg)
      print *
      call exit(1)
      end subroutine die
!.......................................................................

      end program maph_pert 
