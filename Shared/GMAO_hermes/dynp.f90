
      program dynp

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: dynp:  Add/subtract a state perturbation
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

! !DESCRIPTION:  This application adds/subtracts a state perturbation to/from
!                a dynamics state.
!
! !REVISION HISTORY:
!
!  14nov2002  Dee       Generalizes sbc.f
!  28Jul2003  RT/Dee    Making sure b_rst is written to a new file
!  07Apr2004  Todling   Added realp option to handle pert in delp.
!  25Mar2005  Todling   Added pureadd option
!  27Apr2006  Elena N.  Added a check if the perturbation is of the same resolution as the state vector
!                       If not, the perturbation will be interpolated to the resolution of the state vector 
!  06Mar2014  Todling   Add pncf to allow read of GSI non-complaint dyn-vector perturbation files
!  07May2014  Todling   Correct implementation of interpolation of dw-res to state-resolution
!
!EOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname = 'dynp'

!     Local variables
!     ---------------
      integer :: rc, freq, dyntype
      integer :: nymd_s, nhms_s, nymd_p, nhms_p
      logical :: fexists, zero, interp, pncf

      character(len=255) :: pertype
      integer :: ks
      integer :: imi, jmi
      real ::  ptop, pint
      real, pointer ::   ak(:)
      real, pointer ::   bk(:)

      character(len=255) :: dyn_sin       ! state input file name
      character(len=255) :: dyn_sout      ! state output file name
      character(len=255) :: dyn_pin       ! perturbation input file name
      character(len=255) :: dyn_pout      ! perturbation output file name

!     Dynamics vectors
!     ----------------
      type (dyn_vect) :: w                ! state
      type (dyn_vect) :: dw               ! perturbation
      type (dyn_vect) :: dwi              ! interpolated perturbation

      real pcoef                          ! perturbation coefficient
      real pscal                          ! perturbation coefficient

      logical :: pick   = .false.         ! specific time
      logical :: stats  = .false.         ! print state statistics
      logical :: strict = .false.         ! error if no perturbation file
      logical :: realp  = .false.         ! when .t. treats dw%delp as perturbation
      logical :: anainc = .false.         ! when .t. treats perturbation as analysis increment
      logical :: pureadd= .false.         ! when .t. simply adds pert to state (no remap)
      logical :: twoperts= .false.        ! when .t. expects both inputs to be perturbations

       pcoef = 1.0d0
       pscal = 1.0d0

!.......................................................................


!     Parse command line
!     ------------------
      call Init_ ()

      if ( pick ) then   ! specific time requested
           nymd_p = nymd_s
           nhms_p = nhms_s
      end if

!     Read dynamics vector from file
!     ------------------------------
      if ( pick ) then   ! specific time
         call dyn_get ( dyn_sin, nymd_s, nhms_s, w, rc, timidx=0, freq=freq, vectype=dyntype )
      else               ! latest
         call dyn_get ( dyn_sin, nymd_s, nhms_s, w, rc, freq=freq, vectype=dyntype )
      end if

      if ( rc .eq. 0 ) then
           print *, myname//': read state from file '//trim(dyn_sin)
           print *, myname//': timestamp is nymd, nhms = ', nymd_s, nhms_s
      else
           print *, myname//': dyn_get failed with rc=', rc
           call die ( myname, 'could not read state from file '//trim(dyn_sin) )
      end if


!     Define state perturbation
!     -------------------------
      inquire ( file=trim(dyn_pin), exist=fexists )
      if ( fexists ) then

!          Read from file
!          --------------
           if ( pick ) then   ! specific time
              call dyn_get ( dyn_pin, nymd_p, nhms_p, dwi, rc, timidx=0, freq=freq, vectype=dyntype, pncf=pncf )
           else               ! latest
              call dyn_get ( dyn_pin, nymd_p, nhms_p, dwi, rc, freq=freq, vectype=dyntype, pncf=pncf )
           end if
           if ( rc .eq. 0 ) then
                print *, myname//': read perturbation from file '//trim(dyn_pin)
                print *, myname//': timestamp is nymd, nhms = ', nymd_p, nhms_p
                zero = .false.
           else
                print *, myname//': dyn_get failed with rc=', rc
                call die ( myname, 'could not read perturbation from file '//trim(dyn_pin) )
           end if
      else if ( strict ) then

           call die ( myname,'file '//trim(dyn_pin)//' does not exist' )

      else

!          Initialize perturbation (zero all fields)
!          -----------------------------------------
           call dynp_init ( w, dw, rc )
           zero = .true.
           if ( rc .eq. 0 ) then
                print *, myname//': set perturbation fields to zero'
           else
                print *, myname//': dynp_init failed with rc=', rc
                call die ( myname, 'could not initialize perturbation' )
           end if

      end if

!     Check that number of levels in state vector and perturbation are the same
!     -------------------------------------------------------------------------
      if ( dwi%grid%km .ne. w%grid%km ) then
               print *, myname//': levels in state vector and in perturbation do not match'
               call die ( myname, 'could not add perturbation with different number of vertical levels' )
      endif

!     Check that perturbations are interpolated
!     -----------------------------------------
      allocate (   ak(dwi%grid%km+1)       )
      allocate (   bk(dwi%grid%km+1)       )

      call set_eta ( dwi%grid%km, ks, ptop, pint, ak, bk )

      call dyn_null (dw)

      call dyn_init ( w%grid%im, w%grid%jm, w%grid%km, dwi%grid%lm, dw, rc, ptop, ks, ak, bk, vectype=dyntype )

      if ((dwi%grid%im.ne.w%grid%im)   .or. &
          (dwi%grid%jm.ne.w%grid%jm) ) then

          call h_map_pert ( dwi, dw, rc )
          print *, myname, ': interpolated perturbation to state vector grid'

      else
          dw = dwi
      endif
      call dynp_clean ( dwi )

      deallocate (   bk )
      deallocate (   ak )

!     Check that perturbations are interpolated
!     ---------------------------------------------
      if ( stats ) then   ! Print some stats

           print *
           print *, myname//'************************************************'
           print *, myname//': summary stats for state prior to perturbation:'
           print *, myname//'************************************************'
           print *
           call dyn_stat ( 6, w, rc )

           print *
           print *, myname//'************************************************'
           print *, myname//': summary stats for perturbation prior to remap:'
           print *, myname//'************************************************'
           print *
           call dyn_stat ( 6, dw, rc )

      end if


      if ( .not. zero ) then

!          If dw%delp is a perturbed field treat it as such
!          ------------------------------------------------
           if ( realp ) then

              if ( .not. pureadd ) then

                   call dynp_add_surf ( w, pcoef, dw, rc ) 

                      if ( rc .eq. 0 ) then
                           print *, myname//': redefined state vector vertical coord w/ delp pert'
                      else
                           print *, myname//': dynp_add_surf failed with rc=', rc
                           call die ( myname, 'could not add delp perturbation' )
                      end if

              else

                                   call dynp_set ( 'gcmPERT'  , .true. )
                                   call dynp_set ( 'skipSHAVE', .true. )
                     if ( anainc ) call dynp_set ( 'anaPERT'  , .true. ) ! in case of dealing w/ analysis increments
                     if (twoperts) call dynp_set ( 'allPERT'  , .true. ) ! in case both input fields are perturbations

              endif  ! < pureadd >

           endif ! < realp >

!          Remap perturbation to state vertical coordinate
!          -----------------------------------------------
           if ( .not. pureadd ) then

              call dynp_remap ( dw, w, rc, verbose=.true. )

              if ( rc .eq. 0 ) then
                   print *, myname//': remapped perturbation to state vertical coordinate'
              else
                   print *, myname//': dynp_remap failed with rc=', rc
                   call die ( myname, 'could not remap perturbation' )
              end if

           end if ! < pureadd >

!          Add/subtract perturbation
!          -------------------------
           call dynp_add ( w, pcoef, dw, rc, verbose=.true. )

           if ( rc .eq. 0 ) then
                print *, myname//': perturbed the state'
           else
                print *, myname//': dynp_add failed with rc=', rc
                call die ( myname, 'could not perturb the state' )
           end if

      end if

      if ( stats ) then   ! Print some stats

           print *
           print *, myname//'*********************************************'
           print *, myname//': summary stats for perturbation after remap:'
           print *, myname//'*********************************************'
           print *
           call dyn_stat ( 6, dw, rc )

           print *
           print *, myname//'*********************************************'
           print *, myname//': summary stats for state after perturbation:'
           print *, myname//'*********************************************'
           print *
           call dyn_stat ( 6, w, rc )

      end if

      if ( trim(dyn_pout) .ne. 'NONE' ) then

!          If so, interpolate output perturbation to desired resolution
!          ------------------------------------------------------------
           if ( interp .and. ( (dw%grid%im.ne.imi)   .or. &
                               (dw%grid%jm.ne.jmi) ) ) then

!               Initialize dimension of vertically interpolated vector
!               -------------------------------------------------------
                call dyn_init ( imi, jmi, dw%grid%km, dw%grid%lm, dwi, rc, &
                                dw%grid%ptop, dw%grid%ks, dw%grid%ak, dw%grid%bk )

                   if ( rc/=0 ) then
                        call die ( myname, 'error initializing perturbation on way out ' )
                   endif

!               Interpolate perturbation now
!               ----------------------------
                call h_map_pert ( dw, dwi, pertype, rc )
                   if ( rc/=0 ) then
                        call die ( myname, 'error interpolating perturbation on way out ' )
                   else
                        print *, trim(myname), ': interpolated perturbation on way out'
                   endif

!               Write remapped perturbation to file, with state's timestamp
!               -----------------------------------------------------------
                call dynp_scale ( dwi, pscal )
                call dyn_put ( trim(dyn_pout), nymd_s, nhms_s, 0, dwi, rc, new=.true., freq=freq, vectype=dyntype )

!               Clean up
!               --------
                call dynp_clean ( dwi )

           else  ! output perturbation without interpolation

!               Write remapped perturbation to file, with state's timestamp
!               -----------------------------------------------------------
                call dynp_scale ( dw, pscal )
                call dyn_put ( trim(dyn_pout), nymd_s, nhms_s, 0, dw, rc, new=.true., freq=freq, vectype=dyntype )
 
           endif

           if ( rc .eq. 0 ) then
                print *, myname//': wrote perturbation to file '//trim(dyn_pout)
           print *, myname//': timestamp is nymd, nhms = ', nymd_s, nhms_s
           else
                print *, myname//': dyn_put failed with rc=', rc
                call die ( myname, 'could not write perturbation to file '//trim(dyn_pout) )
           end if

      end if


      if ( trim(dyn_sout) .ne. 'NONE' ) then

!          If so, interpolate output perturbation to desired resolution
!          ------------------------------------------------------------
           if ( interp .and. ( (w%grid%im.ne.imi)   .or. &
                               (w%grid%jm.ne.jmi) ) ) then

!               Initialize dimension of vertically interpolated vector
!               -------------------------------------------------------
                call dyn_null ( dwi )
                call dyn_init ( imi, jmi, w%grid%km, w%grid%lm, dwi, rc, &
                                w%grid%ptop, w%grid%ks, w%grid%ak, w%grid%bk, vectype=dyntype )
                   if ( rc/=0 ) then
                        call die ( myname, 'error initializing perturbation on way out ' )
                   endif

!               Interpolate perturbation now
!               ----------------------------
                call h_map_pert ( w, dwi, pertype, rc )
                   if ( rc/=0 ) then
                        call die ( myname, 'error interpolating perturbation on way out ' )
                   else
                        print *, trim(myname), ': interpolated perturbation on way out'
                   endif

!               Write remapped perturbation to file, with state's timestamp
!               -----------------------------------------------------------
                call dynp_scale ( dwi, pscal )
                call dyn_put ( trim(dyn_sout), nymd_s, nhms_s, 0, dwi, rc, freq=freq, vectype=dyntype )

!               Clean up
!               --------
                call dynp_clean ( dwi )

           else  ! output state without interpolation

!              Write perturbed state to file
!              -----------------------------
               call dynp_scale ( w, pscal )
               call dyn_put ( trim(dyn_sout), nymd_s, nhms_s, 0, w, rc, freq=freq, vectype=dyntype )
    
               if ( rc .eq. 0 ) then
                    print *, myname//': wrote state to file '//trim(dyn_sout)
                    print *, myname//': timestamp is nymd, nhms = ', nymd_s, nhms_s
               else
                    print *, myname//': dyn_put failed with rc=', rc
                    call die ( myname, 'could not write state to file '//trim(dyn_sout) )
               end if

           end if

      end if


!     De-allocate state and perturbation
!     ----------------------------------
      call dyn_clean ( w )
      call dynp_clean ( dw )


      print *
      print *, 'dynp: it''s been a pleasure'
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
!     24Aug2007   Todling   Added -g5 opt (support to geos-5 dynvect)
!     28Aug2008   Todling   Added -gradient opt
!     29Feb2008   Todling   Bug fix: index function creates redundancies
!     21Apr2009   Todling   Update defaults resolution for GEOS-5
!     19Jul2010   Todling   Update GEOS-5 d-/e-resolutions to GEOS-4-like
!     06Mar2014   Todling   Add pncf for non-compliant perturbation files
!
!EOP
!-----------------------------------------------------------------------

      integer i, iarg, argc, iargc, ires
      character(len=255) argv

      logical sin, sout, pin, pout, geos4res, gsires

      character(len=1) :: res
      integer, dimension(5), parameter :: IMS4 = (/ 72, 144, 288, 576, 1000 /)
      integer, dimension(5), parameter :: IMS5 = (/ 72, 144, 288, 576, 1152 /)
      integer, dimension(5), parameter :: JMS  = (/ 46,  91, 181, 361,  721 /)

      sin  = .false.
      pin  = .false.
      interp = .false.
      dyntype = 4
      geos4res = .false.
      gsires = .false.
      pertype = 'tlm'
      pncf = .false.

      dyn_sout = 'NONE'
      dyn_pout = 'NONE'

      argc =  iargc()
      if ( argc .lt. 1 ) call usage_()

      iarg = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iArg, argv )
         select case ( argv )
         case ("-pick")
            pick = .true.
            if ( iarg+2 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) nymd_s
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) nhms_s
         case ("-realp")
            realp = .true.
         case ("-ainc")
            anainc = .true.
         case ("-g5")
            dyntype = 5
         case ("-stats")
            stats = .true.
         case ("-pncf")
            pncf = .true.
         case ("-pureadd")
            pureadd = .true.
         case ("-twoperts")
            twoperts = .true.
         case ("-strict")
            strict = .true.
         case ("-geos4")
            geos4res = .true.
         case ("-gsi")
            gsires = .true.
         case ("-adm")
            pertype = 'adm'
         case ("-res")
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, res )
            select case (res)
              case ("a")
                    ires=1
              case ("b")
                    ires=2
              case ("c")
                    ires=3
              case ("d")
                    ires=4
              case ("e")
                    ires=5
              case default
                    print *, 'Sorry this resolution not supported'
                    call exit(1)
            end select
            interp = .true.
         case ("-os")
            sout = .true.
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, dyn_sout )
         case ("-op")
            pout = .true.
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, dyn_pout )
         case ("-s")
            sin = .true.
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, dyn_sin )
         case ("-p")
            pin = .true.
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, dyn_pin )
         case ("-a")
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) pcoef
         case ("-scale")
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) pscal
         end select
      end do

      if ( .not. sin ) then
         print *, myname, ': missing dynamics state input file name'
         call usage_()
      end if
      if ( .not. pin ) then
         print *, myname, ': missing perturbation input file name'
         call usage_()
      end if

      if ( geos4res .and. gsires ) call usage_ ()

      if ( interp ) then
           if ( geos4res ) then
             imi = ims4(ires)
           else
             imi = ims5(ires)
           endif
           jmi = jms (ires)
      endif

      print *
      print *, '-------------------------------------------'
      print *, 'dynp: application for fv state perturbation'
      print *, '-------------------------------------------'
      print *
      print *, 'dynamics state input file name:        ', trim(dyn_sin)
      if ( pick ) then
      print *, 'time stamp:                            ', nymd_s, nhms_s
      else
      print *, 'time stamp:                            last on file'
      end if
      print *, 'perturbation input file name:          ', trim(dyn_pin)
      if ( pick ) then
      print *, 'time stamp:                            ', nymd_s, nhms_s
      else
      print *, 'time stamp:                            last on file'
      end if
      print *, 'perturbation coefficient:              ', pcoef
      print *, 'will scale perturbation by:            ', pscal
      print *, 'output file for perturbed state:       ', trim(dyn_sout)
      print *, 'output file for remapped perturbation: ', trim(dyn_pout)
      print *

      end subroutine Init_

!.......................................................................

      subroutine usage_()
      print *
      print *, 'dynp.x produces w + a*dw, where w is an fvgcm state vector,'
      print *, 'a is a scalar, and dw is an fvgcm state perturbation. The '
      print *, 'operation ''+'' involves:'
      print *
      print *, '  - remapping the vertical coordinate of dw to that of w,'
      print *, '  - perturbing the 3D layer-mean fields of w, and'
      print *, '  - changing the mass in the lowest layers.'
      print *
      print *, 'See comments in m_dynp.f90 for more detail.'
      print *
      print *, 'USAGE:  '
      print *
      print *, '   dynp.x -s dyn_s -p dyn_p -a pcoef'
      print *, '          [-pick nymd nhms] [-os dyn_os] [-op dyn_op]'
      print *, '          [-stats] [-strict] [-realp] [-pureadd]'
      print *, '          [-res RES] [-strict] [-realp] [-pureadd]'
      print *
      print *, 'where'
      print *
      print *, '   -s dyn_s          dynamics state input file name'
      print *
      print *, '   -p dyn_p          perturbation input file name'
      print *
      print *, '   -a pcoef          perturbation coefficient (default: 1.0)'
      print *
      print *, '   -scal  scale      scaling factor for the output perturbation (default: 1.0)'
      print *
      print *, '   -pick nymd nhms   date and time'
      print *, '                    (default: use latest on file)'
      print *
      print *, '   -os dyn_sout      output file for perturbed state,'
      print *, '                     or no output if dyn_sout=''NONE'''
      print *, '                    (default: NONE)'
      print *
      print *, '   -op dyn_pout      output file for remapped perturbation'
      print *, '                     with timestamp of dynamics state,'
      print *, '                     or no output if dyn_pout=''NONE'''
      print *, '                    (default: NONE)'
      print *
      print *, '   -stats            print summary state statistics'
      print *, '                    (default: don''t)'
      print *
      print *, '   -strict           abort if no perturbation input file'
      print *, '                    (default: set perturbation=0 if dyn_p not found)'
      print *
      print *, '   -realp            specify when perturbation file contains pert delp'
      print *, '                    (default: expects fully defined delp)'
      print *
      print *, '   -anic             treats perturbation as analysis increment'
      print *
      print *, '   -pureadd          simply adds pert to state w/o remapping '
      print *, '                    (default: do remapping)'
      print *
      print *, '   -g5               use to read/write GEOS-5 files'
      print *
      print *, '   -geos4            used to set resolution compliant to GEOS-4'
      print *, '                     (ignored if not interpolating fields)'
      print *
      print *, '   -gsi              used to set resolution compliant to GSI'
      print *
      print *, '   -adm              treat an adjoint fields'
      print *
      print *, '   -res  RES         determines resolution of output perturbation'
      print *, '                     (RES defined as a,b,c,d,or e)'

      print *
      print *
      call die ( 'dynp.x','not enough arguments' )
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

      end program dynp
