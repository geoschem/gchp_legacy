
      program ibc_upd

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Ibc_Upd:  Incremental Bias Correction: The Application
!
! !INTERFACE:
!
!     See usage()
!
! !USES:
!
      use  m_dyn
      use  m_die
      use  m_zeit         ! timer
      use  m_FileResolv   ! file resolver
      use  m_mapz

      implicit NONE

! !DESCRIPTION: Given analysis and background, updates bias estimate.
!
! !REVISION HISTORY:
!
!  14dec2004  da Silva  First crack derived from sbc.f
!
!EOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname = 'ibc_upd'

!     Local variables
!     ---------------
      integer :: rc, lt, rcs(2)
      integer :: nymd_f, nhms_f, nymd_b, nhms_b
      integer :: freq, fsyn
      logical :: fexists  

      character(len=255) :: dyn_f         ! first guess file name
      character(len=255) :: dyn_a         ! analysis file name
      character(len=255) :: out_a         ! output analysis file name
      character(len=255) :: dyn_b         ! bias file name

!     Dynamics vectors
!     ----------------
      type (dyn_vect) :: w_f              ! first guess
      type (dyn_vect) :: w_a              ! shaved analysis in lcv
      type (dyn_vect) :: w_e              ! analysi in eta
      type (dyn_vect) :: b_f              ! first guess bias

      logical :: pick = .false., remap = .true., restart = .false.
      real alpha, a

      character(len=255) :: vars, str     ! vars

      logical upd 
      upd ( str ) = ( index((vars),(str)) .gt. 0 )

!.......................................................................

      call zeit_ci ( 'ibc_upd' )

!     Parse command line
!     ------------------
      call Init_ ( dyn_f, dyn_b, dyn_a, out_a, remap, restart, vars, alpha )

!     Load dynamics vector (latest time)
!     ---------------------------------
      rcs = 0
                     call zeit_ci ( 'dyn_get' )
      if ( pick ) then
         call dyn_get ( dyn_f, nymd_f, nhms_f, w_f, rcs(1), 
     &                  timidx=0, freq=freq )
         call dyn_get ( dyn_a, nymd_f, nhms_f, w_a, rcs(2), 
     &                  timidx=0, freq=freq )
      else
         call dyn_get ( dyn_f, nymd_f, nhms_f, w_f, rcs(1), freq=freq )
         call dyn_get ( dyn_a, nymd_f, nhms_f, w_a, rcs(2), freq=freq )
      end if
                     call zeit_co ( 'dyn_get' )

      if ( any(rcs(1:2) /= 0 ) ) then
         call die(myname,'could not read state files '
     &            //trim(dyn_f)//' or '//trim(dyn_a) )
      else
         print *
         print *, myname//': read first guess from file '//trim(dyn_f)
         print *, myname//': read analysis    from file '//trim(dyn_a)
         print *, myname//': timestamp is nymd, nhms = ', nymd_f, nhms_f
      end if


!     Either load bias (latest time)...
!     ---------------------------------
      inquire ( file='b_rst', exist=fexists )
      if ( fexists ) then

                          call zeit_ci ( 'dyn_get' )
              call dyn_get ( 'b_rst', nymd_b, nhms_b, b_f, rc, freq=freq )
                          call zeit_co ( 'dyn_get' )

           if ( rc .ne. 0 ) 
     &          call die(myname,'could not read bias file '//trim(dyn_b))


           print *, myname//': read bias estimate from file '//trim(dyn_b)
           print *, myname//': timestamp is nymd, nhms = ', nymd_b, nhms_b

!     or set bias to zero
!     -------------------
      else

          call dyn_init ( w_f, b_f, rc, copy = .false. )

          nymd_b = nymd_f
          nhms_b = nhms_f

          print *
          print *, myname//': initialized bias to zero'
          print *

      end if

!     Remap to convert shaved analysis from lcv to eta coords
!     -------------------------------------------------------
      if ( remap ) then
         call dyn_init ( w_a, w_e, rc, copy = .true. )
         call z_map ( w_a, w_e, rc, verbose = .true. )
         if ( trim(out_a) /= 'NONE' ) then
           call dyn_put ( trim(out_a), nymd_f, nhms_f, 1, w_e, rc, freq=freq )
           print *, myname//': wrote ' // trim(out_a)
        end if
      else
         call dyn_init ( w_a, w_e, rc, copy = .true. )
      end if

!     Estimate bias
!     -------------
      a = alpha
      if ( upd('ts') )   b_f%Ts   = b_f%Ts   - a * (w_e%Ts   - w_f%Ts)
      if ( upd('u') )    b_f%u    = b_f%u    - a * (w_e%u    - w_f%u)
      if ( upd('v') )    b_f%v    = b_f%v    - a * (w_e%v    - w_f%v)
      if ( upd('pt') )   b_f%pt   = b_f%pt   - a * (w_e%pt   - w_f%pt)
      if ( upd('q') )    b_f%q    = b_f%q    - a * (w_e%q    - w_f%q)
      if ( upd('delp') ) b_f%delp = b_f%delp - a * (w_e%delp - w_f%delp)

!     Write out the updated bias
!     --------------------------
      if ( trim(dyn_b) /= 'NONE' ) then
         call dyn_put ( trim(dyn_b), nymd_b, nhms_b, 0, b_f, rc, freq=freq )
         print *, myname//': wrote ' // trim(dyn_b)
      end if

      if ( restart .and. trim(dyn_b) /= 'b_rst' ) then 
         call dyn_put ( 'b_rst', nymd_b, nhms_b, 0, b_f, rc, 
     &                  freq=freq, new=.true. )
         print *, myname//': wrote ' // trim(dyn_b)
      end if

!     De-allocate first guess
!     -----------------------
      call dyn_clean ( w_f )
      call dyn_clean ( w_e )

!     De-allocate bias
!     ----------------
      call dyn_clean ( b_f )


!     Print out timing info
!     ---------------------
      call zeit_co    ( 'ibc_upd' )
      call zeit_flush ( 6 )

      print *, 'ibc_upd: successfully completed'

      call exit(0)

!...................................................................................

      CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: Init_ --- Initialize Ibc_Upd
!
! !INTERFACE:
!
      subroutine Init_ ( dyn_f, dyn_b, dyn_a, out_a, remap, restart,
     &                   vars, alpha )

! !USES:

      use m_die
      implicit NONE


! !OUTPUT PARAMETERS:

      character(len=255), intent(out) :: dyn_f   ! first guess file name
      character(len=255), intent(out) :: dyn_a   ! analysis file name
      character(len=255), intent(out) :: out_a   ! analysis file name
      character(len=255), intent(out) :: dyn_b   ! bias file name
      logical, intent(out) :: remap              ! whether to remap analysis
      logical, intent(out) :: restart            ! whether to overwrite b_rst
      character(len=255), intent(out) :: vars    ! variable names
      real, intent(out) :: alpha                 ! bias coeff

!
! !DESCRIPTION: Parses command line.
!
! !REVISION HISTORY:
!
!     25May2001   Dee       Initial code.
!
!EOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname = 'init'

      integer i, iarg, argc, iargc, n
      character(len=255) argv, prefix

      print *
      print *, '     ---------------------------------------------------------'
      print *, '     ibc_upd - incremental bias correction Update  application'
      print *, '     ---------------------------------------------------------'
      print *


      prefix = myname

!     Parse command line
!     ------------------
      dyn_b   = 'undef'
      remap = .true. 
      restart = .true.
!!!      vars = 'ts,u,v,pt,q,delp'
      vars = 'pt,delp'
      alpha = 0.1
      out_a = 'undef'

      argc =  iargc()
      if ( argc .lt. 1 ) call usage_()

      iarg = 0
      n = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iArg, argv )
         if (index(argv,'-b' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, dyn_b )
         else if (index(argv,'-noremap' ) .gt. 0 ) then
            remap = .false.
         else if (index(argv,'-nors' ) .gt. 0 ) then
            restart = .false.
         else if (index(argv,'-a' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, out_a )
         else if (index(argv,'-vars' ) .gt. 0 ) then
            if ( iarg+2 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, vars )
         else if (index(argv,'-alpha' ) .gt. 0 ) then
            if ( iarg+2 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) alpha
         else if (index(argv,'-pick' ) .gt. 0 ) then
            if ( iarg+2 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) nymd_f
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) nhms_f
            pick = .true.
         else
            n = n + 1
            call FileResolv ( trim(prefix), nymd_f, nhms_f, argv, dyn_f )
            if ( iarg+1 .gt. argc ) call usage_()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            n = n + 1
            call FileResolv ( trim(prefix), nymd_f, nhms_f, argv, dyn_a )
         end if
      end do

      if ( n < 2 ) then
         call warn ( myname, 'missing first guess/analysis file names' )
         call usage_()
      end if

      if ( trim(out_a) == 'undef' ) out_a = dyn_a
      if ( trim(dyn_b) == 'undef' ) then
           n = index ( dyn_f, '.bkg.' )
           if ( n > 0 ) then
                dyn_b = dyn_f(1:n-1)//'.bias.'//dyn_f(n+5:)
           else
                dyn_b = 'bias.hdf'
           end if
       end if

!     Echo input parameters
!     ---------------------
      print *
      print *, 'Background file: ', trim(dyn_f)
      print *, '  Analysis file: ', trim(dyn_a) // ' (INPUT)'
      print *, '  Analysis file: ', trim(out_a) // ' (OUTPUT)'
      print *, '      Bias file: ', trim(dyn_b)
      if ( pick ) then
      print *, '     nymd, nhms: ', nymd_f, nhms_f
      endif
      if ( remap ) then
      print *, '      Remapping? YES'
      else
      print *, '      Remapping? NO'
      endif
      if ( restart ) then
      print *, '      Overwrite file "b_rst"? YES'
      else
      print *, '      Overwrite file "b_rst"? NO'
      endif
      print *, '      Variables: ', trim(vars)
      print *, '          Alpha: ', alpha
      print *

      end subroutine Init_

!.......................................................................

      subroutine usage_()
      print *
      print *, 'SYNOPSIS  '
      print *
      print *, '       ibc_upd.x [-pick nymd nhms] [-b dyn_b] [-noremap] '
      print *, '                 [-vars ...] [-alpha value] dyn_f dyn_a'
      print *
      print *, 'DESCRIPTION'
      print *, '       Updates the bias estimate on file "b_rst" using the '
      print *, '       simplified algorithm based on the analysis increments.'
      print *, '       Notice that a delp bias is now also estimated. '
      print *, '       On input,'
      print *
      print *, '       dyn_a            analysis state file name'
      print *, '       dyn_f            dynamics state file name'
      print *
      print *, '       Both dyn_a and dyn_b will be overwritten, as well'
      print *, '       as a single time file "b_rst".'
      print *, '       If the bias estimate file "b_rst" does not exist, '
      print *, '       it will be initialized to zero. '
      print *
      print *, 'OPTIONS:'
      print *, ' -pick nymd nhms    selects which date to read; default'
      print *, '                    is the latest'
      print *, ' -b fname           additional output bias file name, '
      print *, '                    default is derived from background '
      print *, '                    file name. Specify NONE to skip it.'
      print *, ' -a fname           output analysis file name; default'
      print *, '                    is to overwrite input file'
      print *, ' -noremap           by default input ana file is remapped'
      print *, '                    from lcv to standard eta; specify this'
      print *, '                    to skip this step. Typically, a PSAS '
      print *, '                    analysis will need remapping, while a '
      print *, '                    SSI/GSI analysis will not. Beware.' 
      print *, ' -vars string       which variables to operate on; default'
      print *, '                    is "pt,delp". Possible values include'
      print *, '                    "ts,u,v,pt,q,delp".'
      print *, ' -alpha value       specifies the simplified bias correction' 
      print *, '                    coefficient:  b := b - alpha * ainc. '
      print *, '                    Default is 0.1'
      print *, ' -nors              does not overwrites restart file "b_rst"'
      print *
      call die('ibc_upd','not enough arguments' )
      end subroutine usage_

!.......................................................................

      end program ibc_upd
