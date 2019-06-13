!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !MODULE: m_mapz --- Performs vertical interpolation of rst-like fields
!                     
!
! !INTERFACE:
!
      module m_dyn2dyn

!USES:

      use m_dyn
      use m_maph, only : h_map
      use m_mapz, only : z_map
      use m_vdc2vdc, only : vdc2vdc
      
      use m_StrTemplate        ! grads style templates
      use m_inpak90
      use m_stdio, only : stdout

      implicit NONE
 
! !PUBLIC MEMBER FUNCTIONS:
 
      PRIVATE
      PUBLIC dyn2dyn_do   ! general interpolation routine
      PUBLIC def_RCd2d    ! default resource file name possibly used by this module

! !PUBLIC MEMBER FUNCTIONS:

      interface dyn2dyn_do
         module procedure dyn2dyn_do0_
         module procedure dyn2dyn_do1_
      end interface


! !DESCRIPTION: This module performs general interpolation of dyn vector.
!
! !SEE ALSO: dyn2dyn, m_mapz, m_maph
!
! !REVISION HISTORY:
!
!  27Sep2004  Todling    Initial module.
!  29dec2005  da Silva   Added option to force remapping even when ptop
!                        and nlevs are the same. This is useful when
!                        converting from lcv to eta coordinates. 
!  11Dec2007  Todling    Add vectype as opt arg to _do
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'm_dyn2dyn'

      character(len=*), parameter :: def_RCd2d = 'dyn2dyn.rc'
      integer,          parameter :: prec_def  = 0             ! 0=32-bits by default

      CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---------
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: dyn2dyn_do0_: Interpolates dyn vectors
!
! !INTERFACE:

      subroutine dyn2dyn_do0_ ( dynfile, w_e, nymd, nhms, freq, nstep, indxlevs, rc, &
                                prec, dophys, expid, RCfile, force, dgrid, &
                                vectype )

! !USES:

      implicit NONE

! !INPUT PARAMETERS:

      type(dyn_vect) w_e  ! dynamics vector in eta (input)

      integer, intent(in) :: nymd              ! date (YYYYMMDD)
      integer, intent(in) :: nhms              ! time (HHMMSS)
      integer, intent(in) :: freq              ! frequency of times in file
      integer, intent(in) :: nstep             ! gcm-phys control parameter

      character(len=*), intent(in) :: dynfile  ! output filename for dyn-vect

      integer,          intent(in), optional :: prec    ! precision of output file
      logical,          intent(in), optional :: dophys  ! controls call oft vdc2vdc
      logical,          intent(in), optional :: indxlevs! place indx levels instead of pressures
      character(len=*), intent(in), optional :: expid   ! experiment name
      character(len=*), intent(in), optional :: RCfile  ! resource file name
      logical,          intent(in), optional :: force   ! force zmap
      logical,          intent(in), optional :: dgrid   ! allow choice between A- or D-grid
      integer,          intent(in), optional :: vectype ! GEOS-4/5 dyn-vect argument

! !OUTPUT PARAMETERS:

      integer, intent(out) :: rc               ! error code:
                                               !  1= error in rc file
                                               !  2= incorrect dyn vect initialization

! !DESCRIPTION: Resource-file-based interface to dyn2dyn.
!
! !SEE ALSO: dyn2dyn_do1_
!
! !REVISION HISTORY:
!
!  28Sep2004  Todling   Initial code.
!  19Nov2007  Todling   A-grid handle.
!
!----------------------------------------------------------------------------------
!EOP

      character(len=*), parameter :: myname_ = myname//'::set_'

      character(len=255)  :: lwifile             ! filename containing LWI
      logical, parameter  :: verbose = .false.   ! no verbose

      integer :: in, jn, kn        ! dim for output
      integer :: lprec
      integer :: ier
      logical :: force_it
      logical :: dgrid_

      rc = 0

      if ( present(force) ) then
           force_it = force
      else
           force_it = .false.
      end if
      if ( present(dgrid) ) then
           dgrid_ = dgrid
      else
           dgrid_ = .true.
      end if

      if(present(prec))then
         lprec = prec
         call set_ ( nymd, nhms, lwifile, in, jn, kn, ier, & 
                     expid=expid, RCfile=RCfile )
      else
         call set_ ( nymd, nhms, lwifile, in, jn, kn, ier, & 
                     prec=lprec, expid=expid, RCfile=RCfile )
      endif
        if (ier/=0) then
            rc = 1
            return
        endif

      call dyn2dyn_do1_ ( w_e, &
                          in, jn, kn, indxlevs, verbose, ier, &
                          dynfile=dynfile, lwifile=lwifile, &
                          nymd=nymd, nhms=nhms, prec=lprec, freq=freq, nstep=nstep, &
                          dophys=dophys, expid=expid,&
                          force=force_it, dgrid=dgrid_, vectype=vectype )
        if (ier/=0) then
            rc = 2
            return
        endif

      end subroutine dyn2dyn_do0_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Data Assimilation Office, Code 900.3, GEOS/DAS !
!--------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: dyn2dyn_do1_: Interpolates dyn vectors
!
! !INTERFACE:

      subroutine dyn2dyn_do1_ ( w_e, &
                                in, jn, kn, indxlevs, verbose, rc, &
                                dynfile, lwifile, w_out, &
                                nymd, nhms, prec, freq, nstep, &
                                dophys, expid, force, dgrid, vectype )
 
! !USES:
      use m_set_eta, only: set_eta

      implicit NONE

! !INPUT PARAMETERS:

      type(dyn_vect) w_e  ! dynamics vector in eta (input)
      type(dyn_vect), optional :: w_out ! dynamics vector in eta (output)

      character(len=*),optional, intent(in) :: dynfile     ! output filename
      character(len=*),optional, intent(in) :: lwifile     ! filename containing LWI
      integer, intent(inout) :: in, jn, kn        ! dim for output
      integer, intent(in),optional :: prec              ! precision of output file 0=32;1=64
      integer, intent(in),optional :: freq              ! frequency of times in file
      integer, intent(in),optional :: nstep             ! gcm-phys control parameter
      logical, intent(in), optional :: indxlevs         ! place indx levels instead of pressures
      logical, intent(in) :: verbose                    ! echo msgs

      integer, intent(in),optional :: nymd              ! date (YYYYMMDD)
      integer, intent(in),optional :: nhms              ! time (HHMMSS)

      logical,          intent(in), optional :: dophys      ! controls call oft vdc2vdc
      character(len=*), intent(in), optional :: expid       ! experiment name
      logical,          intent(in), optional :: force       ! forces z mapping
      logical,          intent(in), optional :: dgrid       ! allows choice between A- or D-grid
      integer,          intent(in), optional :: vectype     ! GEOS-4/5 dyn-vect argument

! !OUTPUT PARAMETERS:

      integer, intent(out)   :: rc                ! error code:
                                                  !  1= incorrect dyn vect initialization

! !DESCRIPTION: Uses S.J. Lin mapping routines to interpolate
!               dynamics state vector between different resolutions.
!
! !REMARKS: When performing horizontal interpolation this program needs
!    to a GCM physics restart at the output resolution. This is to extract
!    the land-water-ice (LWI) mask array since currently we do not have a
!    smart way of interpolating LWI yet. 
!
! !TO DO: Implement a voting interpolation procedure to handle LWI.
!
! !REVISION HISTORY:
!
!  27Sep2004  Todling   Initial independed code; stripped off dyn2dyn.
!  04Feb2005  Gelaro/RT Added phys trajectory output when no interpolation
!  27Jan2015  Todling   Add call to set eta for proper vertical interpolation
!
!----------------------------------------------------------------------------------
!EOP

      character(len=*), parameter :: myname_ = myname//'::dyn2dyn_do1_'

!     Dynamics/simulator vectors
!     --------------------------
      type(dyn_vect) w_o  ! fully interpolated dynamics vector in eta (output)
      type(dyn_vect) w_v  ! auxiliar dynamics vector


!     Locals
!     ------
      integer ier
      integer ks, k
      real,allocatable:: ak(:),bk(:)
      logical interp, zinterp, hinterp, zint_done, do_phys, force_it, dgrid_
      logical indxlevs_

      if (present(dophys)) then 
          do_phys = dophys
      else
          do_phys = .true.
      endif

      if ( present(force) ) then
           force_it = force
      else
           force_it = .false.
      end if      

      if ( present(dgrid) ) then
           dgrid_ = dgrid
      else
           dgrid_ = .true.
      end if      

      if ( present(indxlevs) ) then
           indxlevs_ = indxlevs
      else
           indxlevs_ = .false.
      end if      

!                                 *******


!        Either directly write out or interpolate and write out state vector
!        -------------------------------------------------------------------
         zint_done=.false.
         rc = 0
         if(kn==0)         kn= w_e%grid%km
         zinterp = .not.(  kn==w_e%grid%km  )
         if ( force_it ) then
              zinterp = .true. ! useful for lcv -> eta 
         else
              zinterp = .not.(  kn==w_e%grid%km  )
         end if

         hinterp = .not.( (in==0 .and. jn==0) .or. (in==w_e%grid%im .and. jn==w_e%grid%jm) )
         interp  =  hinterp .or. zinterp
         if ( interp ) then

!           First interpolate vertically, if required
!           -----------------------------------------
            if ( zinterp ) then

                print *, trim(myname_), ': Vertically interpolating from km= ', w_e%grid%km, ' to kn= ', kn

!               Initialize dimension of vertically interpolated vector
!               -------------------------------------------------------
                allocate(ak(kn+1),bk(kn+1))
                call set_eta ( kn,ks,w_v%grid%ptop,w_v%grid%pint,ak,bk )
                call dyn_init ( w_e%grid%im, w_e%grid%jm, kn, w_e%grid%lm, w_v, ier, vectype=vectype, ks=ks, ak=ak, bk=bk )
                   if ( ier/=0 ) then
                        print *, trim(myname_), ': Error initializing dyn vector(w_v), ier=', ier
                        rc = 1
                        return
                   endif
                deallocate(ak,bk)

!               Map to desired resolution
!               -------------------------
                call z_map ( w_e, w_v, ier, verbose=verbose, force=force_it )
                  if (ier<0) then
                      zint_done = .false.
                  else
                      zint_done = .true.
                  endif

            endif

!           Then interpolate horizontally, if required
!           ------------------------------------------
            if ( hinterp ) then

                if ( zint_done ) then

                    print *, trim(myname_), ': Horizontally interpolating from ', &
                              w_v%grid%im, 'x', w_v%grid%jm, ' to ', in, 'x', jn

!                   Initialize dimension of output (interpolated) vector
!                   ----------------------------------------------------
                    call dyn_init ( in, jn, w_v%grid%km, w_v%grid%lm, w_o, ier,  &
                                    w_v%grid%ptop, w_v%grid%ks, w_v%grid%ak, w_v%grid%bk, vectype=vectype ) 
                      if ( ier/=0 ) then
                           print *, trim(myname_), ': Error initializing dyn vector(w_o), ier=', ier
                           rc = 1
                           return
                      endif

!                   Map to desired resolution
!                   -------------------------
                    call h_map ( w_v, w_o, ier, lwifile=lwifile, dgrid=dgrid_ )
                       if ( ier/=0 ) then
                            call dyn_clean ( w_v )
                            call dyn_clean ( w_o )
                            print *, trim(myname_), ': Error in horizontal interpolation'
                            rc = 2
                            return
                       endif

!                   Clean up
!                   --------
                    call dyn_clean ( w_v )

                else

                    print *, trim(myname_), ': Horizontally interpolating from ', &
                              w_e%grid%im, 'x', w_e%grid%jm, ' to ', in, 'x', jn

!                   Initialize dimension of output (interpolated) vector
!                   ----------------------------------------------------
                    call dyn_init ( in, jn, w_e%grid%km, w_e%grid%lm, w_o, ier,  &
                                    w_e%grid%ptop, w_e%grid%ks, w_e%grid%ak, w_e%grid%bk, vectype=vectype ) 
                      if ( ier/=0 ) then
                           print *, trim(myname_), ': Error initializing dyn vector(w_o), ier=', ier
                           rc = 1
                           return
                      endif

!                   Map to desired resolution
!                   -------------------------
                    call h_map ( w_e, w_o, ier, lwifile=lwifile, dgrid=dgrid_ )
                       if ( ier/=0 ) then
                            call dyn_clean ( w_o )
                            print *, trim(myname_), ': Error in horizontal interpolation'
                            rc = 2
                            return
                       endif

                endif

!               Write out horizontally (and maybe vertically) interpolated vector
!               -----------------------------------------------------------------
                if (present(dynfile)) then
                   write(*,'(a,i8,i3,a,i8)') trim(myname_) // ': writing ' // trim(dynfile) // &
                                          ' on ', nymd, nhms/10000, 'Z, freq = ', freq 
                   call dyn_put ( dynfile, nymd, nhms, prec, w_o, ier, &
                                  freq=freq, nstep=nstep, vectype=vectype, indxlevs=indxlevs_ )
                     if ( ier .ne. 0 ) then
                          call dyn_clean ( w_o )
                          print *, trim(myname_), ': cannot write interpolated ETA file'
                          rc = 3
                          return
                     endif
                endif
                if (present(w_out)) then
                   call dyn_init (w_o, w_out, ier, copy=.true.)
                     if ( ier .ne. 0 ) then
                          call dyn_clean ( w_o )
                          print *, trim(myname_), ': cannot copy result to output vector'
                          rc = 3
                          return
                     endif
                endif

!               Try and interpolate vertical diffusion coefficients ... though they may not be available
!               ----------------------------------------------------------------------------------------
                if (do_phys) then
                    call vdc2vdc ( w_e%grid%im, w_e%grid%jm, w_e%grid%km, w_e%grid%ptop, w_e%pt, w_e%delp, &
                                   w_o%grid%im, w_o%grid%jm, w_o%grid%km, w_o%grid%ptop, w_o%pt, w_o%delp, &
	                           w_o%grid%ks, w_o%grid%ak, w_o%grid%bk, &
		                   nymd, nhms,  ier, verbose=verbose, expid=expid ) 
                          if ( ier/=0 ) &
                               print *, trim(myname_), ': Warning, unable to interpolate vertical diff coefficients'
                endif
                      

!               Clean up
!               --------
                call dyn_clean ( w_o )

            else

!               Write out vertically interpolated state vector
!               ----------------------------------------------
                if (present(dynfile)) then
                   write(*,'(a,i8,i3,a,i8)') trim(myname_) // ': writing ' // trim(dynfile) // &
                                          ' on ', nymd, nhms/10000, 'Z, freq = ', freq 
                   call dyn_put ( dynfile, nymd, nhms, prec, w_v, ier, &
                                  freq=freq, nstep=nstep, vectype=vectype, indxlevs=indxlevs_ )
                      if ( ier .ne. 0 ) then
                           print *, trim(myname_), ': cannot write interpolated ETA file'
                           rc = 3
                           return
                      endif
                endif
                if (present(w_out)) then
                   call dyn_init (w_v, w_out, ier, copy=.true.)
                     if ( ier .ne. 0 ) then
                          call dyn_clean ( w_v )
                          print *, trim(myname_), ': cannot copy result to output vector'
                          rc = 3
                          return
                     endif
                endif
  
!               Try and interpolate vertical diffusion coefficients ... though they may not be available
!               ----------------------------------------------------------------------------------------
                if (do_phys) then
                    call vdc2vdc ( w_e%grid%im, w_e%grid%jm, w_e%grid%km, w_e%grid%ptop, w_e%pt, w_e%delp, &
                                   w_v%grid%im, w_v%grid%jm, w_v%grid%km, w_v%grid%ptop, w_v%pt, w_v%delp, &
	                           w_v%grid%ks, w_v%grid%ak, w_v%grid%bk, &
		                   nymd, nhms, ier, verbose=verbose, expid=expid ) 
                          if ( ier/=0 ) &
                               print *, trim(myname_), ': Warning, unable to interpolate vertical diff coefficients'
                endif

!               Clean up
!               --------
                call dyn_clean ( w_v )

            endif

         else   ! no interpolation at all ...

!           Write out pressure file
!           -----------------------
            if (present(dynfile)) then
               write(*,'(a,i8,i3,a,i8)') trim(myname_) // ': writing ' // trim(dynfile) // &
                                      ' on ', nymd, nhms/10000, 'Z, freq = ', freq
               call dyn_put ( dynfile, nymd, nhms, prec, w_e, ier, &
                              freq=freq, nstep=nstep, vectype=vectype, indxlevs=indxlevs_ )
                  if ( ier .ne. 0 ) then
                       print *, trim(myname_), ':cannot write ETA file'
                       rc = 3
                       return
                  endif
            endif
            if (present(w_out)) then
               call dyn_init (w_e, w_out, ier, copy=.true.)
                 if ( ier .ne. 0 ) then
                      call dyn_clean ( w_e )
                      print *, trim(myname_), ': cannot copy result to output vector'
                      rc = 3
                      return
                 endif
            endif
  

            if (do_phys) then
                call vdc2vdc ( w_e%grid%im, w_e%grid%jm, w_e%grid%km, w_e%grid%ptop, w_e%pt, w_e%delp, &
                               w_e%grid%im, w_e%grid%jm, w_e%grid%km, w_e%grid%ptop, w_e%pt, w_e%delp, &
                               w_e%grid%ks, w_e%grid%ak, w_e%grid%bk, &
                               nymd, nhms, ier, verbose=verbose, expid=expid )
                      if ( ier/=0 ) &
                           print *, trim(myname_), ': Warning, unable to interpolate vertical diff coefficients'
            endif

         endif

  end subroutine dyn2dyn_do1_

!---------------------------------------------------------------------------
! NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS !
!---------------------------------------------------------------------------
!BOP
!
! !ROUTINE: set_ --- General paramater setup for singular vector calc
! 
! !INTERFACE:
!
      subroutine set_ ( nymd, nhms, lwifile, in, jn, kn, rc, &
                        prec, expid, RCfile )  ! optionals
 
! !USES: 
    
      Implicit None

! !INCLUDES:

! !INPUT PARAMETERS: 

     integer,          intent(in) :: nymd
     integer,          intent(in) :: nhms
     character(len=*), intent(in), optional :: expid    ! experiment name
     character(len=*), intent(in), optional :: RCfile   ! filename of resources

! !OUTPUT PARAMETERS:
 
     integer,          intent(out), optional :: prec    ! precision of output file
     character(len=*), intent(out) :: lwifile           ! filename of resources
     
     integer, intent(out) :: in, jn, kn                 ! dimensions of output vector
     integer, intent(out) :: rc                         ! return error code:
                                                        !  1 = cannot load resource

! !FILES USED:  dyn2dyn.rc
!
! !DESCRIPTION:  Initializes basic parameters to interpolate dyn vector.
!
! !REVISION HISTORY: 
!
!   28Sep2004  Todling    Initial code.
!   01Oct2004  Todling    Removed dynfile; must be in argument of main_
!   04Oct2004  Todling    Turned precision into optional argument
!
!EOP
!---------------------------------------------------------------------------

      character(len=*), parameter :: myname_   = myname//'::set_'

      character(len=255) template
      character(len=255) d2drc

      integer            ival, iret

!     Default grid: a18
!     -----------------
      integer, parameter :: in_def   = 72
      integer, parameter :: jn_def   = 46
      integer, parameter :: kn_def   = 18

      rc = 0

!     Load resources
!     --------------
      d2drc = ' '
      if ( present(RCfile) ) then
        d2drc = trim(RCfile)
      else
        call get_environment_variable('DYN2DYN_RC',d2drc)		! Unix binding
        if(d2drc.eq.' ') d2drc=def_RCd2d	! default name
      endif
      call i90_loadf (trim(d2drc), iret)
      if( iret .ne. 0) then
         write(stdout,'(3a)') trim(myname_),': Warning, I90_loadf cannot find file', trim(d2drc)
         rc = 1
         return
      end if
      write(stdout,'( a  )') '---------------------------------------------------------'
      write(stdout,'(3a  )') trim(myname_), ': Reading resource file: ', trim(d2drc)
      write(stdout,'( a,/)') '---------------------------------------------------------'


!     Read in number of grid points in the zonal direction
!     ----------------------------------------------------
      call I90_label('dyn2dyn_zonal_grid_points:', iret)
      if (iret .eq. 0) then
        ival = I90_GInt(iret)
        if (iret .ne. 0) then
          in = in_def
          write(stdout,'(2a,i5)') trim(myname_),': IN not defined, taking default ... ', in
        else
          in = ival
        end if
      end if
      write(stdout,'(a,i5)') 'Number of grid points in zonal direction: ', in

!     Read in number of grid point in the meridional direction
!     --------------------------------------------------------
      call I90_label('dyn2dyn_meridional_grid_points:', iret)
      if (iret .eq. 0) then
        ival = I90_GInt(iret)
        if (iret .ne. 0) then
          jn = jn_def
          write(stdout,'(2a,i5)') trim(myname_),': JN not defined, taking default ... ', jn
        else
          jn = ival
        end if
      end if
      write(stdout,'(a,i5)') 'Number of grid points in meridional direction: ', jn

!     Read in number of grid point in the vertical
!     --------------------------------------------
      call I90_label('dyn2dyn_vertical_grid_points:', iret)
      if (iret .eq. 0) then
        ival = I90_GInt(iret)
        if (iret .ne. 0) then
          kn = kn_def
          write(stdout,'(2a,i5)') trim(myname_),': KN not defined, taking default ... ', kn
        else
          kn = ival
        end if
      end if
      write(stdout,'(a,i5)') 'Number of grid points in vertical direction: ', kn

!     Read in precision of output file (if prec is present)
!     -----------------------------------------------------
      if (present(prec) ) then
          call I90_label('dyn2dyn_outfile_precision:', iret)
          if (iret .eq. 0) then
            ival = I90_GInt(iret)
            if (iret .ne. 0) then
              prec = prec_def
              write(stdout,'(2a,i5)') trim(myname_),': PRECISION not found, using default: ', prec 
            else
              prec = ival
            end if
          end if
          write(stdout,'(a,i5)') 'Output dyn-vector file in precision(bits): ', prec
          if(prec==32) prec=0
          if(prec==64) prec=1
      endif

!     Determine LWI file name
!     -----------------------
      if (present(expid)) then
          write(lwifile,'(2a,i3.3,a,i3.3,a)') trim(expid), '.p_rst.', in, 'x', jn, '.bin'
      else
          write(lwifile,'(a,i3.3,a,i3.3,a)') 'p_rst.', in, 'x', jn, '.bin'
      endif
      call i90_label ( 'dyn2dyn_lwi_filename:', iret )
      if ( iret .ne. 0 ) then
         write(stdout,'(a)') 'taking LWI default filename '
      else
        call i90_gtoken ( template, iret )
        if ( iret==0 ) then
           call strTemplate ( lwifile, template, 'GRADS', xid=expid, &
                              nymd=nymd, nhms=nhms, stat=iret )
	else
	   write(stdout,'(a)') 'taking LWI default filename '
	endif		      
      endif

!     release resource file:
!     ---------------------
      call I90_release()

      return
      end subroutine set_



  end module m_dyn2dyn
