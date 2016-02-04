
#include "MAPL_Exceptions.h"

!-------------------------------------------------------------------------
!      NASA/GSFC, Global Modeling & Assimilation Office, Code 900.3      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  Chem_MieTableMod --- Reader for aerosol mie tables
!
! !INTERFACE:
!

   module  Chem_MieTableMod

! !USES:

   use ESMF
   use MAPL_Mod
   use m_die, only: die, warn

   implicit none
   include "netcdf.inc"    ! Required for Mie tables stored as NCDF files

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  Chem_MieTable        ! Holds Mie Lookup Tables
                           
!
! !PUBLIC MEMBER FUNCTIONS:
!
   PUBLIC  Chem_MieTableCreate  ! Constructor 
   PUBLIC  Chem_MieTableDestroy ! Destructor
   PUBLIC  Chem_MieTableRead    ! Read the mie table from the file
   PUBLIC  Chem_MieTableGetDims ! Return table sizes

!
! !DESCRIPTION:
!
!  This module read the mie aerosol tables.
!
! !REVISION HISTORY:
!
!  23Mar2005 Colarco - Initial code.
!  31Mar2005 Todling - Declared netcdf nf_ routines as external (OSF1) 
!                      Removed # from include netcdf.inc
!
!EOP
!-------------------------------------------------------------------------

! Mie LUT table
! Will be reduced from input files to the desired channels
! --------
  type Chem_MieTable

     character(len=255) :: mietablename
     integer :: nlambda         ! number of wavelengths in table
     integer :: nrh             ! number of RH values in table
     integer :: nbin            ! number of size bins in table
     integer :: nMom            ! number of moments of phase function
     integer :: nPol            ! number of elements of scattering phase matrix
     real, pointer    :: lambda(:) => null()       ! wavelengths [m]
     real, pointer    :: rh(:) => null()           ! RH values   [fraction]
     real, pointer    :: reff(:,:) => null()       ! effective radius [m]
     real, pointer    :: bext(:,:,:) => null()     ! bext values [m2 kg-1]
     real, pointer    :: bsca(:,:,:) => null()     ! bsca values [m2 kg-1]
     real, pointer    :: bbck(:,:,:) => null()     ! bbck values [m2 kg-1]
     real, pointer    :: g(:,:,:) => null()        ! asymmetry parameter
     real, pointer    :: pback(:,:,:,:) => null()  ! Backscatter phase function
     real, pointer    :: pmom(:,:,:,:,:) => null( )  ! moments of phase function

     integer          :: rhi(991)        ! pointer to rh map
     real             :: rha(991)        ! slope on rh map

  end type Chem_MieTable

# ifndef HAS_NETCDF3
  external nf_open, nf_inq_dimid, nf_inq_dimlen, nf_inq_varid, &
           nf_get_var_double, nf_close      
#endif


CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_MieTableCreate --- Construct Chemistry Registry
!
! !INTERFACE:
!

  Function Chem_MieTableCreate ( rcfile, rc )

  implicit none
  type(Chem_MieTable) Chem_MieTableCreate 

! !INPUT PARAMETERS:

   character(len=*) :: rcfile  ! Mie table file name

! !OUTPUT PARAMETERS:

   integer, intent(out) ::  rc            ! Error return code:
                                          !  0 - all is well
                                          !  1 - 

! !DESCRIPTION:
!
!
! !REVISION HISTORY:
!
!  09Mar2005 da Silva  API, prologues.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter ::  myname = 'Chem_MieTableCreate'

   type(Chem_MieTable) :: this

                           __Iam__("Chem_MieTableCreate")

   rc = 0

   this%mietablename = rcfile

!  Note: The actual allocation is done when reading because dimensions are
!        read from file

!  All done
!  --------
   Chem_MieTableCreate = this
   
   return 

 end Function Chem_MieTableCreate

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_MieTableDestroy --- Destruct Mie Table
!
! !INTERFACE:
!
  subroutine Chem_MieTableDestroy ( this, rc )

! !USES:

  implicit none

! !INPUT/OUTPUT PARAMETERS:

   type(Chem_MieTable), intent(inout) :: this

! !OUTPUT PARAMETERS:

  integer, intent(out)          ::  rc     ! Error return code:
                                           !  0 - all is well
                                           !  1 - 

! !DESCRIPTION: Destructor for AOD object.
!
! !REVISION HISTORY:
!
!  23Mar2005 Colarco
!
!EOP
!-------------------------------------------------------------------------
   integer ios
                           __Iam__("Chem_MieTableDestroy")

   rc = 0

! Set these to invalid values
! ---------------------------
  this%nlambda = -1
  this%nrh = -1
  this%nbin = -1
  this%nmom = -1

! Deallocate whatever has been allocated
! --------------------------------------
  if ( associated(this%lambda) ) deallocate(this%lambda, stat=rc)
  VERIFY_(rc)
  if ( associated(this%rh) )     deallocate(this%rh, stat=rc)
  VERIFY_(rc)
  if ( associated(this%reff) )   deallocate(this%reff, stat=rc)
  VERIFY_(rc)
  if ( associated(this%bext) )   deallocate(this%bext, stat=rc)
  VERIFY_(rc)
  if ( associated(this%bsca) )   deallocate(this%bsca, stat=rc)
  VERIFY_(rc)
  if ( associated(this%bbck) )   deallocate(this%bbck, stat=rc)
  VERIFY_(rc)
  if ( associated(this%g) )      deallocate(this%g, stat=rc)
  VERIFY_(rc)
  if ( associated(this%pmom) )   deallocate(this%pmom, stat=rc)
  VERIFY_(rc)

end subroutine Chem_MieTableDestroy 

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_MieTableRead --- Read and fill in the Mie table, interpolated
!                              to the requested channels
!
! !INTERFACE:
!
   SUBROUTINE Chem_MieTableRead ( this, nch, channels, rc, nmom )

! !INPUT PARAMETERS:

   IMPLICIT none
   TYPE(Chem_MieTable), intent(inout)       :: this
   integer, intent(in) :: nch               ! number of channels to interpolate table to
   real, intent(in)    :: channels(:)       ! channels to interpolate table to
   integer, OPTIONAL, intent(in) :: nmom    ! number of moments to keep (default=0)
   integer, intent(out) :: rc   ! return code


! !DESCRIPTION:
!
!   Fills in the Mie table
!
! !REVISION HISTORY:
!
!  23Mar2005 Colarco
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Chem_MieTableRead'

      integer :: ncid, idimid, ivarid, ios, n, i, j, ip1
      integer :: nch_table, nrh_table, nbin_table, nmom_table, nPol_table
!     Tables are hard-wired as single precision
      real*8, pointer :: channels_table(:), rh_table(:), reff_table(:,:), &
                         bext_table(:,:,:), bsca_table(:,:,:), &
                         bbck_table(:,:,:), g_table(:,:,:), &
                         pmom_table(:,:,:,:,:), pback_table(:,:,:,:)
      real :: yerr
      integer :: nmom_, imom, ipol

                           __Iam__("Chem_MieTableRead")

      rc = 0

!     Whether or not doing phase function
!     -----------------------------------
      if ( present(nmom) ) then
         nmom_ = nmom
      else
         nmom_ = 0
      end if

!     Set up nPol_table for reading backscatter phase function
!     This will get overwritten if pmoments is requested
!     --------------------------------------------------------
      nPol_table = 6


!     Open the table and get the dimensions
!     -------------------------------------
      rc = nf_open(this%mietablename, NF_NOWRITE, ncid)
      VERIFY_(rc)

!     RH
!     --
      rc = nf_inq_dimid(ncid,'rh',idimid)
      VERIFY_(rc)
      rc = nf_inq_dimlen(ncid,idimid,nrh_table)
      VERIFY_(rc)

!     Channels
!     --------
      rc = nf_inq_dimid(ncid,'lambda',idimid)
      VERIFY_(rc)
      rc = nf_inq_dimlen(ncid,idimid,nch_table)
      VERIFY_(rc)

!     Dry Effective radius
!     --------------------
      rc = nf_inq_dimid(ncid,'radius',idimid)
      VERIFY_(rc)
      rc = nf_inq_dimlen(ncid,idimid,nbin_table)
      VERIFY_(rc)

!     Moments of phase function
!     -------------------------
      if ( nmom_ > 0 ) then
         rc = nf_inq_dimid(ncid,'nMom',idimid)
         VERIFY_(rc)
         rc = nf_inq_dimlen(ncid,idimid,nmom_table)
         VERIFY_(rc)
         if ( nmom_ > nmom_table ) then
            rc = 99
            VERIFY_(rc)
            return
         end if
         rc = nf_inq_dimid(ncid,'nPol',idimid)
         VERIFY_(rc)
         rc = nf_inq_dimlen(ncid,idimid,nPol_table)
         VERIFY_(rc)
      endif



!     Get the table contents
!     -------------------------------------
!      allocate ( channels_table(nch_table), rh_table(nrh_table), &
!                bext_table(nch_table,nrh_table,nbin_table), &
!                bsca_table(nch_table,nrh_table,nbin_table), &
!                bbck_table(nch_table,nrh_table,nbin_table), &
!                g_table(nch_table,nrh_table,nbin_table), stat = rc )

      allocate(channels_table(nch_table),stat = rc )
      VERIFY_(rc)
      allocate(rh_table(nrh_table),stat = rc )
      VERIFY_(rc)
      allocate(reff_table(nrh_table,nbin_table),stat = rc )
      VERIFY_(rc)
      allocate(bext_table(nch_table,nrh_table,nbin_table),stat = rc )
      VERIFY_(rc)
      allocate(bsca_table(nch_table,nrh_table,nbin_table),stat = rc )
      VERIFY_(rc)
      allocate(bbck_table(nch_table,nrh_table,nbin_table), stat = rc )
      VERIFY_(rc)
      allocate(g_table(nch_table,nrh_table,nbin_table), stat = rc )
      VERIFY_(rc)
      allocate(pback_table(nch_table,nrh_table,nbin_table,nPol_table), stat = rc )
      VERIFY_(rc)

      if ( nmom_ > 0 ) then
         allocate(pmom_table(nch_table,nrh_table,nbin_table,nmom_table,nPol_table), stat = rc )
         VERIFY_(rc)
      end if


      rc = nf_inq_varid(ncid,'lambda',ivarid)
      VERIFY_(rc)
      rc = nf_get_var_double(ncid,ivarid,channels_table)
      VERIFY_(rc)
      rc = nf_inq_varid(ncid,'rEff',ivarid)
      VERIFY_(rc)
      rc = nf_get_var_double(ncid,ivarid,reff_table)
      VERIFY_(rc)
      rc = nf_inq_varid(ncid,'bext',ivarid)
      VERIFY_(rc)
      rc = nf_get_var_double(ncid,ivarid,bext_table)
      VERIFY_(rc)
      rc = nf_inq_varid(ncid,'bsca',ivarid)
      VERIFY_(rc)
      rc = nf_get_var_double(ncid,ivarid,bsca_table)
      VERIFY_(rc)
      rc = nf_inq_varid(ncid,'bbck',ivarid)
      VERIFY_(rc)
      rc = nf_get_var_double(ncid,ivarid,bbck_table)
      VERIFY_(rc)
      rc = nf_inq_varid(ncid,'g',ivarid)
      VERIFY_(rc)
      rc = nf_get_var_double(ncid,ivarid,g_table)
      VERIFY_(rc)
      rc = nf_inq_varid(ncid,'rh',ivarid)
      VERIFY_(rc)
      rc = nf_get_var_double(ncid,ivarid,rh_table)
      VERIFY_(rc)

!     Get the backscatter phase function values
      rc = nf_inq_varid(ncid,'pback',ivarid)
      if(rc .ne. NF_NOERR) then   ! pback not in table, fill in dummy variable
        pback_table = 1.
      else
        rc = nf_get_var_double(ncid,ivarid,pback_table)
        VERIFY_(rc)
      endif
 
      if ( nmom_ > 0 ) then
         rc = nf_inq_varid(ncid,'pmom',ivarid)
         VERIFY_(rc)
         rc = nf_get_var_double(ncid,ivarid,pmom_table)
         VERIFY_(rc)
      end if

!     Close the table file
!     -------------------------------------
      rc = nf_close(ncid)
      VERIFY_(rc)

!     Setup the table to be returned
!     -------------------------------------
      this%nlambda = nch
      this%nrh = nrh_table
      this%nbin = nbin_table
      this%nMom = nmom_
!      if ( nmom_ > 0 ) this%nPol = nPol_table
      this%nPol = nPol_table

!      allocate ( this%lambda(this%nLambda), this%rh(this%nrh), &
!                 this%bext(this%nLambda,this%nrh,this%nbin),   &
!                 this%bsca(this%nLambda,this%nrh,this%nbin),   &
!                 this%bbck(this%nLambda,this%nrh,this%nbin),   &
!                 this%g(this%nLambda,this%nrh,this%nbin),      &
!                 stat = rc )

      allocate (this%lambda(this%nLambda),stat = rc )
      VERIFY_(rc)
      allocate (this%rh(this%nrh),stat = rc )
      VERIFY_(rc)
      allocate (this%reff(this%nrh,this%nbin),stat = rc )
      VERIFY_(rc)
      allocate (this%bext(this%nrh,this%nLambda,this%nbin),stat = rc )
      VERIFY_(rc)
      allocate (this%bsca(this%nrh,this%nLambda,this%nbin),stat = rc )
      VERIFY_(rc)
      allocate (this%bbck(this%nrh,this%nLambda,this%nbin),stat = rc )
      VERIFY_(rc)
      allocate (this%g(this%nrh,this%nLambda,this%nbin),   stat = rc )
      VERIFY_(rc)
      allocate (this%pback(this%nrh,this%nLambda,this%nbin,this%nPol),   stat = rc )
      VERIFY_(rc)
      if ( nmom_ > 0 ) then
         allocate (this%pmom(this%nrh,this%nLambda,this%nbin,this%nMom,this%nPol),   stat = rc )
         VERIFY_(rc)
      end if

!     Preserve the full RH structure of the input table
      this%rh(:) = rh_table(:)

!     Insert the requested channels in the output table
      this%lambda(:) = channels(:)

!     Insert rEff (moist effective radius)
      this%reff(:,:) = reff_table(:,:)

!     Now we linearly interpolate the input table to the output table grid
!     of requested channels
      do j = 1, this%nbin
       do i = 1, this%nrh
        do n = 1, this%nlambda
         call polint(channels_table,bext_table(:,i,j),nch_table, &
                     this%lambda(n),this%bext(i,n,j),yerr)
         call polint(channels_table,bsca_table(:,i,j),nch_table, &
                     this%lambda(n),this%bsca(i,n,j),yerr)
         call polint(channels_table,bbck_table(:,i,j),nch_table, &
                     this%lambda(n),this%bbck(i,n,j),yerr)
         call polint(channels_table,g_table(:,i,j),nch_table, &
                     this%lambda(n),this%g(i,n,j),yerr)
         do ipol = 1, this%nPol
                  call polint(channels_table,pback_table(:,i,j,ipol),nch_table, &
                       this%lambda(n),this%pback(i,n,j,ipol),yerr)
         end do
         if ( nmom_ > 0 ) then
            do imom = 1, this%nMom
               do ipol = 1, this%nPol
                  call polint(channels_table,pmom_table(:,i,j,imom,ipol),nch_table, &
                       this%lambda(n),this%pmom(i,n,j,imom,ipol),yerr)
               end do
            end do
         end if
        enddo
       enddo
      enddo

!     Now we do a mapping of the RH from the input table to some high
!     resolution representation.  This is to spare us the need to
!     do a full-up interpolation later on.
!     RH input from the table is scaled 0 - 0.99
!     We resolve the map to 0 - 0.990 in steps of 0.001 (991 total steps)
      do j = 1, 991
       do i = this%nrh, 1, -1
        if( (j-1) .ge. int(this%rh(i)*1000)) then
         ip1 = i + 1
         this%rhi(j) = i
         if(ip1 .gt. this%nrh) then
          this%rha(j) = 0.
         else
          this%rha(j) =   ( (j-1)/1000. - this%rh(i)) &
                       /  ( this%rh(ip1)- this%rh(i))
         endif
         exit
        endif
       enddo
!       print *, j, this%rhi(j), this%rha(j), this%rh(this%rhi(j))
      enddo

!      deallocate (channels_table, rh_table, bext_table, bsca_table, &
!                  bbck_table, g_table, stat = rc )

      deallocate (channels_table, stat = rc )
      VERIFY_(rc)
      deallocate (rh_table, stat = rc )
      VERIFY_(rc)
      deallocate (reff_table, stat = rc )
      VERIFY_(rc)
      deallocate (bext_table, stat = rc )
      VERIFY_(rc)
      deallocate (bsca_table, stat = rc )
      VERIFY_(rc)
      deallocate (bbck_table, stat = rc )
      VERIFY_(rc) 
      deallocate (g_table, stat = rc )
      VERIFY_(rc)
      deallocate (pback_table, stat = rc )
      VERIFY_(rc)
      if ( nmom_ > 0 ) then
         deallocate (pmom_table, stat = rc )
         VERIFY_(rc)
      endif

return

contains

   subroutine polint(x,y,n,xWant,yWant,yErr)
   integer :: n
!  recall, table hard-wired single precision
   real*8 :: x(n),y(n)
   real   :: xWant, yWant, yErr

!  given array x(n) of independent variables and array y(n) of dependent
!  variables, compute the linear interpolated result yWant at xWant and return
!  with a dummy error estimate yErr.  Hacked up from Numerical Recipes Chapter 3

   integer :: i, j
   real    :: dx, slope
   character(len=255) :: msg

!  on out of bounds, set i to lower or upper limit
   i = 0
   if(xWant .lt. x(1)) then 
    write(msg,*) "in polint, wanted: ", xWant, ", got lower bound: ", x(1)
    call warn(myname,msg)
    i = 1
   endif
   if(xWant .gt. x(n)) then 
    write(msg,*) "in polint, wanted: ", xWant, ", got upper bound: ", x(n)
    call warn(myname,msg)
    i = n
   endif

!  if i is still zero find i less than xWant
   if(i .eq. 0) then
    do j = 1, n
     if(xWant .ge. x(j)) i = j
    enddo
   endif

!  slope
   if(i .eq. n) then 
    slope = 0.
   else
    slope = (y(i+1)-y(i)) / (x(i+1)-x(i))
   endif
   dx = xWant - x(i)
   yWant = y(i) + slope*dx

   yErr = 0.

   return
   end subroutine polint

END SUBROUTINE Chem_MieTableRead

!................................................................


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Chem_MieTableGetDims --- Return size of tables.
!
! !INTERFACE:
!
   SUBROUTINE Chem_MieTableGetDims ( mieTableName, nch_table, nrh_table, &
                                     nbin_table, nmom_table, nPol_table, rc )

! !INPUT PARAMETERS:

   IMPLICIT none
   character(len=*), intent(in) :: MieTableName ! table file name
   integer, intent(out)  :: nch_table, nrh_table, nbin_table, nmom_table, nPol_table
   integer, intent(out)  :: rc   ! return code


! !DESCRIPTION:
!
!   Returns the size of the MieTables. It assumes all takes are the same size,
!   so it returns the size based on dust.
!
! !REVISION HISTORY:
!
!  165jun2010  da Silva/Buchard
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'Chem_MieTableRead'

      integer :: ncid, idimid, ivarid

                           __Iam__("Chem_MieTableGetDims")

      rc = 0

!     Open the table and get the dimensions
!     -------------------------------------
      rc = nf_open(mietablename, NF_NOWRITE, ncid)
      VERIFY_(rc)

!     RH
!     --
      rc = nf_inq_dimid(ncid,'rh',idimid)
      VERIFY_(rc)
      rc = nf_inq_dimlen(ncid,idimid,nrh_table)
      VERIFY_(rc)

!     Channels
!     --------
      rc = nf_inq_dimid(ncid,'lambda',idimid)
      VERIFY_(rc)
      rc = nf_inq_dimlen(ncid,idimid,nch_table)
      VERIFY_(rc)

!     Effective radius
!     ----------------
      rc = nf_inq_dimid(ncid,'radius',idimid)
      VERIFY_(rc)
      rc = nf_inq_dimlen(ncid,idimid,nbin_table)
      VERIFY_(rc)

!     Moments of phase function
!     -------------------------
      rc = nf_inq_dimid(ncid,'nMom',idimid)
      if ( rc /= 0 ) then
           nMom_table = 0
           nPol_table = 0
           rc = 0
      else
         rc = nf_inq_dimlen(ncid,idimid,nmom_table)
         VERIFY_(rc)
         rc = nf_inq_dimid(ncid,'nPol',idimid)
         VERIFY_(rc)
         rc = nf_inq_dimlen(ncid,idimid,nPol_table)
         VERIFY_(rc)
      end if

    END SUBROUTINE Chem_MieTableGetDims

 end module Chem_MieTableMod

