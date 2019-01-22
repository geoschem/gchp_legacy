#include "MAPL_Generic.h"

!BOP

! !MODULE: OVP -- Utility routines for generating exports for
!                 local overpass times.  Such exports must be
!                 accumulated over 24 hours and then written out.

! !INTERFACE:

module OVP

! !USES:

  use ESMF
  use MAPL_Mod
  
  implicit none
  private

! !PUBLIC ROUTINES:

  public OVP_init
  public OVP_end_of_timestep_hms
  public OVP_mask
  public OVP_apply_mask

 interface OVP_apply_mask
    module procedure OVP_apply_mask_2D
    module procedure OVP_apply_mask_3D
 end interface


! !DESCRIPTION:

!  This module enables the construction and use of Overpass Masks.
!  Each mask pertains to a particular "local time", and each gridbox
!  is assigned the Hour-Minute-Second INTEGER value for which
!  the model will complete a timestep, and which is the GMT that
!  most closely matches the "local time" of the mask.
!  Usage of the Mask: After a standard export field (e.g. T) is computed,
!  its values should be copied to an OVP export only for the gridboxes
!  for which the Mask == 'End of Current Timestep'.
!
!  NOTES:
!  We identify two timesteps:  the RUN_DT (heartbeat), and the
!  GridComp timestep (e.g. GMICHEM_DT).  The OVP code needs to know
!  RUN_DT when it computes the "end of current timestep", because CLOCK
!  holds the "beginning of current timestep".  And each GridComp decides
!  how often its exports are updated, either every RUN_DT seconds, or
!  every GC_DT seconds; it must initialize the OVP masks accordingly.
!
!  June 17, 2016  Michael Manyin  NASA/GSFC Code 614


!EOP

contains

!==========================================================================

!BOP

! !IROUTINE OVP_init -- Return some initial values

! !INTERFACE

  subroutine OVP_init ( GC, GC_DT_LABEL, LONS, RUN_DT, GC_DT, RC )

! !ARGUMENTS

     TYPE(ESMF_GridComp), INTENT(inout)  :: GC           ! Gridded component 
     CHARACTER(len=*),    INTENT(in)     :: GC_DT_LABEL  ! String to get the GridComp-specific timestep

     REAL(ESMF_KIND_R4), POINTER, INTENT(out) :: LONS(:,:)    ! radians
     INTEGER,                     INTENT(out) :: RUN_DT       ! main timestep (seconds)
     INTEGER,                     INTENT(out) :: GC_DT        ! GridComp timestep (seconds)
     INTEGER, OPTIONAL,           INTENT(out) :: RC           ! Error code

! !DESCRIPTION

!  This subroutine returns values that are needed by other OVP routines
!
! \begin{description}
!   \item[GC]
!                   Gridded Component
!   \item[GC_DT_LABEL]
!                   Configuration string for the GridComp timestep
!   \item[LONS]
!                   Gridbox longitudes (radians)
!   \item[RUN_DT]
!                   Main Timestep (seconds)
!   \item[GC_DT]
!                   GridComp Timestep (seconds)
!   \item[RC]
!                   Return Code (optional)
! \end{description}
!

!EOP

! Locals
     CHARACTER(len=ESMF_MAXSTR)      :: IAm
     INTEGER                         :: STATUS

     type (MAPL_MetaComp),  pointer  :: STATE
     TYPE (ESMF_Config)              :: CF
     REAL                            :: dt1, dt2

! Comment
! ---------------------------------------

    IAm = "OVP_init"

    call MAPL_GetObjectFromGC ( GC, STATE, __RC__ )                   !  Get MAPL state

    call MAPL_Get( STATE, LONS=LONS, __RC__ )                         !  Get LONS

    call ESMF_GridCompGet ( GC, CONFIG=CF, __RC__ )                   !  Get Config

    call ESMF_ConfigGetAttribute ( CF, dt1, Label="RUN_DT:", __RC__ ) !  Get time step

    RUN_DT = dt1 + 0.01

    call ESMF_ConfigGetAttribute ( CF, dt2, Label=GC_DT_LABEL, DEFAULT=dt1, __RC__ )

    GC_DT  = dt2 + 0.01

    RETURN

  end subroutine OVP_init

!==========================================================================

!BOP

! !IROUTINE OVP_mask -- Compute a Mask

! !INTERFACE

  subroutine OVP_mask ( LONS, DELTA_TIME, OVERPASS_HOUR, MASK )

! !ARGUMENTS

     REAL,                 INTENT(IN)  :: LONS(:,:)      ! radians
     INTEGER,              INTENT(IN)  :: DELTA_TIME     ! seconds
     INTEGER,              INTENT(IN)  :: OVERPASS_HOUR
     INTEGER, ALLOCATABLE, INTENT(OUT) :: MASK(:,:)      ! timestep closest to local overpass time, packed in hr/min/sec

! !DESCRIPTION

!  This subroutine creates an Overpass Mask, which is an array of
!  hour/minute/second values, expressed in GMT, which are the timestep
!  completion times that are closest to OVERPASS_HOUR in local time.
!  The arguments are:
!
! \begin{description}
!   \item[LONS]
!                   Gridbox longitudes (radians)
!   \item[DELTA_TIME]
!                   Timestep (seconds)
!   \item[OVERPASS_HOUR]
!                   Local time of day for the mask (hour)
!   \item[MASK]
!                   The output mask, allocated and returned (hr/min/sec)
! \end{description}
!

!EOP

! Locals

     INTEGER              :: I,J,K,iH,iM,iS,VAL

     REAL,    ALLOCATABLE :: SECONDS_AWAY(:,:)   ! local time is this far away from OVERPASS_HOUR
     REAL,    ALLOCATABLE ::         BEST(:,:)   ! closest that local time has gotten to OVERPASS_HOUR

     INTEGER              :: TIME

! Comment
! ---------------------------------------

     ALLOCATE( SECONDS_AWAY( SIZE(LONS(:,1)), SIZE(LONS(1,:)) ) )   ! this will be temporary
     ALLOCATE(         BEST( SIZE(LONS(:,1)), SIZE(LONS(1,:)) ) )   ! this will be temporary
     ALLOCATE(         MASK( SIZE(LONS(:,1)), SIZE(LONS(1,:)) ) )   ! RETURN VALUE


     SECONDS_AWAY = LONS*(180._8/MAPL_PI_R8)*240. - OVERPASS_HOUR*60*60

     CALL RESET_TIME_RANGE(SECONDS_AWAY)

     BEST = SECONDS_AWAY

     TIME = 0
     MASK(:,:) = TIME

     DO
       TIME = TIME + DELTA_TIME
       IF ( TIME .GE. 24*60*60 ) EXIT
       SECONDS_AWAY = SECONDS_AWAY + DELTA_TIME
       CALL RESET_TIME_RANGE(SECONDS_AWAY)

!      IF GREENWICH IS AT CENTER OF GRIDBOX AND EACH TIMESTEP
!      IS TO MATCH AN EVEN NUMBER OF GRID COLUMNS -- OR --
!      IF GREENWICH IS BETWEEN 2 GRIDBOXES AND EACH TIMESTEP
!      IS TO MATCH AN ODD NUMBER OF GRID COLUMNS -- THEN
!      ARBITRARILY DISALLOW THE POSITIVE dt/2 FROM MATCHING
!      (ALLOW THE NEGATIVE dt/2 TO MATCH)
       WHERE( (ABS(SECONDS_AWAY) .LE. ABS(BEST)) .AND. &
              (SECONDS_AWAY /= DELTA_TIME/2) )
         MASK = TIME
         BEST = SECONDS_AWAY
       ENDWHERE
     ENDDO

!    Convert the MASK to be HMS:
     DO J=1,SIZE(MASK(1,:))
     DO I=1,SIZE(MASK(:,1))
       VAL=MASK(I,J)
       iH = VAL/3600
       VAL = VAL - iH*3600
       iM = VAL/60
       iS = VAL - iM*60
!      PRINT "(I2,'h ',I2,'m ',I2,'s  ')",iH,iM,iS
       CALL MAPL_PackTime(VAL,iH,iM,iS)
       MASK(I,J) = VAL
     ENDDO
     ENDDO

       IF(MAPL_AM_I_ROOT()) THEN
         PRINT *, "MASK min and max : ", MINVAL(MASK), MAXVAL(MASK)
       END IF


     DEALLOCATE( SECONDS_AWAY )
     DEALLOCATE(         BEST )

     RETURN

  end subroutine OVP_mask


!==========================================================================

!BOP

! !IROUTINE OVP_end_of_timestep_hms -- Return the hr/min/sec of the end of current timestep

! !INTERFACE

   INTEGER FUNCTION OVP_end_of_timestep_hms( CLOCK, IDT )

! !ARGUMENTS

     type(ESMF_Clock),    intent(in) :: CLOCK  ! The clock
     INTEGER,             INTENT(in) :: IDT    ! Main timestep (seconds)

! !DESCRIPTION

!  Return the end of the current timestep packed into INTEGER form (hhmmss).

!
! \begin{description}
!   \item[CLOCK]
!                   ESMF Clock object
!   \item[IDT]
!                   Length of timestep (delta T) in seconds
! \end{description}
!

!EOP

! Locals

     character(len=ESMF_MAXSTR)      :: IAm
     integer                         :: STATUS, RC
     type(ESMF_Time)                 :: TIME
     integer                         :: IYR, IMM, IDD, IHR, IMN, ISC
     INTEGER                         :: EXTRA_SECONDS, EXTRA_MINUTES, EXTRA_HOURS, EXTRA_DAYS
     integer                         :: ans

     IAm = "OVP_end_of_timestep_hms"

     call ESMF_ClockGet(CLOCK,currTIME=TIME,__RC__)                                    !  Get the time
     call ESMF_TimeGet(TIME, YY=IYR, MM=IMM, DD=IDD, H=IHR, M=IMN, S=ISC, __RC__)      !  Break apart

   ! Add timestep (sec) to the time, do not care about what day it is
     EXTRA_SECONDS = IDT
     ISC = ISC + EXTRA_SECONDS

     EXTRA_MINUTES = ISC/60
     ISC = ISC - EXTRA_MINUTES*60
     IMN = IMN + EXTRA_MINUTES

     EXTRA_HOURS = IMN/60
     IMN = IMN - EXTRA_HOURS*60
     IHR = IHR + EXTRA_HOURS

     EXTRA_DAYS = IHR/24
     IHR = IHR - EXTRA_DAYS*24

   ! Pack the new time
     call MAPL_PackTime(ans,IHR,IMN,ISC)

     OVP_end_of_timestep_hms = ans

   END FUNCTION OVP_end_of_timestep_hms


!==========================================================================

!BOP

! !IROUTINE OVP_apply_mask_2D -- Copy over select values from the source field to the overpass field

! !INTERFACE

  SUBROUTINE OVP_apply_mask_2D( SOURCE_FIELD, OVERPASS_FIELD, OVP_MASK, FIRST_HMS, CURRENT_HMS, SCALE, RC )

! !ARGUMENTS

     REAL, POINTER, DIMENSION(:,:),   INTENT(in)  :: SOURCE_FIELD
     REAL, POINTER, DIMENSION(:,:),   INTENT(out) :: OVERPASS_FIELD
     INTEGER,       DIMENSION(:,:),   INTENT(in)  :: OVP_MASK
     INTEGER,                         INTENT(in)  :: FIRST_HMS
     INTEGER,                         INTENT(in)  :: CURRENT_HMS
     REAL, OPTIONAL,                  INTENT(in)  :: SCALE
     INTEGER, OPTIONAL,               INTENT(out) :: RC           ! Error code

! !DESCRIPTION

!  Apply mask to the source field input, and copy the resulting values into the overpass array.
!
! \begin{description}
!   \item[SOURCE_FIELD]
!                   Input array
!   \item[OVERPASS_FIELD]
!                   Output array
!   \item[OVP_MASK]
!                   Array of integer values
!   \item[FIRST_HMS]
!                   Time at which output array should be set to default value (hhmmss packed in INTEGER)
!   \item[CURRENT_HMS]
!                   End of current timestep  (hhmmss packed in INTEGER)
!   \item[SCALE]
!                   Optional scaling factor to apply
!   \item[RC]
!                   Return Code (optional)
! \end{description}
!

!EOP

! Locals

     CHARACTER(len=ESMF_MAXSTR)      :: IAm
     INTEGER                         :: STATUS

    IAm = "OVP_apply_mask_2D"
    RC = ESMF_SUCCESS

    IF( ASSOCIATED(OVERPASS_FIELD) ) THEN

      IF( .NOT. ASSOCIATED(SOURCE_FIELD) ) THEN
        IF(MAPL_AM_I_ROOT()) PRINT *, "OVP_apply_mask_2D: Cannot extract values because source-field is not available"
        RC=99
        VERIFY_(RC)
      END IF

      IF ( CURRENT_HMS == FIRST_HMS ) OVERPASS_FIELD(:,:) = -987.0    ! SET UNDEFINED ON FIRST TIMESTEP OF THE DAY

      IF ( PRESENT(SCALE) ) THEN
        WHERE(OVP_MASK==CURRENT_HMS) OVERPASS_FIELD(:,:) = SOURCE_FIELD(:,:) * SCALE
      ELSE
        WHERE(OVP_MASK==CURRENT_HMS) OVERPASS_FIELD(:,:) = SOURCE_FIELD(:,:)
      END IF

    ENDIF

   RETURN
   END SUBROUTINE OVP_apply_mask_2D

!==========================================================================

!BOP

! !IROUTINE OVP_apply_mask_3D -- Copy over select values from the source field to the overpass field

! !INTERFACE

   SUBROUTINE OVP_apply_mask_3D( SOURCE_FIELD, OVERPASS_FIELD, OVP_MASK, FIRST_HMS, CURRENT_HMS, K_EDGES, SCALE, RC )

! !ARGUMENTS

     REAL, POINTER, DIMENSION(:,:,:), INTENT(in)  :: SOURCE_FIELD
     REAL, POINTER, DIMENSION(:,:,:), INTENT(out) :: OVERPASS_FIELD
     INTEGER,       DIMENSION(:,:),   INTENT(in)  :: OVP_MASK
     INTEGER,                         INTENT(in)  :: FIRST_HMS
     INTEGER,                         INTENT(in)  :: CURRENT_HMS
     LOGICAL,                         INTENT(in)  :: K_EDGES     ! TRUE: k starts at 0, FALSE: k starts at 1
     REAL, OPTIONAL,                  INTENT(in)  :: SCALE
     INTEGER, OPTIONAL,               INTENT(out) :: RC           ! Error code

! !DESCRIPTION

!  Apply mask to the source field input, and copy the resulting values into the overpass array.
!
! \begin{description}
!   \item[SOURCE_FIELD]
!                   Input array
!   \item[OVERPASS_FIELD]
!                   Output array
!   \item[OVP_MASK]
!                   Array of integer values
!   \item[FIRST_HMS]
!                   Time at which output array should be set to default value (hhmmss packed in INTEGER)
!   \item[CURRENT_HMS]
!                   End of current timestep  (hhmmss packed in INTEGER)
!   \item[K_EDGES]
!                   TRUE => vertical levels are edges (k starts at 0), FALSE => centers (k starts at 1)
!   \item[SCALE]
!                   Optional scaling factor to apply
!   \item[RC]
!                   Return Code (optional)
! \end{description}
!

!EOP

! Locals

     CHARACTER(len=ESMF_MAXSTR)      :: IAm
     INTEGER                         :: STATUS
     INTEGER                         :: k,km,kmin,kmax

    IAm = "OVP_apply_mask_3D"
    RC = ESMF_SUCCESS

    IF( ASSOCIATED(OVERPASS_FIELD) ) THEN

      IF( .NOT. ASSOCIATED(SOURCE_FIELD) ) THEN
        IF(MAPL_AM_I_ROOT()) PRINT *, "OVP_apply_mask_3D: Cannot extract values because source-field is not available"
        RC=99
        VERIFY_(RC)
      END IF

      km = SIZE(SOURCE_FIELD(1,1,:))

      IF ( K_EDGES ) THEN
        kmin = 0
        kmax = km-1
      ELSE
        kmin = 1
        kmax = km
      END IF

      IF ( CURRENT_HMS == FIRST_HMS ) OVERPASS_FIELD(:,:,:) = -987.0    ! SET UNDEFINED ON FIRST TIMESTEP OF THE DAY

      IF ( PRESENT(SCALE) ) THEN
        DO k = kmin,kmax
          WHERE(OVP_MASK==CURRENT_HMS) OVERPASS_FIELD(:,:,k) = SOURCE_FIELD(:,:,k) * SCALE
        ENDDO
      ELSE
        DO k = kmin,kmax
          WHERE(OVP_MASK==CURRENT_HMS) OVERPASS_FIELD(:,:,k) = SOURCE_FIELD(:,:,k)
        ENDDO
      END IF

    ENDIF

   RETURN
   END SUBROUTINE OVP_apply_mask_3D


!!!  Private routines:

!--------------------------------------------------------
!     NASA/GSFC, Chemistry and Dynamics, Code 614       !
!--------------------------------------------------------
!
!  Given an offset in seconds, if the value is outside
!  the interval (-12 to 12), push it closer to that interval
!  by 24 hours.
!
   SUBROUTINE RESET_TIME_RANGE(A)
     REAL :: A(:,:)

     WHERE( A .GT. 43200. )
       A = A - 86400.
     ENDWHERE

     WHERE( A .LE. -43200. )
       A = A + 86400.
     ENDWHERE

   RETURN
   END SUBROUTINE RESET_TIME_RANGE

!! !--------------------------------------------------------
!! !     NASA/GSFC, Chemistry and Dynamics, Code 614       !
!! !--------------------------------------------------------
!! !
!! !  Convert time interval in seconds, to hour_minute_second
!! !  packed into an INTEGER
!! !
!!    INTEGER FUNCTION SEC_TO_HHMMSS( SEC )
!!      INTEGER, INTENT(IN) :: SEC
!! 
!!      INTEGER              :: iH,iM,iS,VAL
!! 
!!      VAL=SEC
!!      iH = VAL/3600
!!      VAL = VAL - iH*3600
!!      iM = VAL/60
!!      iS = VAL - iM*60
!!      CALL MAPL_PackTime(VAL,iH,iM,iS)
!!      SEC_TO_HHMMSS = VAL
!! 
!!    END FUNCTION SEC_TO_HHMMSS

end module OVP
