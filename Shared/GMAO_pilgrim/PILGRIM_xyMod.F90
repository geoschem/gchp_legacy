!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS
!-------------------------------------------------------------------------
      MODULE PILGRIM_xyMod
!BOP
!
! !MODULE: PILGRIM_xyMod
!
! !USES:
      USE shr_kind_mod, only: r8 => shr_kind_r8, r4 => shr_kind_r4, &
                              i8 => shr_kind_i8, i4 => shr_kind_i4
      USE parutilitiesmodule, ONLY : parpatterntype
      IMPLICIT NONE

! !DESCRIPTION:
!
!      This module provides the xy decomposition object and several methods:
!      \begin{center}
!      \begin{tabular}{|l|l|} \hline \hline
!         PILGRIM_xy        & XY decomposition object \\ \hline
!         PILGRIM_xyCreate  & Create a new object\\ \hline
!         PILGRIM_xyGather  & Gather an XY-decomposed array on PE 0 \\ \hline
!         PILGRIM_xyScatter & Scatter an XY-decomposed array from PE 0 \\ \hline
!         PILGRIM_xyDestroy % Destroy the object \\
!         \hline  \hline
!      \end{tabular}
!      \end{center}
!
! !BUGS:
!   Not yet tested.
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
! !PUBLIC TYPES:
      PUBLIC PILGRIM_xy, PILGRIM_xyCreate, PILGRIM_xyGather,     &
             PILGRIM_xyScatter, PILGRIM_xyDestroy

! Decomposition info

       TYPE PILGRIM_xy
         INTEGER                   ::  datatype
         TYPE ( parpatterntype )   ::  scatter_2d
         TYPE ( parpatterntype )   ::  gather_2d
         TYPE ( parpatterntype )   ::  scatter_3d
         TYPE ( parpatterntype )   ::  gather_3d
       END TYPE PILGRIM_xy

      INTERFACE     PILGRIM_xyScatter
        MODULE PROCEDURE PILGRIM_xyScatter_2d_i4
        MODULE PROCEDURE PILGRIM_xyScatter_2d_r4
        MODULE PROCEDURE PILGRIM_xyScatter_2d_r8
        MODULE PROCEDURE PILGRIM_xyScatter_3d_i4
        MODULE PROCEDURE PILGRIM_xyScatter_3d_r4
        MODULE PROCEDURE PILGRIM_xyScatter_3d_r8
      END INTERFACE

      INTERFACE     PILGRIM_xyGather
        MODULE PROCEDURE PILGRIM_xyGather_2d_i4
        MODULE PROCEDURE PILGRIM_xyGather_2d_r4
        MODULE PROCEDURE PILGRIM_xyGather_2d_r8
        MODULE PROCEDURE PILGRIM_xyGather_3d_i4
        MODULE PROCEDURE PILGRIM_xyGather_3d_r4
        MODULE PROCEDURE PILGRIM_xyGather_3d_r8
      END INTERFACE
!EOP
      CONTAINS

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyCreate --- Create an object
 !
! !INTERFACE:
      FUNCTION PILGRIM_xyCreate(xdist,ydist,im,jm,km,              &
                                ig_w,ig_e,jg_s,jg_n,prec)
! !USES:
      USE decompmodule, ONLY : decomptype, decompcreate, decompfree
      USE ghostmodule, ONLY  : ghosttype, ghostcreate, ghostfree
      USE mod_comm, ONLY : commglobal, gid
      USE parutilitiesmodule, ONLY : INT4, REAL4, REAL8, parpatterncreate
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: xdist(:)          ! Distribution in X
      INTEGER, INTENT(IN) :: ydist(:)          ! Distribution in Y
      INTEGER, INTENT(IN) :: im, jm, km        ! Global dimensions       
      INTEGER, INTENT(IN), OPTIONAL :: ig_w    ! Halo on west
      INTEGER, INTENT(IN), OPTIONAL :: ig_e    ! Halo on east
      INTEGER, INTENT(IN), OPTIONAL :: jg_s    ! Halo on south
      INTEGER, INTENT(IN), OPTIONAL :: jg_n    ! Halo on north
      CHARACTER(2), INTENT( IN ), OPTIONAL :: prec ! Data type ("i4", "r4", "r8")
      TYPE (PILGRIM_xy) :: PILGRIM_xyCreate
! !DESCRIPTION:
!     Create an object from the XY distribution
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!
!EOP
!-----------------------------------------------------------------------
!BOC
!
! 
      TYPE (decomptype) :: dg, dl
      TYPE (ghosttype)  :: gl
      INTEGER, ALLOCATABLE :: xdist_global(:), ydist_global(:), zdist_global(:)
      INTEGER :: npes_x, npes_y, myid_x, myid_y, datatype
      INTEGER :: istart, iend, jstart, jend, iw, ie, js, jn
      IF ( present( prec ) ) THEN
        IF ( prec == "i4" .OR. prec == "I4" ) THEN
          datatype = INT4
        ELSEIF ( prec == "r4" .OR. prec == "R4" ) THEN
          datatype = REAL4
        ELSEIF ( prec == "r8" .OR. prec == "R8" ) THEN
          datatype = REAL8
        ELSE
          datatype = REAL8
        ENDIF
      ELSE
        datatype = REAL8
      ENDIF
      PILGRIM_xyCreate%datatype = datatype       ! store the data type
      npes_x = SIZE(xdist,1)
      npes_y = SIZE(ydist,1)
      myid_x = MOD(gid, npes_x)
      myid_y = gid / npes_x
      istart = sum(xdist(1:myid_x))+1
      iend   = sum(xdist(1:myid_x+1))
      jstart = sum(ydist(1:myid_y))+1
      jend   = sum(ydist(1:myid_y+1))
!!!      print *, "is, ie, js, je", istart, iend, jstart, jend, "myid_x, myid_y", myid_x, myid_y

      call decompcreate( npes_x, npes_y, xdist, ydist, dl )

      ALLOCATE( xdist_global( npes_x ) )
      ALLOCATE( ydist_global( npes_y ) )
      xdist_global = 0
      ydist_global = 0
      xdist_global(1) = im
      ydist_global(1) = jm
      call decompcreate( npes_x, npes_y, xdist_global, ydist_global, dg )
      DEALLOCATE( ydist_global )
      DEALLOCATE( xdist_global )

      iw = 0; ie = 0; js = 0; jn = 0; 
      if ( present( ig_w ) ) iw = ig_w
      if ( present( ig_e ) ) ie = ig_e
      if ( present( jg_s ) ) js = jg_s
      if ( present( jg_n ) ) jn = jg_n

      if ( iw /= 0 .OR. ie /= 0 .OR. js /= 0 .OR. jn /= 0 ) then
        call ghostcreate( dl, Gid, im, istart-iw, iend+ie,.TRUE.,  &
                          jm, jstart-js, jend+jn, .FALSE., gl )
        call parpatterncreate( commglobal, dg, gl, PILGRIM_xyCreate%scatter_2d, &
                               mod_method=0, T=datatype )
        call parpatterncreate( commglobal, gl, dg, PILGRIM_xyCreate%gather_2d,  &
                               mod_method=0, T=datatype )
        call ghostfree( gl )
      else
        call parpatterncreate( commglobal, dg, dl, PILGRIM_xyCreate%scatter_2d, &
                               mod_method=0, T=datatype )
        call parpatterncreate( commglobal, dl, dg, PILGRIM_xyCreate%gather_2d,  &
                               mod_method=0, T=datatype )
      endif
      call decompfree( dg )
      call decompfree( dl )

!
! This code is being debugged:  for ghosted decompositions there is a problem
!
!!!      call parpatterncreate( PILGRIM_xyCreate%scatter_2d,                &
!!!                             PILGRIM_xyCreate%scatter_3d, km )
!!!      call parpatterncreate( PILGRIM_xyCreate%gather_2d,                 &
!!!                             PILGRIM_xyCreate%gather_3d, km )

!
! The rest of the code can be removed if the above code is fixed
!
      ALLOCATE( xdist_global( npes_x ) )
      ALLOCATE( ydist_global( npes_y ) )
      ALLOCATE( zdist_global( 1 ) )
      xdist_global = 0
      ydist_global = 0
      zdist_global = 0
      xdist_global(1) = im
      ydist_global(1) = jm
      zdist_global(1) = km
      call decompcreate( npes_x, npes_y, 1, xdist_global, ydist_global, &
                         zdist_global, dg )

      call decompcreate( npes_x, npes_y, 1, xdist, ydist, zdist_global, dl )
      DEALLOCATE( zdist_global )
      DEALLOCATE( ydist_global )
      DEALLOCATE( xdist_global )

      if ( iw /= 0 .OR. ie /= 0 .OR. js /= 0 .OR. jn /= 0 ) then
        call ghostcreate( dl, Gid, im, istart-iw, iend+ie,.TRUE.,  &
                          jm, jstart-js, jend+jn, .FALSE., &
                          km, 1, km, .FALSE., gl )
        call parpatterncreate( commglobal, dg, gl, PILGRIM_xyCreate%scatter_3d, &
                               mod_method=0, T=datatype )
        call parpatterncreate( commglobal, gl, dg, PILGRIM_xyCreate%gather_3d, &
                               mod_method=0, T=datatype )
        call ghostfree( gl )
      else
        call parpatterncreate( commglobal, dg, dl, PILGRIM_xyCreate%scatter_3d, &
                               mod_method=0, T=datatype )
        call parpatterncreate( commglobal, dl, dg, PILGRIM_xyCreate%gather_3d,  &
                               mod_method=0, T=datatype )
      endif

      call decompfree( dg )
      call decompfree( dl )

      RETURN
!EOC
      END FUNCTION PILGRIM_xyCreate
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyDestroy  --- Destroy an XY object
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyDestroy( this )
! !USES:
      USE decompmodule, ONLY : decompfree
      USE ghostmodule, ONLY : ghostfree
      USE mod_comm, ONLY : commglobal
      USE parutilitiesmodule, ONLY : parpatternfree
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(INOUT):: this         ! XY Object
!
! !DESCRIPTION:
!     Deallocate the xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!
!EOP
!------------------------------------------------------------------------
!BOC
      CALL parpatternfree( commglobal, this%scatter_2d )
      CALL parpatternfree( commglobal, this%gather_2d )
      CALL parpatternfree( commglobal, this%scatter_3d )
      CALL parpatternfree( commglobal, this%gather_3d )

      RETURN
!EOC
      END SUBROUTINE PILGRIM_xyDestroy
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyScatter_2d_i4 --- Scatter a 2d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyScatter_2d_i4( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : INT4
      USE mod_comm, ONLY : commglobal, mp_swapirr_i4
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      INTEGER(i4), INTENT(IN)     :: qin(:,:)     ! Input array (global)
      INTEGER(i4), INTENT(INOUT)  :: qout(:,:)    ! Ouput array (local)
!
! !DESCRIPTION:
!     Scatter a 2D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= INT4 ) THEN
        stop "PILGRIM_xyScatter_2d_i4: data type is not INT4"
      ENDIF
      CALL mp_swapirr_i4(commglobal, this%scatter_2d%SendDesc,            &
                         this%scatter_2d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyScatter_2d_i4
!------------------------------------------------------------------------


!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyGather_2d_i4 --- Gather a 2d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyGather_2d_i4( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : INT4
      USE mod_comm, ONLY : commglobal, mp_swapirr_i4
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      INTEGER(i4), INTENT(IN)        :: qin(:,:)  ! Input array (local)
      INTEGER(i4), INTENT(INOUT)     :: qout(:,:) ! Ouput array (global)
!
! !DESCRIPTION:
!     Gather a 2D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= INT4 ) THEN
        stop "PILGRIM_xyGather_2d_i4: data type is not INT4"
      ENDIF
      CALL mp_swapirr_i4(commglobal, this%gather_2d%SendDesc,             &
                         this%gather_2d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyGather_2d_i4
!------------------------------------------------------------------------

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyScatter_2d_r4 --- Scatter a 2d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyScatter_2d_r4( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : REAL4
      USE mod_comm, ONLY : commglobal, mp_swapirr_r4
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      REAL(r4), INTENT(IN)        :: qin(:,:)     ! Input array (global)
      REAL(r4), INTENT(INOUT)     :: qout(:,:)    ! Ouput array (local)
!
! !DESCRIPTION:
!     Scatter a 2D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= REAL4 ) THEN
        stop "PILGRIM_xyScatter_2d_r4: data type is not REAL4"
      ENDIF
      CALL mp_swapirr_r4(commglobal, this%scatter_2d%SendDesc,            &
                         this%scatter_2d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyScatter_2d_r4
!------------------------------------------------------------------------


!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyGather_2d_r4 --- Gather a 2d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyGather_2d_r4( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : REAL4
      USE mod_comm, ONLY : commglobal, mp_swapirr_r4
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      REAL(r4), INTENT(IN)        :: qin(:,:)     ! Input array (local)
      REAL(r4), INTENT(INOUT)     :: qout(:,:)    ! Ouput array (global)
!
! !DESCRIPTION:
!     Gather a 2D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= REAL4 ) THEN
        stop "PILGRIM_xyGather_2d_r4: data type is not REAL4"
      ENDIF
      CALL mp_swapirr_r4(commglobal, this%gather_2d%SendDesc,             &
                         this%gather_2d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyGather_2d_r4
!------------------------------------------------------------------------

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyScatter_2d_r8 --- Scatter a 2d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyScatter_2d_r8( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : REAL8
      USE mod_comm, ONLY : commglobal, mp_swapirr
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      REAL(r8), INTENT(IN)        :: qin(:,:)     ! Input array (global)
      REAL(r8), INTENT(INOUT)     :: qout(:,:)    ! Ouput array (local)
!
! !DESCRIPTION:
!     Scatter a 2D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= REAL8 ) THEN
        stop "PILGRIM_xyScatter_2d_r8: data type is not REAL8"
      ENDIF
      CALL mp_swapirr(commglobal, this%scatter_2d%SendDesc,               &
                      this%scatter_2d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyScatter_2d_r8
!------------------------------------------------------------------------


!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyGather_2d_r8 --- Gather a 2d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyGather_2d_r8( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : REAL8
      USE mod_comm, ONLY : commglobal, mp_swapirr
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      REAL(r8), INTENT(IN)        :: qin(:,:)     ! Input array (local)
      REAL(r8), INTENT(INOUT)     :: qout(:,:)    ! Ouput array (global)
!
! !DESCRIPTION:
!     Gather a 2D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= REAL8 ) THEN
        stop "PILGRIM_xyGather_2d_r8: data type is not REAL8"
      ENDIF
      CALL mp_swapirr(commglobal, this%gather_2d%SendDesc,                &
                      this%gather_2d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyGather_2d_r8
!------------------------------------------------------------------------



!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyScatter_3d_i4 --- Scatter a 3d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyScatter_3d_i4( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : INT4
      USE mod_comm, ONLY : commglobal, mp_swapirr_i4
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      INTEGER(i4), INTENT(IN)     :: qin(:,:,:)   ! Input array (global)
      INTEGER(i4), INTENT(INOUT)  :: qout(:,:,:)  ! Ouput array (local)
!
! !DESCRIPTION:
!     Scatter a 3D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= INT4 ) THEN
        stop "PILGRIM_xyScatter_3d_i4: data type is not INT4"
      ENDIF
      CALL mp_swapirr_i4(commglobal, this%scatter_3d%SendDesc,           &
                         this%scatter_3d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyScatter_3d_i4
!------------------------------------------------------------------------


!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyGather_3d_i4 --- Gather a 3d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyGather_3d_i4( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : INT4
      USE mod_comm, ONLY : commglobal, mp_swapirr_i4
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      INTEGER(i4), INTENT(IN)     :: qin(:,:,:)   ! Input array (local)
      INTEGER(i4), INTENT(INOUT)  :: qout(:,:,:)  ! Ouput array (global)
!
! !DESCRIPTION:
!     Gather a 3D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= INT4 ) THEN
        stop "PILGRIM_xyGather_3d_i4: data type is not INT4"
      ENDIF
      CALL mp_swapirr_i4(commglobal, this%gather_3d%SendDesc,             &
                         this%gather_3d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyGather_3d_i4
!------------------------------------------------------------------------

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyScatter_3d_r4 --- Scatter a 3d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyScatter_3d_r4( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : REAL4
      USE mod_comm, ONLY : commglobal, mp_swapirr_r4
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      REAL(r4), INTENT(IN)        :: qin(:,:,:)   ! Input array (global)
      REAL(r4), INTENT(INOUT)     :: qout(:,:,:)  ! Ouput array (local)
!
! !DESCRIPTION:
!     Scatter a 3D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= REAL4 ) THEN
        stop "PILGRIM_xyScatter_3d_r4: data type is not REAL4"
      ENDIF
      CALL mp_swapirr_r4(commglobal, this%scatter_3d%SendDesc,            &
                         this%scatter_3d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyScatter_3d_r4
!------------------------------------------------------------------------


!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyGather_3d_r4 --- Gather a 3d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyGather_3d_r4( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : REAL4
      USE mod_comm, ONLY : commglobal, mp_swapirr_r4
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      REAL(r4), INTENT(IN)        :: qin(:,:,:)   ! Input array (local)
      REAL(r4), INTENT(INOUT)     :: qout(:,:,:)  ! Ouput array (global)
!
! !DESCRIPTION:
!     Gather a 3D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= REAL4 ) THEN
        stop "PILGRIM_xyGather_3d_r4: data type is not REAL4"
      ENDIF
      CALL mp_swapirr_r4(commglobal, this%gather_3d%SendDesc,             &
                         this%gather_3d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyGather_3d_r4
!------------------------------------------------------------------------

!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyScatter_3d_r8 --- Scatter a 3d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyScatter_3d_r8( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : REAL8
      USE mod_comm, ONLY : commglobal, mp_swapirr
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      REAL(r8), INTENT(IN)        :: qin(:,:,:)   ! Input array (global)
      REAL(r8), INTENT(INOUT)     :: qout(:,:,:)  ! Ouput array (local)
!
! !DESCRIPTION:
!     Scatter a 3D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= REAL8 ) THEN
        stop "PILGRIM_xyScatter_3d_r8: data type is not REAL8"
      ENDIF
      CALL mp_swapirr(commglobal, this%scatter_3d%SendDesc,               &
                      this%scatter_3d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyScatter_3d_r8
!------------------------------------------------------------------------


!-----------------------------------------------------------------------
!BOP
! !IROUTINE: PILGRIM_xyGather_3d_r8 --- Gather a 3d array
!
! !INTERFACE:
      SUBROUTINE PILGRIM_xyGather_3d_r8( this, qin, qout )
! !USES:
      USE parutilitiesmodule, ONLY : REAL8
      USE mod_comm, ONLY : commglobal, mp_swapirr
      IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:
      TYPE(PILGRIM_xy), INTENT(IN):: this         ! XY Object
      REAL(r8), INTENT(IN)        :: qin(:,:,:)   ! Input array (local)
      REAL(r8), INTENT(INOUT)     :: qout(:,:,:)  ! Ouput array (global)
!
! !DESCRIPTION:
!     Gather a 3D array based on xy object "this"
!
! !REVISION HISTORY:
!   05.03.09   Sawyer     Creation
!   09.04.01   Sawyer     Upgraded to PILGRIM/cam3_6_33 (w/ communicators)
!
!EOP
!------------------------------------------------------------------------
!BOC
      IF ( this%datatype /= REAL8 ) THEN
        stop "PILGRIM_xyGather_3d_r8: data type is not REAL8"
      ENDIF
      CALL mp_swapirr(commglobal, this%gather_3d%SendDesc,                &
                      this%gather_3d%RecvDesc, qin, qout)
!EOC
      END SUBROUTINE PILGRIM_xyGather_3d_r8
!------------------------------------------------------------------------


      END MODULE PILGRIM_xyMod
