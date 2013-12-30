!------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS
!------------------------------------------------------------------------
!BOP
! !ROUTINE: ScatterGatherTest --- Unit tester for PILGRIM_xyMod
!
! !INTERFACE:
      PROGRAM ScatterGatherTest

! !USES:
      USE  shr_kind_mod, only: r8 => shr_kind_r8
      USE  PILGRIM_xyMOD, ONLY: PILGRIM_xy, PILGRIM_xyCreate,            &
                   PILGRIM_xyDestroy, PILGRIM_xyScatter, PILGRIM_xyGather
      USE  parutilitiesmodule, ONLY: CommGlobal, GID, GSize,             &
                   ParInit, ParExit, ParCollective

      IMPLICIT NONE
#if defined(TIMING)
#include "gpt.inc"
#endif

! !DESCRIPTION:
!
!    This main program tests the functionality of the GhostModule
!    It performs the following tests:
!
!    \begin{enumerate}
!      \item Scatter, then gather unghosted 3D array (int4, real4, real8)
!      \item Scatter, then gather ghosted 3D array (int4, real4, real8)
!    \end{enumerate}
!
!    Validation check: 
!
!      mpirun -np 8 ScatterGatherTest
!
!    Should yield a single message (if -DDEBUG_ON is *not* defined):
!
!      Passed all tests
!
!    Be patient, it tests many complex cases, so it could take a while
!
! !REVISION HISTORY:
!   05.04.19   Sawyer     Creation
!
!EOP
!-------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      TYPE (PILGRIM_xy)     :: xy_scattergather

      INTEGER  :: GhostWidth, Im, Jm, Km
      PARAMETER (GhostWidth = 4, Im = 72, Jm = 46, Km = 60 )

! For the Observation decomposition
      INTEGER  ::  BlockLen, I, J, K, Local, Global, Pe
      INTEGER  ::  PEsInX, PEsInY, IamInX, IamInY
      INTEGER  ::  Xstart, Xend, Ystart, Yend
      INTEGER  ::  ig_w, ig_e, jg_s, jg_n
      PARAMETER ( ig_w = 1, ig_e = 2, jg_s = 3, jg_n = 4 )

      LOGICAL :: Passed
      REAL (r8), ALLOCATABLE :: Rtmp3d(:,:,:), RtmpGlobal(:,:,:)
      INTEGER , ALLOCATABLE  :: Xdist(:), Ydist(:)
      INTEGER  :: iw, ie, js, jn

!
! Initialize PILGRIM
!
      CALL ParInit()
      Passed = .TRUE.

!
! Initialize timing library.  2nd arg 0 means disable, 1 means enable
!
#if defined(TIMING)
      call t_setoptionf (usrsys, 1)
      call t_initializef ()
#endif

#if defined(TIMING)
      call t_startf('Scatter/Gather Total')
#endif
!
! Try to find a nice XY decomposition
!
      IF ( Gsize .GT. 1 ) THEN
        PEsInX = 2
        DO WHILE ( MOD(Gsize,PEsInX) .NE. 0 )
          PEsInX = PEsInX + 1
        ENDDO
      ELSE
          PEsInX = 1
      ENDIF
!
! In the worst case PEsInX = Gsize, PEsInY=1
!
      PEsInY = Gsize / PEsInX

!
! Determine this process's coordinates in the virtual topology
!
      IamInY = GID / PEsInX
      IamInX = MOD( GID, PEsInX )

!
! Determine a non-trivial 2D distribution 
!
      ALLOCATE( Xdist( PEsInX ) )
      ALLOCATE( Ydist( PEsInY ) )
!
      BlockLen = Im
      DO I = 1, PEsInX-1
        Xdist( I ) = BlockLen / 2
        BlockLen  = BlockLen - Xdist(I)
      ENDDO
      Xdist( PEsInX ) = BlockLen
 
      DO J = 1, PEsInY-1
        Ydist( J ) = Jm / PEsInY
      ENDDO
      Ydist( PEsInY ) = Jm - (PEsInY-1)*(Jm/PEsInY)

      Xstart = 1
      IF (IamInX .GT. 0) Xstart = SUM( Xdist(1:IamInX) ) + 1
      Xend = Im
      IF (IamInX .LT. PEsInX-1) Xend   = Xstart + Xdist(IamInX+1) - 1
      Ystart = 1
      IF (IamInY .GT. 0) Ystart = SUM( Ydist(1:IamInY) ) + 1
      Yend = Jm
      IF (IamInY .LT. PEsInY-1) Yend   = Ystart + Ydist(IamInY+1) - 1
      print *, "Xs,Xe,Ys,Ye", Xstart, Xend, Ystart, Yend

!
! Test : Ghosted Scatter/Gather  (r8)
!

#if defined(TIMING)
      call t_startf('PILGRIM_xyCreate')
#endif
!
! Create the unghosted ScatterGather object
!
      iw = ig_w
      ie = ig_e
      IF ( PEsInX == 1 )  THEN
        iw = 0
        ie = 0
      ENDIF
      js = jg_s
      jn = jg_n
      IF ( Ystart == 1  ) js = 0    ! Don't use undefined ghost regions
      IF ( Yend   == Jm ) jn = 0    ! Don't use undefined ghost regions
      xy_scattergather = PILGRIM_xyCreate(xdist,ydist,im,jm,km,     &
                                          ig_w = iw, ig_e = ie, &
                                          jg_s = js, jg_n = jn, &
                                          prec="r8")
#if defined(TIMING)
      call t_stopf('PILGRIM_xyCreate')
#endif

! 
! Allocate the local (ghosted array)
!
      ALLOCATE( Rtmp3d(Xstart-iw:Xend+ie,Ystart-js:Yend+jn,1:KM) )
      Rtmp3d = 0

!
! Put the correct global tag into entry of the array, but zero out ghost region
!
      IF ( gid == 0 ) THEN 
        ALLOCATE( RtmpGlobal(1:IM,1:JM,1:KM) )
        DO K=1, KM
          DO J=1, JM
            DO I=1, IM
              RtmpGlobal(I,J,K) = ((K-1)*Jm+(J-1))*Im + I
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
! Do a test with the communication pattern
!
#if defined(TIMING)
      call t_startf('Scatter/Gather R8 (ghosted)')
#endif
      CALL PILGRIM_xyScatter( xy_scattergather, RtmpGlobal, Rtmp3d )
      RtmpGlobal = 0
      CALL PILGRIM_xyGather(  xy_scattergather, Rtmp3d, RtmpGlobal )
#if defined(TIMING)
      call t_stopf('Scatter/Gather R8 (ghosted)')
#endif

!
! Test the scattered array
!
      DO K=1,KM
         DO J=Ystart-js, Yend+jn
           DO I=Xstart-iw, Xend+ie
            Global = ((K-1)*Jm+(J-1))*Im+MOD(I+IM-1,IM)+1   ! MOD takes periodic BC into account
            IF ( Rtmp3D(I,J,K) .NE. Global ) THEN
              print *, "Error on",GID,"Rtmp3d(",I,J,K,")=",Rtmp3d(I,J,K), "not", Global
              Passed = .FALSE.
            ENDIF
          ENDDO
        ENDDO
      ENDDO

!
! Test the Global array on PE 0
!
      IF ( gid == 0 ) THEN
        DO K=1,KM
          DO J=1,JM
            DO I=1,IM
              Global = ((K-1)*Jm+(J-1))*Im+I
              IF ( RtmpGlobal(I,J,K) .NE. Global ) THEN
                print *, "Error on",GID,"RtmpGlobal(",I,J,K,")=",RtmpGlobal(I,J,K), &
                         "not", Global
                Passed = .FALSE.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      CALL ParCollective( CommGlobal )

!
! Free the communication pattern
!
      CALL PILGRIM_xyDestroy( xy_scattergather )

      DEALLOCATE( Rtmp3d )
      IF ( gid == 0 ) DEALLOCATE( RtmpGlobal )

#if defined(TIMING)
      call t_stopf('Scatter/Gather Total')
#endif

#if defined(TIMING)
      call t_prf(GID)
#endif

      DEALLOCATE( Ydist )
      DEALLOCATE( Xdist )



      IF ( Passed ) THEN
        PRINT *, "Passed ScatterGatherTest"
      ELSE
        PRINT *, "Failed ScatterGatherTest"
      ENDIF

      CALL ParExit()

!
! That's all folks
!
!EOC
!-------------------------------------------------------------------------
      END PROGRAM ScatterGatherTest

