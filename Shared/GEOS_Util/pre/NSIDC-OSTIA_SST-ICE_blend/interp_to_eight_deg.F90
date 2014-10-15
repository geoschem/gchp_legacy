!
! This subroutine is SAME as Larry's interp_reynolds.F90 [.../pre/OSTIA/interp_reynolds.F90]
!
      subroutine interp_to_eight_deg (qi,imi,jmi,qo,imo,jmo,undef)
      implicit none

      integer,intent(in) :: imi,jmi
      integer,intent(in) :: imo,jmo
      real,   intent(in) :: undef
      real,   intent(in) :: qi(imi,jmi)
      real,   intent(out):: qo(imo,jmo)

      integer ib,jb
      integer ii1,ii2,ii3,ii4
      integer ji1,ji2,ji3,ji4
      integer io1,io2,io3,io4
      integer jo1,jo2,jo3,jo4

      real qz(imi,0:jmi+1)

      if( imi.ne.2*jmi .or. &
          imo.ne.2*jmo .or. &
          imo.ne.2*imi .or. &
          jmo.ne.2*jmi ) then
          PRINT *
          PRINT *, 'ERROR!       Output Resolution: ',imo,jmo
          PRINT *, 'must be twice Input Resolution: ',imi,jmi
          PRINT *
          stop 1
      endif

      qo = undef

      qz(:,0    ) = undef
      qz(:,1:jmi) = qi
      qz(:,jmi+1) = undef

      do jb=0,jmi
      do ib=1,imi

      ii1 = ib ; ii2 = ib+1 ; ii3 = ib+1 ; ii4 = ib
      ji1 = jb ; ji2 = jb   ; ji3 = jb+1 ; ji4 = jb+1

      io1 = 2*ib ; io2 = 2*ib+1 ; io3 = 2*ib+1 ; io4 = 2*ib
      jo1 = 2*jb ; jo2 = 2*jb   ; jo3 = 2*jb+1 ; jo4 = 2*jb+1

      if( ii2.gt.imi ) ii2 = ii2-imi
      if( ii3.gt.imi ) ii3 = ii3-imi
      if( io2.gt.imo ) io2 = io2-imo
      if( io3.gt.imo ) io3 = io3-imo

! Case (a)
! --------
      if( qz(ii1,ji1).ne.undef   .and. &
          qz(ii2,ji2).eq.undef   .and. &
          qz(ii3,ji3).eq.undef   .and. &
          qz(ii4,ji4).eq.undef ) then
          qo(io1,jo1) = qz(ii1,ji1)
      endif

! Case (b)
! --------
      if( qz(ii1,ji1).eq.undef   .and. &
          qz(ii2,ji2).ne.undef   .and. &
          qz(ii3,ji3).eq.undef   .and. &
          qz(ii4,ji4).eq.undef ) then
          qo(io2,jo2) = qz(ii2,ji2)
      endif

! Case (c)
! --------
      if( qz(ii1,ji1).eq.undef   .and. &
          qz(ii2,ji2).eq.undef   .and. &
          qz(ii3,ji3).ne.undef   .and. &
          qz(ii4,ji4).eq.undef ) then
          qo(io3,jo3) = qz(ii3,ji3)
      endif

! Case (d)
! --------
      if( qz(ii1,ji1).eq.undef   .and. &
          qz(ii2,ji2).eq.undef   .and. &
          qz(ii3,ji3).eq.undef   .and. &
          qz(ii4,ji4).ne.undef ) then
          qo(io4,jo4) = qz(ii4,ji4)
      endif

! Case (e)
! --------
      if( qz(ii1,ji1).ne.undef   .and. &
          qz(ii2,ji2).ne.undef   .and. &
          qz(ii3,ji3).eq.undef   .and. &
          qz(ii4,ji4).eq.undef ) then
          qo(io1,jo1) = 0.75*qz(ii1,ji1) + 0.25*qz(ii2,ji2)
          qo(io2,jo2) = 0.25*qz(ii1,ji1) + 0.75*qz(ii2,ji2)
      endif

! Case (f)
! --------
      if( qz(ii1,ji1).eq.undef   .and. &
          qz(ii2,ji2).ne.undef   .and. &
          qz(ii3,ji3).ne.undef   .and. &
          qz(ii4,ji4).eq.undef ) then
          qo(io2,jo2) = 0.75*qz(ii2,ji2) + 0.25*qz(ii3,ji3)
          qo(io3,jo3) = 0.25*qz(ii2,ji2) + 0.75*qz(ii3,ji3)
      endif

! Case (g)
! --------
      if( qz(ii1,ji1).eq.undef   .and. &
          qz(ii2,ji2).eq.undef   .and. &
          qz(ii3,ji3).ne.undef   .and. &
          qz(ii4,ji4).ne.undef ) then
          qo(io3,jo3) = 0.75*qz(ii3,ji3) + 0.25*qz(ii4,ji4)
          qo(io4,jo4) = 0.25*qz(ii3,ji3) + 0.75*qz(ii4,ji4)
      endif

! Case (h)
! --------
      if( qz(ii1,ji1).ne.undef   .and. &
          qz(ii2,ji2).eq.undef   .and. &
          qz(ii3,ji3).eq.undef   .and. &
          qz(ii4,ji4).ne.undef ) then
          qo(io1,jo1) = 0.75*qz(ii1,ji1) + 0.25*qz(ii4,ji4)
          qo(io4,jo4) = 0.25*qz(ii1,ji1) + 0.75*qz(ii4,ji4)
      endif

! Case (i)
! --------
      if( qz(ii1,ji1).ne.undef   .and. &
          qz(ii2,ji2).eq.undef   .and. &
          qz(ii3,ji3).ne.undef   .and. &
          qz(ii4,ji4).eq.undef ) then
          qo(io1,jo1) = 0.75*qz(ii1,ji1) + 0.25*qz(ii3,ji3)
          qo(io3,jo3) = 0.25*qz(ii1,ji1) + 0.75*qz(ii3,ji3)
      endif

! Case (j)
! --------
      if( qz(ii1,ji1).eq.undef   .and. &
          qz(ii2,ji2).ne.undef   .and. &
          qz(ii3,ji3).eq.undef   .and. &
          qz(ii4,ji4).ne.undef ) then
          qo(io2,jo2) = 0.75*qz(ii2,ji2) + 0.25*qz(ii4,ji4)
          qo(io4,jo4) = 0.25*qz(ii2,ji2) + 0.75*qz(ii4,ji4)
      endif

! Case (k)
! --------
      if( qz(ii1,ji1).ne.undef   .and. &
          qz(ii2,ji2).ne.undef   .and. &
          qz(ii3,ji3).ne.undef   .and. &
          qz(ii4,ji4).eq.undef ) then
          qo(io1,jo1) = 0.75*qz(ii1,ji1) + 0.125*(qz(ii2,ji2)+qz(ii3,ji3))
          qo(io3,jo3) = 0.75*qz(ii3,ji3) + 0.125*(qz(ii2,ji2)+qz(ii1,ji1))
          qo(io2,jo2) = 0.50*qz(ii2,ji2) + 0.250*(qz(ii1,ji1)+qz(ii3,ji3))
      endif

! Case (L)
! --------
      if( qz(ii1,ji1).eq.undef   .and. &
          qz(ii2,ji2).ne.undef   .and. &
          qz(ii3,ji3).ne.undef   .and. &
          qz(ii4,ji4).ne.undef ) then
          qo(io2,jo2) = 0.75*qz(ii2,ji2) + 0.125*(qz(ii4,ji4)+qz(ii3,ji3))
          qo(io4,jo4) = 0.75*qz(ii4,ji4) + 0.125*(qz(ii2,ji2)+qz(ii3,ji3))
          qo(io3,jo3) = 0.50*qz(ii3,ji3) + 0.250*(qz(ii2,ji2)+qz(ii4,ji4))
      endif

! Case (m)
! --------
      if( qz(ii1,ji1).ne.undef   .and. &
          qz(ii2,ji2).eq.undef   .and. &
          qz(ii3,ji3).ne.undef   .and. &
          qz(ii4,ji4).ne.undef ) then
          qo(io1,jo1) = 0.75*qz(ii1,ji1) + 0.125*(qz(ii4,ji4)+qz(ii3,ji3))
          qo(io3,jo3) = 0.75*qz(ii3,ji3) + 0.125*(qz(ii1,ji1)+qz(ii4,ji4))
          qo(io4,jo4) = 0.50*qz(ii4,ji4) + 0.250*(qz(ii1,ji1)+qz(ii3,ji3))
      endif

! Case (n)
! --------
      if( qz(ii1,ji1).ne.undef   .and. &
          qz(ii2,ji2).ne.undef   .and. &
          qz(ii3,ji3).eq.undef   .and. &
          qz(ii4,ji4).ne.undef ) then
          qo(io4,jo4) = 0.75*qz(ii4,ji4) + 0.125*(qz(ii1,ji1)+qz(ii2,ji2))
          qo(io2,jo2) = 0.75*qz(ii2,ji2) + 0.125*(qz(ii1,ji1)+qz(ii4,ji4))
          qo(io1,jo1) = 0.50*qz(ii1,ji1) + 0.250*(qz(ii2,ji2)+qz(ii4,ji4))
      endif

! Case (o)
! --------
      if( qz(ii1,ji1).ne.undef   .and. &
          qz(ii2,ji2).ne.undef   .and. &
          qz(ii3,ji3).ne.undef   .and. &
          qz(ii4,ji4).ne.undef ) then
          qo(io1,jo1) = ( 9*qz(ii1,ji1) + 3*qz(ii2,ji2) + 3*qz(ii4,ji4) + 1*qz(ii3,ji3) )/16.0
          qo(io2,jo2) = ( 9*qz(ii2,ji2) + 3*qz(ii1,ji1) + 3*qz(ii3,ji3) + 1*qz(ii4,ji4) )/16.0
          qo(io3,jo3) = ( 9*qz(ii3,ji3) + 3*qz(ii2,ji2) + 3*qz(ii4,ji4) + 1*qz(ii1,ji1) )/16.0
          qo(io4,jo4) = ( 9*qz(ii4,ji4) + 3*qz(ii1,ji1) + 3*qz(ii3,ji3) + 1*qz(ii2,ji2) )/16.0
      endif

      enddo
      enddo

      return
      end SUBROUTINE interp_to_eight_deg
