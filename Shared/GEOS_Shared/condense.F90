
subroutine condensation(T,P,Qv,Ql,SuperSat)

  implicit none

  real, intent(INOUT) :: T (:,:,:) ! Temperature in Kelvin
  real, intent(IN   ) :: P (:,:,:) ! pressure in Pascals
  real, intent(INOUT) :: Qv(:,:,:) ! specific humidity
  real, intent(INOUT) :: Ql(:,:,:) ! liquid water
  real, intent(IN)    :: SuperSat

  integer :: IM,JM,LM, I,J,L
  
  IM=size(T,1)
  JM=size(T,2)
  LM=size(T,3)

  do I=1,IM
     do J=1,JM
        LEVELS: do L=1,LM
           if(P(i,j,l)>5000.0) then ! DONT BOTHER ABOVE 50 MB
              call Iterate(T(i,j,l),p(i,j,l),qv(i,j,l),ql(i,j,l),SuperSat)
           end if
        end do LEVELS
     end do
  end do

  return

contains

  subroutine Iterate(TT,PP,QQ,QC,SuperSat)

    use MAPL_Mod, only : QSAT=>MAPL_EQSAT, MAPL_ALHL, MAPL_CP, MAPL_AIRMW, MAPL_RUNIV, MAPL_RDRY, MAPL_RGAS, MAPL_CP

    implicit none

    real, intent(INOUT) :: TT,QQ,QC
    real, intent(IN)    :: PP,SuperSat

   !real, parameter :: MAPL_ALHL   = 2.4665E6               ! J/kg @15C
   !real, parameter :: MAPL_AIRMW  = 28.965                 ! kg/Kmole
   !real, parameter :: MAPL_RUNIV  = 8314.47                ! J/(Kmole K)
   !real, parameter :: MAPL_RDRY   = MAPL_RUNIV/MAPL_AIRMW  ! J/(kg K)
   !real, parameter :: MAPL_RGAS   = MAPL_RDRY              ! J/(kg K) (DEPRECATED)
   !real, parameter :: MAPL_CP     = MAPL_RGAS/(2./7.)      ! J/(kg K) (DEPRECATED)

    real, parameter :: LbyCP=(MAPL_ALHL/MAPL_CP)

    real :: QST, GAM, DQ

    QST = Qsat(TT,PP,GAM)
    DQ  = SuperSat*QST - QQ

    if(DQ > 0.0) then
       do
          DQ = DQ/(1.0 + GAM*LbyCP)
          TT = TT - DQ*LbyCP
          QQ = QQ + DQ
          QC = QC - DQ
          QST   = Qsat(TT,PP,GAM)
          DQ    = SuperSat*QST-QQ
          if(abs(DQ/QST)<0.0001) exit
       end do
    end if

    return
  end subroutine Iterate

end subroutine condensation

