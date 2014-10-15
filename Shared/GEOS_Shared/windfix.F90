      subroutine windfix ( ua,va,plea, &
                           ub,vb,pleb,im,jm,lm,lattice,grid,method )
      use G3_MPI_Util_Mod
      implicit none
      type ( dynamics_lattice_type ) lattice

      integer    im,jm,lm,method
      real    ua(im,jm,lm)
      real    va(im,jm,lm)
      real  plea(im,jm,lm+1)
      real    ub(im,jm,lm)
      real    vb(im,jm,lm)
      real  pleb(im,jm,lm+1)

      real     u(im,jm,lm)
      real     v(im,jm,lm)
      real   dpa(im,jm,lm)
      real   dpb(im,jm,lm)
      real  diva(im,jm,lm)
      real  divb(im,jm,lm)

      character(*) grid

      integer index(lm),ierror

      real, allocatable ::  uglo(:,:,:)
      real, allocatable ::  vglo(:,:,:)
      real, allocatable ::  dglo(:,:,:)
      real, allocatable :: dpglo(:,:,:)

      real, allocatable ::   sum1(:,:)
      real, allocatable ::   sum2(:,:)
      real, allocatable ::   sum3(:,:)
      real, allocatable :: lambda(:,:)
      real, allocatable ::    chi(:,:)
      real, allocatable ::   uchi(:,:)
      real, allocatable ::   vchi(:,:)
      real, allocatable :: dumdiv(:,:)
      real, allocatable :: dumchi(:,:)

      integer L, comm, myid, npes
      integer img, jmg
      integer imax,jmax,msgn
      real    undef

      imax  = 576  ! Maximum IMG size for Laplacian Solver
      jmax  = 361  ! Maximum JMG size for Laplacian Solver
      msgn  = 0    ! Scalar  Flag
      undef = 1e15

      img  = lattice%imglobal
      jmg  = lattice%jmglobal
      comm = lattice%comm
      myid = lattice%myid
      npes = lattice%nx * lattice%ny

      do L=1,lm
      index(L) = mod(L-1,npes)
      enddo
 
      allocate (  uglo(img,jmg,lm) )
      allocate (  vglo(img,jmg,lm) )
      allocate (  dglo(img,jmg,lm) )
      allocate ( dpglo(img,jmg,lm) )
      allocate (   chi(img,jmg)    )
      allocate (  uchi(img,jmg)    )
      allocate (  vchi(img,jmg)    )

      allocate (   sum1(im,jm)    )
      allocate (   sum2(im,jm)    )
      allocate (   sum3(im,jm)    )
      allocate ( lambda(im,jm)    )

! Compute Pressure Thickness
! --------------------------
      do L=1,lm
        dpa(:,:,L) = ( plea(:,:,L+1)-plea(:,:,L) )
        dpb(:,:,L) = ( pleb(:,:,L+1)-pleb(:,:,L) )
      enddo

! Gather Winds for Background
! ---------------------------
      call G3_GATHER (  uglo, ub,lattice )
      call G3_GATHER (  vglo, vb,lattice )
      call G3_GATHER ( dpglo,dpb,lattice )

      call mpi_bcast (  uglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
      call mpi_bcast (  vglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
      call mpi_bcast ( dpglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )

! Compute Vorticity and Divergence
! --------------------------------
      do L=1,lm
      if( index(L).eq.myid ) call getdiv (uglo(1,1,L),vglo(1,1,L),dpglo(1,1,L),dglo(1,1,L),img,jmg )               
      enddo
      call mpi_barrier (comm,ierror)
      do L=1,lm
      call mpi_bcast ( dglo(1,1,L),img*jmg,lattice%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)
      call G3_SCATTER  (dglo,divb,lattice)

! Gather Winds for Analysis
! -------------------------
      call G3_GATHER (  uglo, ua,lattice )
      call G3_GATHER (  vglo, va,lattice )
      call G3_GATHER ( dpglo,dpa,lattice )

      call mpi_bcast (  uglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
      call mpi_bcast (  vglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
      call mpi_bcast ( dpglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )

! Compute Vorticity and Divergence
! --------------------------------
      do L=1,lm
      if( index(L).eq.myid ) call getdiv (uglo(1,1,L),vglo(1,1,L),dpglo(1,1,L),dglo(1,1,L),img,jmg )               
      enddo
      call mpi_barrier (comm,ierror)
      do L=1,lm
      call mpi_bcast ( dglo(1,1,L),img*jmg,lattice%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)
      call G3_SCATTER  (dglo,diva,lattice)


! Compute Divergence Increment (to force vanishing vertical integral)
! -------------------------------------------------------------------

! Minimize Relative Change
! ------------------------
      if( method.eq.1 ) then
          sum1(:,:) = 0.0
          sum2(:,:) = 0.0
          sum3(:,:) = 0.0
          do L=1,lm
          sum1(:,:) = sum1(:,:) +  diva(:,:,L)
          sum2(:,:) = sum2(:,:) +  divb(:,:,L)
          sum3(:,:) = sum3(:,:) + (diva(:,:,L)-divb(:,:,L))**2
          enddo
          where( sum3 .ne. 0.0 )
                 lambda = (sum1-sum2) / sum3
          elsewhere
                 lambda = 0.0
          endwhere
          do L=1,lm
          diva(:,:,L) = -lambda(:,:)*( diva(:,:,L)-divb(:,:,L) )**2
          enddo
          sum3(:,:) = sum1(:,:)
          do L=1,lm
          sum3(:,:) = sum3(:,:) +  diva(:,:,L)
          enddo
      endif

! Minimize Absolute Change
! ------------------------
      if( method.eq.2 ) then
          sum1(:,:) = 0.0
          sum2(:,:) = 0.0
          sum3(:,:) = 0.0
          do L=1,lm
          sum1(:,:) = sum1(:,:) +  diva(:,:,L)
          sum2(:,:) = sum2(:,:) +  divb(:,:,L)
          sum3(:,:) = sum3(:,:) +   dpa(:,:,L)**2
          enddo
          lambda = (sum1-sum2) / sum3
          do L=1,lm
          diva(:,:,L) = -lambda(:,:)*dpa(:,:,L)**2
          enddo
          sum3(:,:) = sum1(:,:)
          do L=1,lm
          sum3(:,:) = sum3(:,:) +  diva(:,:,L)
          enddo
      endif

!     call writit ( sum1,im,jm,1,55,lattice )
!     call writit ( sum2,im,jm,1,55,lattice )
!     call writit ( sum3,im,jm,1,55,lattice )

! Gather and Broadcast Divergence Increment
! -----------------------------------------
      call G3_GATHER ( dglo,diva,lattice )
      call mpi_bcast ( dglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )

! Compute Wind Increments Associated with Divergence Increment
! ------------------------------------------------------------
      do L=1,lm
      if( index(L).eq.myid ) then

          if( img.gt.imax .or. jmg.gt.jmax ) then
              allocate( dumdiv(imax,jmax) )
              allocate( dumchi(imax,jmax) )
              call regrid ( dglo(1,1,L),img,jmg,dumdiv,imax,jmax,undef,msgn )
              call VELPOT (dumdiv,dumchi,imax,jmax)
              call regrid ( dumchi,imax,jmax,chi,img,jmg,undef,msgn )
              deallocate( dumdiv )
              deallocate( dumchi )
          else
              call VELPOT (dglo(1,1,L),chi,img,jmg)
          endif

          call gradq  (chi,  uchi,vchi,img,jmg)
          uglo(:,:,L) = uglo(:,:,L) + uchi(:,:)/dpglo(:,:,L)
          vglo(:,:,L) = vglo(:,:,L) + vchi(:,:)/dpglo(:,:,L)
      endif
      enddo
      call mpi_barrier (comm,ierror)
      do L=1,lm
      call mpi_bcast ( uglo(1,1,L),img*jmg,lattice%mpi_rkind,index(L),comm,ierror )
      call mpi_bcast ( vglo(1,1,L),img*jmg,lattice%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)

! Scatter Winds
! -------------
      call G3_SCATTER ( uglo,ua,lattice )
      call G3_SCATTER ( vglo,va,lattice )

      deallocate ( sum1,sum2,sum3,lambda )
      deallocate ( chi,uchi,vchi         )
      deallocate ( uglo,vglo,dglo,dpglo  )
      return
      end

      SUBROUTINE GETDIV ( U,V,DP,DIV,IM,JM )
! ********************************************************************          
! ****                                                            ****          
! ****  THIS PROGRAM CALCULATES DIVERGENCE                        ****          
! ****  AT EACH LEVEL FOR A NON-STAGGERED A-GRID                  ****          
! ****                                                            ****          
! ****  INPUT:                                                    ****          
! ****    U ....... ZONAL WIND                                    ****          
! ****    V ....... MERIDIONAL WIND                               ****          
! ****    IM ...... NUMBER OF LONGITUDE POINTS                    ****          
! ****    JM ...... NUMBER OF LATITUDE  POINTS                    ****          
! ****                                                            ****          
! ****  OUTPUT:                                                   ****          
! ****    DIV  (IM,JM) .... DIVERGENCE                            ****          
! ****                                                            ****          
! ********************************************************************          
                                                                                
      real      U(IM,JM)                                               
      real      V(IM,JM)                                               
      real     DP(IM,JM)                                               
      real    DIV(IM,JM)                                               
                                                                                
      real   P1X (IM,JM)                                               
      real   P1Y (IM,JM)                                               
      real   TMP1(IM,JM)                                               
      real   TMP2(IM,JM)                                               
      real  cosij(IM,JM)                                               
                                                                                
      DIMENSION MSGN(2)                                                         
                                                                                
      DATA MSGN /-1,1/                                                          
                                                                                
! *********************************************************                     
! ****          INITIALIZATION FOR DIVERGENCE          ****                     
! *********************************************************                     
                                                                                
      A    = 6.372e6
      pi   = 4.*atan(1.)
      dlon = 2*pi/ im
      dlat =   pi/(jm-1)
                                                                                
      C11 =  1.0 / (4.0*A*IM*(1.0-COS(0.5*dlat)))                           

      CX1 =  1.0 / (4.0*A*dlon)                                             
      CY1 =  1.0 / (4.0*A*dlat)                                             
                                                                                
      do j=2,jm-1
         phi = -pi/2.+(j-1)*dlat
      cosphi = cos(phi)
      do i=1,im
      cosij(i,j) = cosphi
      enddo
      enddo
      cosij(:,1)  = 0.0
      cosij(:,jm) = 0.0

! ********************************************************                     
! ****          CALCULATE AVERAGE QUANTITIES           ****                     
! *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
         i  =im
      DO ip1=1,im
         P1X(i,j) = ( U(ip1,j)+U(i,j) )*( DP(ip1,j)+DP(i,j) )
         i  =ip1
      ENDDO                                                                     
      ENDDO                                                                     

      DO j=1,jm-1                                                           
      DO I=1,im
         P1Y(I,j) = ( V(I,J+1)*COSIJ(I,J+1)+V(I,j)*COSIJ(I,j) )*( DP(I,J+1)+DP(I,J) )
      ENDDO                                                                     
      ENDDO                                                                     
                                                                                
! *********************************************************                     
! ****        CALCULATE HORIZONTAL DIVERGENCE          ****                     
! *********************************************************                     
                                                                                
      DO j=2,jm-1                                                           
          im1=im
         DO i=1,im
         TMP1(i,j) = ( P1X(i,j)-P1X(im1,j) )*CX1                               
          im1=i
         ENDDO                                                                     

         DO   I=1,im
         TMP2(I,j) = ( P1Y(I,j)  -P1Y(I,j-1) )*CY1                                  
         DIV (I,j) = ( TMP1(I,j)+TMP2(I,j) )/(cosij(i,j))                        
         ENDDO                                                                     
      ENDDO                                                                     
                                                                                
! *********************************************************                     
! ****     CALCULATE HORIZONTAL DIVERGENCE AT POLES    ****                     
! *********************************************************                     
                                                                                
      DO 100 M=1,2                                                              
      JPOLE  = 1 + (M-1)*(jm-1)                                                
      JPH    = 1 + (M-1)*(jm-2)                                               

      SUM11  = 0.0
      DO    I=1,im
      SUM11  = SUM11 + P1Y(I,JPH)
      ENDDO                                                                     
                                                                                
      DO  I=1,im
      DIV(I,JPOLE) = - MSGN(M) * C11*SUM11
      ENDDO                                                                     
  100 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GRADQ (Q,DQDX,DQDY,IM,JM)
! *********************************************************
! ****                                                 ****
! ****  THIS PROGRAM CALCULATES THE HORIZONTAL         ****
! ****  GRADIENT OF THE INPUT FIELD Q                  ****
! ****                                                 ****
! ****  ARGUMENTS:                                     ****
! ****    Q ....... FIELD TO BE DIFFERENTIATED         ****
! ****    DQDX .... LONGITUDINAL Q-DERIVATIVE          ****
! ****    DQDY .... MERIDIONAL   Q-DERIVATIVE          ****
! ****    IM ...... NUMBER OF LONGITUDINAL POINTS      ****
! ****    JM ...... NUMBER OF LATITUDINAL  POINTS      ****
! ****                                                 ****
! *********************************************************

      implicit none
      integer  im,jm

      real    Q(IM,JM)
      real DQDX(IM,JM)
      real DQDY(IM,JM)
      real  Q1X(IM,JM)
      real  Q2X(IM,JM)
      real  Q1Y(IM,JM)
      real  Q2Y(IM,JM)
      real acos(JM)
      real sinl(IM)
      real cosl(IM)

      real cx1,cx2,cy1,cy2,uc,vc,us,vs
      real dl,dp,a,pi,fjeq,phi
      integer i,j,m,ip1,ip2,jpole,msgn

! *********************************************************
! ****               INITIALIZATION                    ****
! *********************************************************

      a  = 6376.0E3
      pi = 4.0*atan(1.0)
      dl = 2.0*pi/im
      dp = pi/(jm-1)

      CX1 = 2.0 / ( 3.0*A*DL)
      CX2 = 1.0 / (12.0*A*DL)
      CY1 = 2.0 / ( 3.0*A*DP)
      CY2 = 1.0 / (12.0*A*DP)

      Q1X(:,:) = 0.0
      Q2X(:,:) = 0.0
      Q1Y(:,:) = 0.0
      Q2Y(:,:) = 0.0

      fjeq = ( jm+1 )*0.5
      do j=2,jm-1
          phi = dp * (j-fjeq)
      acos(j) = 1.0/( cos(phi) )
      enddo
      do i=1,im/2
      cosl(i)      = -cos((i-1)*dl)
      cosl(i+im/2) = -cosl(i)
      sinl(i)      = -sin((i-1)*dl)
      sinl(i+im/2) = -sinl(i)
      enddo

! *********************************************************
! ****          CALCULATE AVERAGE QUANTITIES           ****
! *********************************************************

      do j   = 2,jm-1
         i   = im-1
         ip1 = im
      do ip2 = 1,im
      Q1X(i  ,j) = Q(ip1,j) + Q(i,j)
      Q2X(ip1,j) = Q(ip2,j) + Q(i,j)
         i   = ip1
         ip1 = ip2
      enddo
      enddo

      do j=1,jm-1
      do i=1,im
      Q1Y(i,j) = Q(i,j+1) + Q(i,j)
      enddo
      enddo

      do j=2,jm-1
      do i=1,im
      Q2Y(i,j) = Q(i,j+1) + Q(i,j-1)
      enddo
      enddo

      do i=1,im/2
      Q2Y(i, 1) = Q(i,2)
      Q2Y(i,jm) = Q(i,jm-1)
      enddo

      do i=1,im/2
      Q2Y(i     , 1) = Q(i+im/2,2)    + Q2Y(i,1)
      Q2Y(i+im/2, 1) = Q2Y(i,1)
      Q2Y(i     ,jm) = Q(i+im/2,jm-1) + Q2Y(i,jm)
      Q2Y(i+im/2,jm) = Q2Y(i,jm)
      enddo

! *********************************************************
! ****             CALCULATE Q-GRADIENTS               ****
! *********************************************************

      do j   = 2,jm-1
         i   = im-1
         ip1 = im
      do ip2 = 1,im
      DQDX(ip1,j) =  ACOS(j) * ( ( Q1X(ip1,j)-Q1X(i,j) )*CX1 &
                               - ( Q2X(ip2,j)-Q2X(i,j) )*CX2 )
         i   = ip1
         ip1 = ip2
      enddo
      enddo

      do j=2,jm-1
      do i=1,im
      DQDY(i,j) = ( Q1Y(i,j)  -Q1Y(i,j-1) )*CY1 &
                - ( Q2Y(i,j+1)-Q2Y(i,j-1) )*CY2
      enddo
      enddo

! *********************************************************
! ****         CALCULATE Q-GRADIENTS (POLES)           ****
! *********************************************************

      do i=1,im/2
      Q1Y(i,      2) = Q(i,      1) + Q(i+im/2,2)
      Q1Y(i+im/2, 2) = Q(i+im/2, 1) + Q(i,     2)
      Q2Y(i,      1) = Q(i,      1) + Q(i+im/2,3)
      Q2Y(i+im/2, 1) = Q(i+im/2, 1) + Q(i,     3)

      Q1Y(i,     jm) = Q(i,     jm) + Q(i+im/2,jm-1)
      Q1Y(i+im/2,jm) = Q(i+im/2,jm) + Q(i,     jm-1)
      Q2Y(i,     jm) = Q(i,     jm) + Q(i+im/2,jm-2)
      Q2Y(i+im/2,jm) = Q(i+im/2,jm) + Q(i,     jm-2)
      enddo

      do i=1,im
      DQDY(i,jm) = ( Q1Y(i,jm)-Q1Y(i,jm-1) )*CY1 &
                 - ( Q2Y(i,jm)-Q2Y(i,jm-1) )*CY2

      DQDY(i, 1) = ( Q1Y(i,1)-Q1Y(i,2) )*CY1 &
                 - ( Q2Y(i,2)-Q2Y(i,1) )*CY2
      enddo

!   APPLY BOUNDARY CONDITIONS AT THE POLES
! ==========================================

      DO 170 M=1,2
      MSGN  = (-1)**M
      JPOLE = 1 + (M-1)*(jm - 1)

      VC = 0.0
      VS = 0.0
      DO 180 I=1,IM
      VC = VC + DQDY(I,JPOLE)*COSL(I)
      VS = VS + DQDY(I,JPOLE)*SINL(I)
  180 CONTINUE
      VC = 2.0 * VC / IM
      VS = 2.0 * VS / IM

      UC = - MSGN*VS
      US =   MSGN*VC

      DO 190 I=1,IM
      DQDX(I,JPOLE) = US*SINL(I) + UC*COSL(I)
      DQDY(I,JPOLE) = VS*SINL(I) + VC*COSL(I)
  190 CONTINUE

  170 CONTINUE

      RETURN
      END
      SUBROUTINE VELPOT (DIV,VELP,im,jnp)

      integer        IM,JNP
      real       DIV(IM,JNP)
      real      VELP(IM,JNP)

      real*8, allocatable :: VP(:,:)
      real*8, allocatable ::  w(:)
      real*8, allocatable ::  bdtf(:)
      real*8, allocatable ::  bdts(:)
      real*8, allocatable ::  bdps(:)
      real*8, allocatable ::  bdpf(:)
      real*8  ts,tf,ps,pf,elmbda,pertrb,pi

      imp = im+1
      iwk = 11*jnp+6*imp

      allocate ( vp(jnp,imp) )
      allocate (    w(iwk)   )
      allocate ( bdtf(imp)   )
      allocate ( bdts(imp)   )
      allocate ( bdps(jnp)   )
      allocate ( bdpf(jnp)   )
 
      vp(:,:)=0.0
         w(:)=0.0
      bdtf(:)=0.0
      bdts(:)=0.0
      bdps(:)=0.0
      bdpf(:)=0.0
 
! Transpose the input array
! -------------------------
      do j=1,jnp
      do i=1,im
      vp(j,i)   = div(i,j)
      enddo
      vp(j,imp) =  vp(j,1)
      enddo

! === SET THE INPUT VARIABLES
      RAD = 6371000.0
      PI  = 3.14159265358979D0
      INTL=0
      TS=0.0
      TF=PI
      M=JNP-1
      MBDCND=9
      PS=0.0
      PF=2*PI
      N=IM
      NBDCND=0
      ELMBDA=0
      PERTRB=0
      IDIMF=M+1
 
      CALL PWSSSP (INTL,TS,TF,M,MBDCND,BDTS,BDTF,PS,PF,N,NBDCND,BDPS, &
                   BDPF,ELMBDA,VP,IDIMF,PERTRB,IERROR,W)

      if( ierror.ne.0 ) then
          print *, 'PWSSSP IERROR = ',ierror
          stop
      endif
 
! Scale by earth radius
! ---------------------
      do j=1,jnp
      do i=1,im
      VELP(I,J) = VP(J,I) * RAD * RAD
      enddo
      enddo
 
! Remove global mean
! ------------------
      CALL ZEROG (VELP,IM,JNP)
 
      deallocate (   vp )
      deallocate (    w )
      deallocate ( bdtf )
      deallocate ( bdts )
      deallocate ( bdps )
      deallocate ( bdpf )
      RETURN
      END

      SUBROUTINE ZEROG (VEL,IM,JNP)
      integer    IM,JNP
      real   VEL(IM,JNP)

      pi  = 4.0*atan(1.0)
      dl  = 2*pi/im
      dp  = pi/(jnp-1)
      cap = 1-cos(0.5*dp)

! Ensure unique pole values
! -------------------------
      sum1 = 0.0
      sum2 = 0.0
      do i=1,im
      sum1 = sum1 + vel(i,1)
      sum2 = sum2 + vel(i,jnp)
      enddo
      sum1 = sum1/im
      sum2 = sum2/im
      do i=1,im
      vel(i,1)   = sum1
      vel(i,jnp) = sum2
      enddo

! Compute global average
! ----------------------
      sum1 = 0.0
      sum2 = 0.0
      do i=1,im
      sum1 = sum1 + cap*vel(i,1)
      sum2 = sum2 + cap
      enddo

      do j=2,jnp-1
      cosj = cos( -pi/2 + (j-1)*dp )
      do i=1,im
      sum1 = sum1 + cosj*dp*vel(i,j)
      sum2 = sum2 + cosj*dp
      enddo
      enddo

      do i=1,im
      sum1 = sum1 + cap*vel(i,jnp)
      sum2 = sum2 + cap
      enddo

      qave = sum1/sum2

      do j=1,jnp
      do i=1,im
      vel(i,j) = vel(i,j)-qave
      enddo
      enddo

!     print *, 'Remove Global Average: ', qave

      RETURN
      END
 
      subroutine writit ( q,im,jm,lm,ku,lattice )
      use G3_MPI_Util_Mod
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer  im,jm,lm,L,ku,img,jmg
      real   q(im,jm,lm)
      real,   allocatable :: glo(:,:)
      real*4, allocatable ::   a(:,:)
      img = lattice%imglobal
      jmg = lattice%jmglobal
      allocate ( glo(img,jmg) )
      allocate (   a(img,jmg) )
      do L=1,lm
         call G3_GATHER ( glo,q(:,:,L),lattice )
         if( lattice%myid.eq.0 ) then
                       a = glo
             write(ku) a
         endif
      enddo
      deallocate ( glo )
      deallocate ( a   )
      return
      end

      subroutine regrid ( qin,im_in,jm_in,qout,im_out,jm_out,undef,msgn )
      implicit  none
      integer   im_in ,jm_in ,msgn
      integer   im_out,jm_out
      real      undef
      real    qin(im_in ,jm_in )
      real   qout(im_out,jm_out)
      real q10x10(360*6,180*6)
 
! Parse Arbitray Field (im,jm) to 10'x10' Variable
! ------------------------------------------------
      call bin_10x10 ( qin,im_in,jm_in,q10x10 )
 
! Bin 10'x10' Variable to Output Field (im_out,jm_out)
! ----------------------------------------------------
      call averaged_10x10 ( q10x10,qout,im_out,jm_out,undef,msgn )

      return
      end

      subroutine averaged_10x10 ( z10x10,z,im,jm,undef,msgn )
!***********************************************************************
!
!  PURPOSE:
!  ========
!    Average a (10m X 10m) input array to an output array (im,jm)
!
!  INPUT:
!  ======
!    z10x10 ..... Input array(360*6,180*6)
!    msgn ....... Integer Flag for scalar (0) or vector (1)
!
!  OUTPUT:
!  =======
!    z .......... Output array(im,jm)
!    im ......... Longitudinal dimension of z
!    jm ......... Latitudinal  dimension of z
!
!  NOTES:
!  ======
!    Input array z10x10  represents values within a 10min X 10min grid-box.
!             Each box is referenced by the latitude and longitude of
!             its southwest corner, not its center point.  Thus,
!             the height associated with a coordinate actually
!             represents the heights centered to the northeast of that point.
!
!    Output array z(im,jm) is assumed to be on an A-grid.
!                 z(i,j)   represents the value at the center of the grid-box.
!                 z(1,j)   is located at lon=-180.
!                 z(i,1)   is located at lat=-90.
!                 z(i,jm)  is located at lat=+90.
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      integer im,jm,msgn
      real  z(im,jm)
      real  dlam(im), dphi(jm)
      real  z10x10(360*6,180*6)

      integer i,j,ibeg,iend,jbeg,jend
      integer ii,jj,itmp
      real    sum1,sum2
      real    zlat,zlon
      real    lon1,lon2,wx
      real    lat1,lat2,wy
      real    lonbeg,lonend,lat,coslat
      real    latbeg,latend
      real    undef 
      real    pi,dz 
      real    lon_cmp(im)
      real    lat_cmp(jm)
      logical defined

      pi   = 4.*atan(1.)
      dz   = pi/(6.*180)
      dlam = 2*pi/ im
      dphi =   pi/(jm-1)

! Compute Computational Lambda's and Phi's
! ----------------------------------------
      lon_cmp(1) = -pi
      do i=2,im
      lon_cmp(i) = lon_cmp(i-1) + dlam(i-1)
      enddo
      lat_cmp(1) = -pi*0.5
      do j=2,jm-1
      lat_cmp(j) = lat_cmp(j-1) + dphi(j-1)
      enddo
      lat_cmp(jm) =  pi*0.5


! Compute average away from poles
! -------------------------------
      do j=2,jm-1
      do i=1,im

      zlat = lat_cmp(j)
      zlon = lon_cmp(i)

      latbeg = zlat-dphi(j-1)/2
      latend = zlat+dphi(j)  /2
      if( i.eq.1 ) then
      lonbeg = zlon-dlam(im) /2
      else
      lonbeg = zlon-dlam(i-1)/2
      endif
      lonend = zlon+dlam(i)  /2
      
      ibeg = 1.+(lonbeg+pi)  /dz
      iend = 1.+(lonend+pi)  /dz
      jbeg = 1.+(latbeg+pi/2)/dz
      jend = 1.+(latend+pi/2)/dz

      sum1 = 0
      sum2 = 0
      do jj=jbeg,jend
      lat = -pi/2+(jj-0.5)*dz
      coslat = cos(lat)
      lat1 = -pi/2  + (jj-1)*dz
      lat2 = -pi/2  +  jj   *dz
                           wy = 1.0
      if( lat1.lt.latbeg ) wy = (lat2-latbeg)/dz
      if( lat2.gt.latend ) wy = (latend-lat1)/dz

         if(ibeg.ge.1) then
           do ii=ibeg,iend
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         else
                 itmp = 1.+(lonbeg+0.1*dz+3*pi)/dz
           do ii=itmp,360*6
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                     wx = 1.0
           if( lon1.lt.lonbeg+2*pi ) wx = (lon2-lonbeg-2*pi)/dz
           if( lon2.gt.lonend+2*pi ) wx = (2*pi+lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
           do ii=1,iend
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         endif

      enddo
      if( sum2.ne.0.0 ) then
          z(i,j) = sum1/sum2
      else
          z(i,j) = undef
      endif
      enddo
      enddo

! Compute average at South Pole
! -----------------------------
         j=1
      do i=1,im

      zlat = lat_cmp(j)
      zlon = lon_cmp(i)

      latbeg = zlat
      latend = zlat+dphi(j)  /2
      if( i.eq.1 ) then
      lonbeg = zlon-dlam(im) /2
      else
      lonbeg = zlon-dlam(i-1)/2
      endif
      lonend = zlon+dlam(i)  /2
      
      ibeg = 1.+(lonbeg+pi)  /dz
      iend = 1.+(lonend+pi)  /dz
      jbeg = 1
      jend = 1.+(latend+pi/2)/dz

      sum1 = 0
      sum2 = 0
      do jj=jbeg,jend
      lat = -pi/2+(jj-0.5)*dz
      coslat = cos(lat)
      lat1 = -pi/2  + (jj-1)*dz
      lat2 = -pi/2  +  jj   *dz
                           wy = 1.0
      if( lat1.lt.latbeg ) wy = (lat2-latbeg)/dz
      if( lat2.gt.latend ) wy = (latend-lat1)/dz

         if(ibeg.ge.1) then
           do ii=ibeg,iend
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         else
                 itmp = 1.+(lonbeg+0.1*dz+3*pi)/dz
           do ii=itmp,360*6
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                     wx = 1.0
           if( lon1.lt.lonbeg+2*pi ) wx = (lon2-lonbeg-2*pi)/dz
           if( lon2.gt.lonend+2*pi ) wx = (2*pi+lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
           do ii=1,iend
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         endif

      enddo
      if( sum2.ne.0.0 ) then
          z(i,j) = sum1/sum2
      else
          z(i,j) = undef
      endif
      enddo

! Compute average at North Pole
! -----------------------------
         j=jm
      do i=1,im

      zlat = lat_cmp(j)
      zlon = lon_cmp(i)

      latbeg = zlat-dphi(j-1)/2
      latend = zlat
      if( i.eq.1 ) then
      lonbeg = zlon-dlam(im) /2
      else
      lonbeg = zlon-dlam(i-1)/2
      endif
      lonend = zlon+dlam(i)  /2
      
      ibeg = 1.+(lonbeg+pi)  /dz
      iend = 1.+(lonend+pi)  /dz
      jbeg = 1.+(latbeg+pi/2)/dz
      jend = 1080

      sum1 = 0
      sum2 = 0
      do jj=jbeg,jend
      lat = -pi/2+(jj-0.5)*dz
      coslat = cos(lat)
      lat1 = -pi/2  + (jj-1)*dz
      lat2 = -pi/2  +  jj   *dz
                           wy = 1.0
      if( lat1.lt.latbeg ) wy = (lat2-latbeg)/dz
      if( lat2.gt.latend ) wy = (latend-lat1)/dz

         if(ibeg.ge.1) then
           do ii=ibeg,iend
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         else
                 itmp = 1.+(lonbeg+0.1*dz+3*pi)/dz
           do ii=itmp,360*6
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                     wx = 1.0
           if( lon1.lt.lonbeg+2*pi ) wx = (lon2-lonbeg-2*pi)/dz
           if( lon2.gt.lonend+2*pi ) wx = (2*pi+lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
           do ii=1,iend
           if( defined(z10x10(ii,jj),undef) ) then
           lon1 = -pi  + (ii-1)*dz
           lon2 = -pi  +  ii   *dz
                                wx = 1.0
           if( lon1.lt.lonbeg ) wx = (lon2-lonbeg)/dz
           if( lon2.gt.lonend ) wx = (lonend-lon1)/dz
           sum1 = sum1 + z10x10(ii,jj)*coslat*wx*wy
           sum2 = sum2 +               coslat*wx*wy
           endif
           enddo
         endif

      enddo
      if( sum2.ne.0.0 ) then
          z(i,j) = sum1/sum2
      else
          z(i,j) = undef
      endif
      enddo

! Average Pole Values
! -------------------
      if( msgn.eq.0 ) then
      sum1 = 0
         j = 0
      do i=1,im
         if( defined(z(i,1),undef) ) then
             sum1 = sum1 + z(i,1)
                j = j + 1
         endif
      enddo
      if( j.ne.0 ) then
      z(:,1) = sum1/j
      else
      z(:,1) = undef
      endif

      sum2 = 0
         j = 0
      do i=1,im
         if( defined(z(i,jm),undef) ) then
             sum2 = sum2 + z(i,jm)
                j = j + 1
         endif
      enddo
      if( j.ne.0 ) then
      z(:,jm) = sum2/j
      else
      z(:,jm) = undef
      endif

      endif

      return
      end

      subroutine bin_10x10 ( z,im,jm,z10x10 )
!***********************************************************************
!
!  PURPOSE:
!  ========
!    Compute a (10m X 10m) array binned from an input array (im,jm)
!
!  INPUT:
!  ======
!    z .......... Input array(im,jm)
!    im ......... Longitudinal dimension of z
!    jm ......... Latitudinal  dimension of z
!
!  OUTPUT:
!  =======
!    z10x10 ..... Output array(360*6,180*6)
!
!  NOTES:
!  ======
!    Input array z(im,jm) is assumed to be on an A-grid.
!                z(i,j)   represents the value at the center of the grid-box.
!                z(1,j)   is located at lon=-180.
!                z(i,1)   is located at lat=-90.
!                z(i,jm)  is located at lat=+90.
!
!    Output array z10x10  represents values within a 10min X 10min grid-box.
!             Each box is referenced by the latitude and longitude of
!             its southwest corner, not its center point.  Thus,
!             the height associated with a coordinate actually
!             represents the heights centered to the northeast of that point.
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      integer im,jm
      real  z(im,jm)
      real z10x10(360*6,180*6)

      integer i,j,ii,jj,ibeg,iend,jbeg,jend
      real    zlatc,zlonc
      real    lonbeg,lonend,lat
      real    latbeg,latend
      real    pi,dl,dp,dz 

      pi = 4.*atan(1.)
      dl = 2*pi/im
      dp = pi/(jm-1)
      dz = pi/(6.*180)
      
      do j=1,180*6
      do i=1,360*6

      zlatc = -pi/2+(j-0.5)*dz  ! Latitude  at center of 10x10 box
      zlonc = -pi  +(i-0.5)*dz  ! Longitude at center of 10x10 box

! Find bounding lat and lon on IMxJM grid
! ---------------------------------------
      iend = nint( 1.+(zlonc+pi)/dl )
      lonend = -pi + (iend-1)*dl
      if( lonend.ge.zlonc ) then
      lonbeg = -pi + (iend-2)*dl
      else
      iend = iend+1
      lonbeg = lonend
      lonend = -pi + (iend-1)*dl
      endif
      ibeg = iend-1

      jend = nint( 1.+(zlatc+pi/2)/dp )
      latend = -pi/2 + (jend-1)*dp
      if( latend.ge.zlatc ) then
      latbeg = -pi/2 + (jend-2)*dp
      else
      jend = jend+1
      latbeg = latend
      latend = -pi/2 + (jend-1)*dp
      endif
      jbeg = jend-1


      if(iend.gt.im) iend=iend-im

      if( zlonc.le.lonbeg+0.5*dl ) then
      ii = ibeg
      else
      ii = iend
      endif
      if( zlatc.le.latbeg+0.5*dp ) then
      jj = jbeg
      else
      jj = jend
      endif

      z10x10(i,j) = z(ii,jj)
      
      enddo
      enddo

      return
      end

      function defined ( q,undef )
      implicit none
      logical  defined
      real     q,undef
      defined = abs(q-undef).gt.0.1*abs(undef)
      return
      end

