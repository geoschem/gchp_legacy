      subroutine windfix ( ua,va,plea,
     .                     ub,vb,pleb,im,jm,lm,lattice,grid,method )
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice

      integer    im,jm,lm,method
      real    ua(im,jm,lm)
      real    va(im,jm,lm)
      real  plea(im,jm,lm+1)
      real    ub(im,jm,lm)
      real    vb(im,jm,lm)
      real  pleb(im,jm,lm+1)

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

      integer L, comm, myid, npes
      integer img, jmg

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
      do L=1,lm
             call gather_2d (  uglo(1,1,L), ub(1,1,L),lattice )
             call gather_2d (  vglo(1,1,L), vb(1,1,L),lattice )
             call gather_2d ( dpglo(1,1,L),dpb(1,1,L),lattice )
         if( trim(grid).eq.'d'  .or. 
     .       trim(grid).eq.'D' ) then
         if( lattice%myid.eq.0 ) then
             call dtoa      ( uglo(1,1,L),uglo(1,1,L),img,jmg,1,2 )
             call dtoa      ( vglo(1,1,L),vglo(1,1,L),img,jmg,1,1 )
         endif
         endif
      enddo
#ifdef mpi
      call mpi_bcast (  uglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
      call mpi_bcast (  vglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
      call mpi_bcast ( dpglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
#endif

c Compute Vorticity and Divergence
c --------------------------------
      do L=1,lm
      if( index(L).eq.myid ) call getdiv (uglo(1,1,L),vglo(1,1,L),dpglo(1,1,L),dglo(1,1,L),img,jmg )               
      enddo
#ifdef mpi
      call mpi_barrier (comm,ierror)
      do L=1,lm
      call mpi_bcast ( dglo(1,1,L),img*jmg,lattice%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)
#endif
      do L=1,lm
      call scatter_2d ( dglo(1,1,L),divb(1,1,L),lattice )
      enddo

! Gather Winds for Analysis
! -------------------------
      do L=1,lm
             call gather_2d ( uglo(1,1,L), ua(1,1,L),lattice )
             call gather_2d ( vglo(1,1,L), va(1,1,L),lattice )
             call gather_2d (dpglo(1,1,L),dpa(1,1,L),lattice )
         if( trim(grid).eq.'d'  .or. 
     .       trim(grid).eq.'D' ) then
         if( lattice%myid.eq.0 ) then
             call dtoa      ( uglo(1,1,L),uglo(1,1,L),img,jmg,1,2 )
             call dtoa      ( vglo(1,1,L),vglo(1,1,L),img,jmg,1,1 )
         endif
         endif
      enddo
#ifdef mpi
      call mpi_bcast (  uglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
      call mpi_bcast (  vglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
      call mpi_bcast ( dpglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
#endif

c Compute Vorticity and Divergence
c --------------------------------
      do L=1,lm
      if( index(L).eq.myid ) call getdiv (uglo(1,1,L),vglo(1,1,L),dpglo(1,1,L),dglo(1,1,L),img,jmg )               
      enddo
#ifdef mpi
      call mpi_barrier (comm,ierror)
      do L=1,lm
      call mpi_bcast ( dglo(1,1,L),img*jmg,lattice%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)
#endif
      do L=1,lm
      call scatter_2d ( dglo(1,1,L),diva(1,1,L),lattice )
      enddo


c Compute Divergence Increment (to force vanishing vertical integral)
c -------------------------------------------------------------------

c Minimize Relative Change
c ------------------------
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

c Minimize Absolute Change
c ------------------------
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

      call writit ( sum1,im,jm,1,55,lattice )
      call writit ( sum2,im,jm,1,55,lattice )
      call writit ( sum3,im,jm,1,55,lattice )

c Gather and Broadcast Divergence Increment
c -----------------------------------------
      do L=1,lm
      call gather_2d ( dglo(1,1,L),diva(1,1,L),lattice )
      enddo
#ifdef mpi
      call mpi_bcast ( dglo,img*jmg*lm,lattice%mpi_rkind,0,comm,ierror )
#endif

c Compute Wind Increments Associated with Divergence Increment
c ------------------------------------------------------------
      do L=1,lm
      if( index(L).eq.myid ) then
          call VELPOT (dglo(1,1,L),chi,img,jmg)
          call gradq  (chi,  uchi,vchi,img,jmg)
          uglo(:,:,L) = uglo(:,:,L) + uchi(:,:)/dpglo(:,:,L)
          vglo(:,:,L) = vglo(:,:,L) + vchi(:,:)/dpglo(:,:,L)
      endif
      enddo
#ifdef mpi
      call mpi_barrier (comm,ierror)
      do L=1,lm
      call mpi_bcast ( uglo(1,1,L),img*jmg,lattice%mpi_rkind,index(L),comm,ierror )
      call mpi_bcast ( vglo(1,1,L),img*jmg,lattice%mpi_rkind,index(L),comm,ierror )
      enddo
      call mpi_barrier (comm,ierror)
#endif

! Scatter Winds
! -------------
      do L=1,lm
         if( trim(grid).eq.'d'  .or. 
     .       trim(grid).eq.'D' ) then
         if( lattice%myid.eq.0 ) then
             call atod   ( uglo(1,1,L),uglo(1,1,L),img,jmg,1,2 )
             call atod   ( vglo(1,1,L),vglo(1,1,L),img,jmg,1,1 )
         endif
         endif
         call scatter_2d ( uglo(1,1,L),ua(1,1,L),lattice )
         call scatter_2d ( vglo(1,1,L),va(1,1,L),lattice )
      enddo

      deallocate ( sum1,sum2,sum3,lambda )
      deallocate ( chi,uchi,vchi         )
      deallocate ( uglo,vglo,dglo,dpglo  )
      return
      end

      SUBROUTINE GETDIV ( U,V,DP,DIV,IM,JM )
C ********************************************************************          
C ****                                                            ****          
C ****  THIS PROGRAM CALCULATES DIVERGENCE                        ****          
C ****  AT EACH LEVEL FOR A NON-STAGGERED A-GRID                  ****          
C ****                                                            ****          
C ****  INPUT:                                                    ****          
C ****    U ....... ZONAL WIND                                    ****          
C ****    V ....... MERIDIONAL WIND                               ****          
C ****    IM ...... NUMBER OF LONGITUDE POINTS                    ****          
C ****    JM ...... NUMBER OF LATITUDE  POINTS                    ****          
C ****                                                            ****          
C ****  OUTPUT:                                                   ****          
C ****    DIV  (IM,JM) .... DIVERGENCE                            ****          
C ****                                                            ****          
C ********************************************************************          
                                                                                
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
                                                                                
C *********************************************************                     
C ****          INITIALIZATION FOR DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
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

C ********************************************************                     
C ****          CALCULATE AVERAGE QUANTITIES           ****                     
C *********************************************************                     
                                                                                
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
                                                                                
C *********************************************************                     
C ****        CALCULATE HORIZONTAL DIVERGENCE          ****                     
C *********************************************************                     
                                                                                
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
                                                                                
C *********************************************************                     
C ****     CALCULATE HORIZONTAL DIVERGENCE AT POLES    ****                     
C *********************************************************                     
                                                                                
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
C *********************************************************
C ****                                                 ****
C ****  THIS PROGRAM CALCULATES THE HORIZONTAL         ****
C ****  GRADIENT OF THE INPUT FIELD Q                  ****
C ****                                                 ****
C ****  ARGUMENTS:                                     ****
C ****    Q ....... FIELD TO BE DIFFERENTIATED         ****
C ****    DQDX .... LONGITUDINAL Q-DERIVATIVE          ****
C ****    DQDY .... MERIDIONAL   Q-DERIVATIVE          ****
C ****    IM ...... NUMBER OF LONGITUDINAL POINTS      ****
C ****    JM ...... NUMBER OF LATITUDINAL  POINTS      ****
C ****                                                 ****
C *********************************************************

      use MAPL_ConstantsMod
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

C *********************************************************
C ****               INITIALIZATION                    ****
C *********************************************************

      a  = MAPL_RADIUS
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

C *********************************************************
C ****          CALCULATE AVERAGE QUANTITIES           ****
C *********************************************************

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

C *********************************************************
C ****             CALCULATE Q-GRADIENTS               ****
C *********************************************************

      do j   = 2,jm-1
         i   = im-1
         ip1 = im
      do ip2 = 1,im
      DQDX(ip1,j) =  ACOS(j) * ( ( Q1X(ip1,j)-Q1X(i,j) )*CX1
     .                         - ( Q2X(ip2,j)-Q2X(i,j) )*CX2 )
         i   = ip1
         ip1 = ip2
      enddo
      enddo

      do j=2,jm-1
      do i=1,im
      DQDY(i,j) = ( Q1Y(i,j)  -Q1Y(i,j-1) )*CY1
     .          - ( Q2Y(i,j+1)-Q2Y(i,j-1) )*CY2
      enddo
      enddo

C *********************************************************
C ****         CALCULATE Q-GRADIENTS (POLES)           ****
C *********************************************************

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
      DQDY(i,jm) = ( Q1Y(i,jm)-Q1Y(i,jm-1) )*CY1
     .           - ( Q2Y(i,jm)-Q2Y(i,jm-1) )*CY2

      DQDY(i, 1) = ( Q1Y(i,1)-Q1Y(i,2) )*CY1
     .           - ( Q2Y(i,2)-Q2Y(i,1) )*CY2
      enddo

C   APPLY BOUNDARY CONDITIONS AT THE POLES
C ==========================================

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
 
c Transpose the input array
c -------------------------
      do j=1,jnp
      do i=1,im
      vp(j,i)   = div(i,j)
      enddo
      vp(j,imp) =  vp(j,1)
      enddo

C === SET THE INPUT VARIABLES
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
 
      CALL PWSSSP (INTL,TS,TF,M,MBDCND,BDTS,BDTF,PS,PF,N,NBDCND,BDPS,
     *             BDPF,ELMBDA,VP,IDIMF,PERTRB,IERROR,W)

      if( ierror.ne.0 ) then
          print *, 'PWSSSP IERROR = ',ierror
          stop
      endif
 
c Scale by earth radius
c ---------------------
      do j=1,jnp
      do i=1,im
      VELP(I,J) = VP(J,I) * RAD * RAD
      enddo
      enddo
 
c Remove global mean
c ------------------
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

c Ensure unique pole values
c -------------------------
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

c Compute global average
c ----------------------
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

c     print *, 'Remove Global Average: ', qave

      RETURN
      END
