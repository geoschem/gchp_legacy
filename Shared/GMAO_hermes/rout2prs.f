C---------------------------------------------------------------
C Program:	eta2prs.prog
C Date Coded:	March 6, 2000 
C Description:	Converts refout prognostic output from FVGCM
C		model eta levels to pressure.  
C---------------------------------------------------------------

      program eta2prs

      implicit none

      integer im, jm, km, pm    	
      parameter (im = 144)
      parameter (jm = 91)
      parameter (km = 55)	
      parameter (pm = 36)	! 36 GEOS DAS levels output

      real 	ak(km+1)	! model eta level parameter a
      real 	bk(km+1)	! model eta level parameter b
      real 	delp(im,jm,km)  ! model level pressure thickness 
      real	es(im,jm)	! saturation vapor pressure (not needed)
      character*80 filein	! input filename instantaneous
      character*80 fileout	! output filename
      real 	f2d(im,jm)	! 2D data 
      real 	f3d(im,jm,km) 	! 3D data on pressure coordinates
      real      f3dp(im,jm,pm)	! 3d field on requested pressure levels to write
      logical   first		! flag to initialize water vapor sat table
      integer 	i,j,k,kk,l,n,nn,ntime
      logical   noeof		! true, end of file not reached
      real	rh(im,jm,pm)	! relative humidity
      integer 	ro		! record number of output file
      real 	pe(im,jm,km+1)  ! edge eta levels could be p, p**kappa, or log(p)
      real	pout(pm)	! pressure levels 
      real 	p_out(pm)	! array of output log(pressure) levels
      real 	ps(im,jm)	! surface pressure
      real      satq(im,jm,pm)	! saturation specific humidity (kg/kg) on output levels
      real      sphu(im,jm,pm)	! specific humidity (kg/kg) on output levels
      real 	tmpu(im,jm,pm)  ! temperature (K) on output levels
      real 	undef		! undefined value

C-- Variable Initialization -----------------------------------

      undef = 1.e25

      data pout /0.2, 0.4, 1., 2., 3., 5., 7., 10., 20., 30., 40.,
     .           50., 70., 100., 150., 200., 250., 300., 350., 400., 
     .           450., 500., 550., 600., 650., 700., 750., 800., 825.,
     .           850., 875., 900., 925., 950., 975., 1000./

      do k = 1, pm 
        p_out(k) = log(pout(k) * 1.e2)
      enddo

      data ak /1.0000,    2.0000,    3.2700,    4.7585,     6.6000,
     &        8.9345,   11.9703,   15.9495,   21.1349,    27.8526,
     &       36.5041,   47.5806,   61.6779,   79.5134,   101.9442,
     &      130.0508,  165.0792,  208.4972,  262.0212,   327.6433,
     &      407.6567,  504.6805,  621.6800,  761.9839,   929.2943,
     &     1127.6888, 1364.3392, 1645.7072, 1979.1554,  2373.0361,
     &     2836.7816, 3380.9955, 4017.5417, 4764.3932,  5638.7938,
     &     6660.3377, 7851.2298, 9236.5661,10866.3427, 12420.400 ,
     &    13576.500 , 14365.400, 14807.800, 14915.500 , 14691.400, 
     &    14129.400 , 13214.800, 11923.200, 10220.700 ,  8062.000,
     &     5849.500 ,  3777.000,  2017.200,   720.600,      0.000,
     &        0.000 /

      data bk /                      0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.003633 , 0.014628 , 0.033276 , 0.060071 ,
     &        0.095722 , 0.141171 , 0.197623 , 0.266571 , 0.349839 ,
     &        0.449632 , 0.568589 , 0.685887 , 0.793252 , 0.883128 ,
     &        0.948792 , 0.9851119, 1.0000000 /


      undef = 1.e25
      first = .true.
      noeof = .true.

C----- Open files ---------------------------------------------

      call getarg (1, filein)
      call getarg (2, fileout)

       write(6,*) filein
       write(6,*) fileout
      
      open (11, file=filein,access='sequential',form='unformatted',
     .          status='old')
      open (12, file=fileout,access='direct',form='unformatted',
     .          recl=im*jm*4,status='new')
   
C----- Read input file -------------------------------------------

C    z    55   0 geop-height
C    zs    0   0 surf. geop.
C    slp   0   0 sea-level pressure (Pa)
C    ps    0   0 surface pressure (Pa)
C    u    55   0 u (m/s)
C    v    55   0 v (m/s)
C    t    55   0 temperature (K)
C    pv   55   0 Ertel Potential Vorticity
C    q    55   0 Specific humidity
C    tg    0   0 Surface temperature
C    precp 0   0 Instaneous total precip 

        ro = 0  
        do while (noeof)

        do n = 1,6	! six upper air fields to interpolate

          do k = 1, km	
            read(11,END=9999) ((f3d(i,j,k),i=1,im),j=1,jm)	
          enddo

          if (n.eq.1) then
            do nn = 1,2
              read(11) f2d
              ro = ro+1
              write(12,rec=ro) f2d
            enddo

            read(11) ps
            ro = ro+1
            write(12,rec=ro) ps

            do k = 1, km + 1
            do j = 1, jm
            do i = 1, im
              pe(i,j,k) = ak(k) + bk(k) * ps(i,j)
              pe(i,j,k) = log(pe(i,j,k))
            enddo
            enddo
            enddo
          endif


          call p2p(im, jm, km, pm, p_out, pe, undef, f3d, f3dp)	
      
          do k = pm, 1, -1	! flip to write bottom-up
            ro = ro + 1
            write(12,rec=ro) ((f3dp(i,j,k),i=1,im),j=1,jm)
          enddo
  
          if (n .eq. 4) then	! save temp to compute rh
            do k = 1, km
            do j = 1, jm
            do i = 1, im
              tmpu(i,j,k) = f3dp(i,j,k)
            enddo
            enddo
            enddo
          else if (n .eq. 6) then	! save sphu to compute rh
            do k = 1, km
            do j = 1, jm
            do i = 1, im
              sphu(i,j,k) = f3dp(i,j,k)
            enddo
            enddo
            enddo
          endif
  
        enddo


C----------- Compute Extra Diagnostics -------------------------------
 
      if (first) then
        call gestbl
        first = .false.
      endif

c  Compute relative humidity
      do k = 1, pm
        call vqsat(tmpu(1,1,k),pout(k)*100.,es,satq(1,1,k),im*jm,undef)
        do j = 1, jm
        do i = 1, im
          if (sphu(i,j,k) .ne. undef) then
            rh(i,j,k) = sphu(i,j,k) / satq(i,j,k)
          else
            rh(i,j,k) = undef
          endif
        enddo
        enddo
c       write(6,*) ' T Qstar Q rh ',tmpu(40,45,k),satq(40,45,k),
c    .              sphu(40,45,k),rh(40,45,k)
      enddo

      do k = pm, 1, -1	! flip to write bottom-up
        ro = ro + 1
        write(12,rec=ro) ((rh(i,j,k),i=1,im),j=1,jm)
      enddo

      do n = 1,2
        read(11) f2d
        ro = ro+1
        write(12,rec=ro) f2d
      enddo

      enddo		! keep looping until end of file

 9999 close(11)		! end of file reached
      close(12)

      stop
      end
C---------------------------------------------------------------
C Subroutine:	P2P
C Date Coded:	July 21, 1999 
C Description:	Converts fvccm3-0.9.7 model level (eta) output
C		to pressure levels (int_ppm).
C---------------------------------------------------------------

      subroutine p2p(im, jm, km, pm, p_out, pe, undef, f3d, f3dp)


      implicit none

c input variables
      integer 	im, jm, km	! eta model dimensions
      real 	f3d(im,jm,km) 	! 3D data on eta coordinates
      real 	pe(im,jm,km+1)  ! edge pressure levels could be p, p**kappa, or log(p)
      integer 	pm		! number of output pressure levels
      real 	p_out(pm)	! array of output log(pressure) levels
      real 	undef		! undefined value

c output variables
      real 	f3dp(im,jm,pm)  ! 3D data on pressure coordinates

c variables unique to routine
      real 	f2d(im,jm)	  ! 2D data on eta coordinates
      real 	f3dflip(im,km,jm) ! 3D data on eta coordinates flipped level lat
      real 	f2dp(im,pm)       ! 2D data (vertical)
      integer 	i,j,k,l,n
      real 	peflip(im,km+1,jm) ! pe flipped level lat
 
      real 	dp1(im,km)
      real 	dq1(im,km)
      real 	dc(im,km)
      real	qr(im,km)
      real 	ql(im,km)
      real 	q6(im,km)

C-- Variable Initialization -----------------------------------

      do k = 1, km
      do i = 1, im
      dc(i,k) = 0.
      qr(i,k) = 0.
      ql(i,k) = 0.
      q6(i,k) = 0.
      enddo
      enddo

c flip level and lat indicies for int_ppm
      do k = 1, km  		
        do j = 1, jm
          do i = 1, im
            f3dflip(i,k,j) = f3d(i,j,k) 	
          end do
        end do
      end do

      do k = 1, km+1
        do j = 1, jm
          do i = 1, im
            peflip(i,k,j) = pe(i,j,k) 	
          end do
        end do
      end do

C-------- Interpolate to pressure levels ---------------------

c Compute coordinate increments : thickness
      do j = 1, jm
        do k = 1, km
        do i = 1, im
          dp1(i,k) =  peflip(i,k+1,j) -  peflip(i,k,j)
          dq1(i,k) = f3dflip(i,k+1,j) - f3dflip(i,k,j)
        end do
        end do

        call int_ppm (im, km, pm, 1, im, peflip(1,1,j),
     .                p_out, f3dflip(1,1,j), f2dp, 3, dp1,
     .                dq1, dc, ql, qr, q6, undef)

        do k = 1, pm
        do i = 1, im
          f3dp(i,j,k) = f2dp(i,k)
        end do
        end do
      enddo
      
      return
      end



c------------------------------------------------------------------
      subroutine int_ppm(im,km,kn,i1,i2,pe1,pp,q1,q2,
     &                   lmt,dp1,dq1,dc,ql,qr,q6,undef)
c------------------------------------------------------------------

C This routine applies a high order monotonic interpolation algorithm
C for "inerpolating" cell-mena values to prescribed coordinate
C Input:
      real pe1(im,km+1)      !coordinate value at original grid edges
      real q1(im,km)         !original field
      real pp(kn)            !output coordinate
      real undef
C Input work arrays
      real dp1(im,km), qr(im,km), ql(im,km), q6(im,km)
      real dq1(im,km), dc(im,km)
C
C Output:
      real q2(im,kn)            ! output fields
C Local variabls:
      real qmax, qmin, qtmp
      real s


C#if ( defined OpenMP )
C!$omp  parallel do
C!$omp& default(shared)
C!$omp& private(dp1, dq1, dc, ql, qr, q6)
C#endif

C#if ( defined SGI ) || ( defined PGI )
Cc$doacross   local(dp1, dq1, dc, ql, qr, q6)
C#endif


c     do k=1,km
c       do i=i1,i2
c         dp1(i,k) = pe1(i,k+1) - pe1(i,k)
c         dq1(i,k) =  q1(i,k+1) -  q1(i,k)
c       end do
c     end do

C Construct the subgrid distribution of the original data
C accordingto the chosen lmt value
      call ppm2d (q1,ql,qr,q6,dp1,dq1,dc,im,km,i1,i2,lmt)

      do 555 i=i1,i2
         k0 = 1
      do 500 k=1,kn

C Start searching
      if(pp(k) .lt. pe1(i,1) .or. pp(k) .gt. pe1(i,km+1) ) then
         q2(i,k) = undef
      else
        do 45 l=k0,km
        if(pp(k) .le. pe1(i,L+1) .and. pp(k) .ge. pe1(i,l)) then

         if(L .eq. 1) then
            qmax = max(q1(i,L), q1(i,L+1))
            qmin = min(q1(i,L), q1(i,L+1))
         elseif(L .eq. km) then
            qmax = max(q1(i,L), q1(i,L-1))
            qmin = min(q1(i,L), q1(i,L-1))
         else
            qmax = max(q1(i,L-1),q1(i,L), q1(i,L+1))
            qmin = min(q1(i,L-1),q1(i,L), q1(i,L+1))
         endif

           s  = (pp(k)-pe1(i,L)) / dp1(i,L)
         qtmp = ql(i,l) + s*(qr(i,l) - ql(i,l) +
     &                       q6(i,l)*(1.-s)     )

         qtmp    = max(qtmp, qmin)
         q2(i,k) = min(qtmp, qmax)
         k0 = l
         goto 500
        endif
45      continue
      endif
500   continue
555   continue

      return
      end


C****6***0*********0*********0*********0*********0*********0**********72
      subroutine ppm2d(p,al,ar,a6,delp,delq,dc,im,km,i1,i2,lmt)
C****6***0*********0*********0*********0*********0*********0**********72
      real p(im,km),al(im,km),ar(im,km),a6(im,km),
     &     delp(im,km)
      real dc(im,km), delq(im,km)
 
      km1 = km - 1
 
      do 500 k=2,km
      do 500 i=i1,i2
500   A6(i,k) = delp(i,k-1) + delp(i,k)

c     do 1000 k=1,km1
c     do 1000 i=i1,i2
c     delq(i,k) = P(i,k+1) - P(i,k)
c1000  continue
 
      DO 1220 k=2,km1
      DO 1220 i=i1,i2
      c1 = (delp(i,k-1)+0.5*delp(i,k))/A6(i,k+1)
      c2 = (delp(i,k+1)+0.5*delp(i,k))/A6(i,k)
      tmp = delp(i,k)*(c1*delq(i,k) + c2*delq(i,k-1)) /
     &                              (A6(i,k)+delp(i,k+1))
      Qmax = max(P(i,k-1),P(i,k),P(i,k+1)) - P(i,k)
      Qmin = P(i,k) - min(P(i,k-1),P(i,k),P(i,k+1))
      dc(i,k) = sign(min(abs(tmp),Qmax,Qmin), tmp)
1220  continue
 
C****6***0*********0*********0*********0*********0*********0**********72
C 4th order interpolation of the provisional cell edge value
C****6***0*********0*********0*********0*********0*********0**********72
 
      DO 12 k=3,km1
      DO 12 i=i1,i2
      c1 = delq(i,k-1)*delp(i,k-1) / A6(i,k)
      A1 = A6(i,k-1) / (A6(i,k) + delp(i,k-1))
      A2 = A6(i,k+1) / (A6(i,k) + delp(i,k))
      AL(i,k) = P(i,k-1) + c1 + 2./(A6(i,k-1)+A6(i,k+1)) *
     &          ( delp(i,k  )*(c1*(A1 - A2)+A2*DC(i,k-1)) -
     &                          delp(i,k-1)*A1*DC(i,k  ) )
12    CONTINUE
 
 
C three-cell parabolic subgrid distribution at model top
 
      DO 10 i=i1,i2
C three-cell PP-distribution
C Compute a,b, and c of q = aP**2 + bP + c using cell averages and delp
C a3 = a / 3
C b2 = b / 2
      S1 = delp(i,1)
      S2 = delp(i,2) + S1
C
      S3 = delp(i,2) + delp(i,3)
      S4 = S3 + delp(i,4)
      SS3 =  S3 + S1
      S32 = S3*S3
      S42 = S4*S4
      S34 = S3*S4
C model top
      a3 = (delq(i,2) - delq(i,1)*S3/S2) / (S3*SS3)
C
      if(abs(a3) .gt. 1.E-14) then
         b2 =  delq(i,1)/S2 - a3*(S1+S2)
         SC = -b2/(3.*a3)
         if(SC .lt. 0. .or. SC .gt. S1) then
             AL(i,1) = P(i,1) - S1*(a3*S1 + b2)
         else
             AL(i,1) = P(i,1) - delq(i,1)*S1/S2
         endif
      else
C Linear
         AL(i,1) = P(i,1) - delq(i,1)*S1/S2
      endif
      DC(i,1) = P(i,1) - AL(i,1)
C compute coef. for the off-centered area preserving cubic poly.
      DM = delp(i,1) / (S34*SS3*(delp(i,2)+S3)*(S4+delp(i,1)))
      F1 = delp(i,2)*S34 / ( S2*SS3*(S4+delp(i,1)) )
      F2 = (delp(i,2)+S3) * (SS3*(delp(i,2)*S3+S34+delp(i,2)*S4)
     &      + S42*(delp(i,2)+S3+S32/S2))
      F3 = -delp(i,2)*( SS3*(S32*(S3+S4)/(S4-delp(i,2))
     &      + (delp(i,2)*S3+S34+delp(i,2)*S4))
     &      + S42*(delp(i,2)+S3) )
      F4 = SS3*delp(i,2)*S32*(delp(i,2)+S3) / (S4-delp(i,2))
      AL(i,2) = F1*P(i,1)+(F2*P(i,2)+F3*P(i,3)+F4*P(i,4))*DM
C****6***0*********0*********0*********0*********0*********0**********72
C No over- and undershoot condition
      Cmax = max(P(i,1), P(i,2))
      Cmin = min(P(i,1), P(i,2))
      AL(i,2) = max(Cmin,AL(i,2))
      AL(i,2) = min(Cmax,AL(i,2))
10    continue

C****6***0*********0*********0*********0*********0*********0**********72

C Bottom
C Area preserving cubic with 2nd deriv. = 0 at the surface
      DO 15 i=i1,i2
      d1 = delp(i,km )
      d2 = delp(i,km1)
      qm = (d2*P(i,km)+d1*P(i,km1)) / (d1+d2)
      dq = 2.*(P(i,km1)-P(i,km)) / (d1+d2)
      c1 = (AL(i,km1)-qm-d2*dq) / (d2*(2.*d2*d2+d1*(d2+3.*d1)))
      c3 = dq - 2.0*c1*(d2*(5.*d1+d2)-3.*d1**2)
      AL(i,km) = qm - c1*d1*d2*(d2+3.*d1)
      AR(i,km) = d1*(8.*c1*d1**2-c3) + AL(i,km)
      DC(i,km) = AR(i,km) -  P(i,km)
C****6***0*********0*********0*********0*********0*********0**********72
C No over- and undershoot condition
      Cmax = max(P(i,km), P(i,km1))
      Cmin = min(P(i,km), P(i,km1))
      AL(i,km) = max(Cmin,AL(i,km))
      AL(i,km) = min(Cmax,AL(i,km))
C****6***0*********0*********0*********0*********0*********0**********72
15    continue

      do 20 k=1,km1
      do 20 i=i1,i2
      AR(i,k) = AL(i,k+1)
20    continue
C
C f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
C
      do 30 k=1,km
      do 30 i=i1,i2
      A6(i,k) = 3.*(P(i,k)+P(i,k) - (AL(i,k)+AR(i,k)))
30    continue
 
      if(LMT.LE.2) then
	len = i2-i1+1
         do k=1,km
         call lmppm(DC(i1,k),A6(i1,k),AR(i1,k),AL(i1,k),
     &               P(i1,k),len,LMT)
         enddo
      endif
      return
      end

C****6***0*********0*********0*********0*********0*********0**********72
      subroutine lmppm(DM,A6,AR,AL,P,IM,LMT)
C****6***0*********0*********0*********0*********0*********0**********72
      parameter ( R12 = 1./12. )
      REAL A6(IM),AR(IM),AL(IM),P(IM),DM(IM)
 
C LMT = 0: full monotonicity
C LMT = 1: semi-monotonic constraint (no undershoot)
C LMT = 2: positive-definite constraint
 
      if(LMT.eq.0) then
C Full constraint
      do 100 i=1,IM
      if(DM(i).eq.0.) then
            AR(i) = p(i)
            AL(i) = p(i)
            A6(i) = 0.
      else
      da1  = AR(i) - AL(i)
      da2  = da1**2
      A6DA = A6(i)*da1
      if(A6DA .lt. -da2) then
            A6(i) = 3.*(AL(i)-p(i))
            AR(i) = AL(i) - A6(i)
      elseif(A6DA .gt. da2) then
            A6(i) = 3.*(AR(i)-p(i))
            AL(i) = AR(i) - A6(i)
      endif
      endif
100   continue
      elseif(LMT.eq.1) then
C Semi-monotonic constraint
      do 150 i=1,IM
      if(abs(AR(i)-AL(i)) .GE. -A6(i)) go to 150
      if(p(i).lt.AR(i) .and. p(i).lt.AL(i)) then
            AR(i) = p(i)
            AL(i) = p(i)
            A6(i) = 0.
      elseif(AR(i) .gt. AL(i)) then
            A6(i) = 3.*(AL(i)-p(i))
            AR(i) = AL(i) - A6(i)
      else
            A6(i) = 3.*(AR(i)-p(i))
            AL(i) = AR(i) - A6(i)
      endif
150   continue
      elseif(LMT.eq.2) then
C Positive definite constraint
      do 250 i=1,IM
      if(abs(AR(i)-AL(i)) .GE. -A6(i)) go to 250
      fmin = p(i) + 0.25*(AR(i)-AL(i))**2/A6(i) + A6(i)*R12
      if(fmin.ge.0.) go to 250
      if(p(i).lt.AR(i) .and. p(i).lt.AL(i)) then
            AR(i) = p(i)
            AL(i) = p(i)
            A6(i) = 0.
      elseif(AR(i) .gt. AL(i)) then
            A6(i) = 3.*(AL(i)-p(i))
            AR(i) = AL(i) - A6(i)
      else
            A6(i) = 3.*(AR(i)-p(i))
            AL(i) = AR(i) - A6(i)
      endif
250   continue
      endif
      return
      end


      subroutine vqsat(t, p, es, qs, len, undef)
C-----------------------------------------------------------------------
C
C Utility procedure to look up and return saturation vapor pressure from 
C precomputed table, calculate and return saturation specific humidity 
C (g/g), for input vectors of temperature and pressure (length len)
C
C----------------------------Code History-------------------------------
C
C Original version:  J. Hack
C Standardized:      J. Rosinski, June 1992
C                    T. Acker, March 1996
C Reviewed:          J. Hack, August 1992
C Modified:	To handle pressure level data which can have undefined
C		values.  This code was also stripped of the use of the
C		land index array location.
C-----------------------------------------------------------------------
c
c $Id$
c $Author$
c
C-----------------------------------------------------------------------
      implicit none
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer len              ! Vector length 
c
      real t(len)              ! Temperature
      real p                   ! Pressure
      real undef               ! undefined value
C
C Output arguments
C
      real es(len)             ! Saturation vapor pressure
      real qs(len)             ! Saturation specific humidity
C
C--------------------------Local Variables------------------------------
C
      real omeps               ! 1 - 0.622
      integer i,ii             ! Local vector indices
C
C-----------------------------------------------------------------------
c
c $Id$
c $Author$
c
C
C Common block and statement functions for saturation vapor pressure
C look-up procedure, J. J. Hack, February 1990
C
      integer plenest  ! length of saturation vapor pressure table
      parameter (plenest=250)
C
C Table of saturation vapor pressure values es from tmin degrees
C to tmax+1 degrees k in one degree increments.  ttrice defines the
C transition region where es is a combination of ice & water values
C
      common/comes/estbl(plenest) ,tmin  ,tmax  ,ttrice ,pcf(6) ,
     $             epsqs          ,rgasv ,hlatf ,hlatv  ,cp     ,
     $             icephs
C
      real estbl      ! table values of saturation vapor pressure
      real tmin       ! min temperature (K) for table
      real tmax       ! max temperature (K) for table
      real ttrice     ! transition range from es over H2O to es over ice
      real pcf        ! polynomial coeffs -> es transition water to ice
      real epsqs      ! Ratio of h2o to dry air molecular weights 
      real rgasv      ! Gas constant for water vapor
      real hlatf      ! Latent heat of vaporization
      real hlatv      ! Latent heat of fusion
      real cp         ! specific heat of dry air
      logical icephs  ! false => saturation vapor press over water only
C
C Dummy variables for statement functions
C
      real td         ! dummy variable for function evaluation
      real tlim       ! intermediate variable for es look-up with estbl4
      real estblf     ! statement function es look-up
      real estbl4     ! statement function es look-up
C
C Statement functions used in saturation vapor pressure table lookup
C there are two ways to use these three statement functions.
C For compilers that do a simple in-line expansion:
C => ttemp = tlim(t)
C    es    = estbl4(ttemp)
C
C For compilers that provide real optimization:
C => es    = estblf(t)
C
      tlim(td) = max(min(td,tmax),tmin)
C
      estblf(td) =  (tmin + int(tlim(td)-tmin) - tlim(td) + 1.0)
     $            *estbl(int(tlim(td)-tmin)+1)
     $            -(tmin + int(tlim(td)-tmin) - tlim(td)      )
     $            *estbl(int(tlim(td)-tmin)+2)
C
      estbl4(td) =  (tmin+int(td-tmin)+1.0-td)*estbl(int(td-tmin)+1)
     $            + ( td-(tmin+int(td-tmin)) )*estbl(int(td-tmin)+2)
C
 
C-----------------------------------------------------------------------
C
      omeps = 1.0 - epsqs
c     write(6,*) ' epsqs = ',epsqs
CDIR$ IVDEP
      do i=1,len
        if (t(i) .ne. undef) then
         es(i) = estblf(t(i))
c        if (i .eq. 40) then
c         write(6,*) ' t es ',t(i),es(i)
c        endif
C
C Saturation specific humidity
C
         qs(i) = epsqs*es(i)/(p - omeps*es(i))
         
c        if (i .eq. 40) then
c        write(6,*) ' qs ',qs(i)
c        endif
C
C The following check is to avoid the generation of negative values
C that can occur in the upper stratosphere and mesosphere
C
         qs(i) = min(1.0,qs(i))
C
         if (qs(i) .lt. 0.0) then
            qs(i) = 1.0
            es(i) = p
         end if
        else
          qs(i) = undef
          es(i) = undef
        endif
      end do

      return
C
      end
 

      subroutine gestbl
C-----------------------------------------------------------------------
C
C Builds saturation vapor pressure table for later lookup procedure.
C Uses Goff & Gratch (1946) relationships to generate the table
C according to a set of free parameters defined below.  Auxiliary
C routines are also included for making rapid estimates (well with 1%)
C of both es and d(es)/dt for the particular table configuration.
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Hack
C Standardized:      L. Buja, Jun 1992,  Feb 1996
C Reviewed:          J. Hack, G. Taylor, Aug 1992
C                    J. Hack, Aug 1992
C
C-----------------------------------------------------------------------
c
c $Id$
c $Author$
c
C-----------------------------------------------------------------------
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real tmn           ! Minimum temperature entry in es lookup table
      real tmx           ! Maximum temperature entry in es lookup table
      real epsil         ! Ratio of h2o to dry air molecular weights
      real trice         ! Transition range from es over range to es over ice
      real latvap        ! Latent heat of vaporization
      real latice        ! Latent heat of fusion
      real rh2o          ! Gas constant for water vapor
      real cpair         ! Specific heat of dry air
C
C---------------------------Local variables-----------------------------
C
      real t             ! Temperature
      integer n          ! Increment counter
      integer lentbl     ! Calculated length of lookup table
      integer itype      ! Ice phase: 0 -> no ice phase
                         !            1 -> ice phase, no transition
                         !           -x -> ice phase, x degree transition
      logical ip         ! Ice phase logical flag
C
C---------------------------Statement function--------------------------
C
C
c
c $Id$
c $Author$
c
C
C Common block and statement functions for saturation vapor pressure
C look-up procedure, J. J. Hack, February 1990
C
      integer plenest  ! length of saturation vapor pressure table
      parameter (plenest=250)
C
C Table of saturation vapor pressure values es from tmin degrees
C to tmax+1 degrees k in one degree increments.  ttrice defines the
C transition region where es is a combination of ice & water values
C
      common/comes/estbl(plenest) ,tmin  ,tmax  ,ttrice ,pcf(6) ,
     $             epsqs          ,rgasv ,hlatf ,hlatv  ,cp     ,
     $             icephs
C
      real estbl      ! table values of saturation vapor pressure
      real tmin       ! min temperature (K) for table
      real tmax       ! max temperature (K) for table
      real ttrice     ! transition range from es over H2O to es over ice
      real pcf        ! polynomial coeffs -> es transition water to ice
      real epsqs      ! Ratio of h2o to dry air molecular weights 
      real rgasv      ! Gas constant for water vapor
      real hlatf      ! Latent heat of vaporization
      real hlatv      ! Latent heat of fusion
      real cp         ! specific heat of dry air
      logical icephs  ! false => saturation vapor press over water only
C
C Dummy variables for statement functions
C
      real td         ! dummy variable for function evaluation
      real tlim       ! intermediate variable for es look-up with estbl4
      real estblf     ! statement function es look-up
      real estbl4     ! statement function es look-up
C
C Statement functions used in saturation vapor pressure table lookup
C there are two ways to use these three statement functions.
C For compilers that do a simple in-line expansion:
C => ttemp = tlim(t)
C    es    = estbl4(ttemp)
C
C For compilers that provide real optimization:
C => es    = estblf(t)
C
      tlim(td) = max(min(td,tmax),tmin)
C
      estblf(td) =  (tmin + int(tlim(td)-tmin) - tlim(td) + 1.0)
     $            *estbl(int(tlim(td)-tmin)+1)
     $            -(tmin + int(tlim(td)-tmin) - tlim(td)      )
     $            *estbl(int(tlim(td)-tmin)+2)
C
      estbl4(td) =  (tmin+int(td-tmin)+1.0-td)*estbl(int(td-tmin)+1)
     $            + ( td-(tmin+int(td-tmin)) )*estbl(int(td-tmin)+2)
C
 
C-----------------------------------------------------------------------
C
C Set es table parameters
C
      tmin   = 173.16       ! Minimum temperature entry in table
      tmax   = 375.16       ! Maximum temperature entry in table
      ttrice = 20.   	    ! Trans. range from es over h2o to es over ice
      icephs = .true.       ! Ice phase (true or false)
C
C Set physical constants required for es calculation
C
      epsqs  = 0.622
      hlatv  = 2.5104e6
      hlatf  = 3.336e5
      rgasv  = 4.61e2
      cp     = 1004.64
C
      lentbl = ifix(tmax-tmin+2.000001)
      if (lentbl .gt. plenest) then
         write(6,9000) tmax, tmin, plenest
         stop
      end if
C
C Begin building es table.
C Check whether ice phase requested.
C If so, set appropriate transition range for temperature
C
      if (icephs) then
         if(ttrice.ne.0.0) then
            itype = -ttrice
         else
            itype = 1
         end if
      else
         itype = 0
      end if
C
      t = tmin - 1.0
      do n=1,lentbl
         t = t + 1.0
         call gffgch(t,estbl(n),itype)
      end do
C
      do n=lentbl+1,plenest
         estbl(n) = -99999.0
      end do
C
C Table complete -- Set coefficients for polynomial approximation of
C difference between saturation vapor press over water and saturation
C pressure over ice for -ttrice < t < 0 (degrees C). NOTE: polynomial
C is valid in the range -40 < t < 0 (degrees C).
C
C                  --- Degree 5 approximation ---
C
      pcf(1) =  5.04469588506e-01
      pcf(2) = -5.47288442819e+00
      pcf(3) = -3.67471858735e-01
      pcf(4) = -8.95963532403e-03
      pcf(5) = -7.78053686625e-05
C
C                  --- Degree 6 approximation ---
C
C-----pcf(1) =  7.63285250063e-02
C-----pcf(2) = -5.86048427932e+00
C-----pcf(3) = -4.38660831780e-01
C-----pcf(4) = -1.37898276415e-02
C-----pcf(5) = -2.14444472424e-04
C-----pcf(6) = -1.36639103771e-06
C
C
 9000 format('GESTBL: FATAL ERROR *********************************',/,
     $     ' TMAX AND TMIN REQUIRE A LARGER DIMENSION ON THE LENGTH',
     $     ' OF THE SATURATION VAPOR PRESSURE TABLE ESTBL(PLENEST)',/,
     $     ' TMAX, TMIN, AND PLENEST => ', 2f7.2, i3)
C 
      end
 
      subroutine gffgch(t       ,es      ,itype   )
C-----------------------------------------------------------------------
C
C Computes saturation vapor pressure over water and/or over ice using
C Goff & Gratch (1946) relationships.  T (temperature), and itype are
C input parameters, while es (saturation vapor pressure) is an output
C parameter.  The input parameter itype serves two purposes: a value of
C zero indicates that saturation vapor pressures over water are to be
C returned (regardless of temperature), while a value of one indicates
C that saturation vapor pressures over ice should be returned when t is
C less than 273.16 degrees k.  If itype is negative, its absolute value
C is interpreted to define a temperature transition region below 273.16
C degrees k in which the returned saturation vapor pressure is a
C weighted average of the respective ice and water value.  That is, in
C the temperature range 0 => -itype degrees c, the saturation vapor
C pressures are assumed to be a weighted average of the vapor pressure
C over supercooled water and ice (all water at 0 c; all ice at -itype
C c).  Maximum transition range => 40 c
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Hack
C Standardized:      L. Buja, Jun 1992,  Feb 1996
C Reviewed:          J. Hack, G. Taylor, Aug 1992
C                    J. Hack, Feb 1996 
C
C-----------------------------------------------------------------------
c
c $Id$
c $Author$
c
C-----------------------------------------------------------------------
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real t          ! Temperature
      integer itype   ! Flag for ice phase and associated transition
C
C Output arguments
C
      real es         ! Saturation vapor pressure
C
C---------------------------Local variables-----------------------------
C
      real e1         ! Intermediate scratch variable for es over water
      real e2         ! Intermediate scratch variable for es over water
      real eswtr      ! Saturation vapor pressure over water
      real f          ! Intermediate scratch variable for es over water
      real f1         ! Intermediate scratch variable for es over water
      real f2         ! Intermediate scratch variable for es over water
      real f3         ! Intermediate scratch variable for es over water
      real f4         ! Intermediate scratch variable for es over water
      real f5         ! Intermediate scratch variable for es over water
      real ps         ! Reference pressure (mb)
      real t0         ! Reference temperature (freezing point of water)
      real term1      ! Intermediate scratch variable for es over ice
      real term2      ! Intermediate scratch variable for es over ice
      real term3      ! Intermediate scratch variable for es over ice
      real tr         ! Transition range for es over water to es over ice
      real ts         ! Reference temperature (boiling point of water)
      real weight     ! Intermediate scratch variable for es transition
      integer itypo   ! Intermediate scratch variable for holding itype
C
C-----------------------------------------------------------------------
C      
C Check on whether there is to be a transition region for es
C
      if (itype.lt.0) then
        tr    = abs(float(itype))
        itypo = itype
        itype = 1
      else
        tr    = 0.0
        itypo = itype
      end if
      if (tr .gt. 40.0) then
        write(6,900) tr
        stop
      end if
C
      if(t .lt. (273.16 - tr) .and. itype.eq.1) go to 10
C
C Water
C
      ps = 1013.246
      ts = 373.16
      e1 = 11.344*(1.0 - t/ts)
      e2 = -3.49149*(ts/t - 1.0)
      f1 = -7.90298*(ts/t - 1.0)
      f2 = 5.02808*log10(ts/t)
      f3 = -1.3816*(10.0**e1 - 1.0)/10000000.0
      f4 = 8.1328*(10.0**e2 - 1.0)/1000.0
      f5 = log10(ps)
      f  = f1 + f2 + f3 + f4 + f5
      es = (10.0**f)*100.0
      eswtr = es
C
      if(t.ge.273.16 .or. itype.eq.0) go to 20
C
C Ice
C
   10 continue
      t0    = 273.16
      term1 = 2.01889049/(t0/t)
      term2 = 3.56654*log(t0/t)
      term3 = 20.947031*(t0/t)
      es    = 575.185606e10*exp(-(term1 + term2 + term3))
C
      if (t.lt.(273.16 - tr)) go to 20
C
C Weighted transition between water and ice
C
      weight = min((273.16 - t)/tr,1.0)
      es = weight*es + (1.0 - weight)*eswtr
C
   20 continue
      itype = itypo
      return
C
  900 format('GFFGCH: FATAL ERROR ******************************',/,
     $       'TRANSITION RANGE FOR WATER TO ICE SATURATION VAPOR',
     $       ' PRESSURE, TR, EXCEEDS MAXIMUM ALLOWABLE VALUE OF',
     $       ' 40.0 DEGREES C',/, ' TR = ',f7.2)
C
      end
 

