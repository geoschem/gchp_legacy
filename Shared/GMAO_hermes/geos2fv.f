
      program geos2fv

c****6***0*********0*********0*********0*********0*********0**********72
C Program to convert GEOS-2 A-grid data on sigma coordinate to
C The hybrid sigma-P Eulerian coordinate coordinate for the
C finite-volume GCM

C The terrain and the IC for the fv-GCM must be 64-bit precision.

C Must compile with "f90 -O -r8" if FVGCM restart is desired; -r8 no
C longer necessary for grads or GFIO output (Arlindo) 
C Programmer : S.-J. Lin, Code 910.3, NASA/GSFC, with revisions by A. da Silva
C Last modified: July 21, 1999

c The terrain data can be found on our machines at nas

c /share/fvgcm/surf.data_144x91           for 2x2.5
c /share/fvgcm/surf.data_288x181              1x1.25
c /share/fvgcm/surf.data_576x361              0.5x0.625
c
c REVISION HISTORY:
c
c dark ages  SJ Lin    Initial code.
c
c 10oct1999  da Silva  Changed some real*8 into real for having the option of
c                      compiling with 32 bits (otherwise it does not on my 
c                      linux box).  When compiled in 32-bits the
c                      restart option does not produce a valid output file.
c
c 02nov1999  da Silva  Started conversion to read GEOS-DAS HDF files
c                      written by GFIO. GEOS levels are still hardwired 
c                      in the main program butb get_geos2() now uses
c                      dynamic allocation according to input file.
c
c 09nov1999  da Silva  Reading of SST for dyn vect.
c 06dec1999  da Silva  Better defaults for sst/surf file names.
c 18dec1999  da Silva  Introduced f77 interface of dyn_init()
c 20apr2001  da Silva  Added option of sst fname = 'NONE', setting sst
c                      to 288 K in this case.
c
c****6***0*********0*********0*********0*********0*********0**********72

      use m_dyn           ! dynamics state vector

      implicit      none

      type(dyn_vect) w_f

      integer rc


C Output horizontal resolution:

      integer imr, jmr
      integer jnp, nl

      parameter  (imr=144, jmr=90)
c     parameter  (imr=288, jmr=180)
c     parameter  (imr=576, jmr=360)
      parameter  (jnp = jmr+1)

C Output vertical resolution
c     parameter(nl=32)
      parameter(nl=55)

C Original data vertical resolution: For GEOS 48 and 70 layrs just
C   set olev below - no other change necessary (ams)

      integer plev
c     parameter (plev=55)
c     parameter (plev=70)
      parameter (plev=48)  

C Note: there are a few other hard-wired parameter statement that set the horizontal
C resolution. (For example, in plft2d and lpft)


C Input/output units:

      integer iuhs                !unit to read terrain data for the fv
      integer iuic                !unit to write IC for f-v dycore
      parameter (iuhs = 80)
      parameter (iuic = 81)

      integer it
      integer nymd, nhms, nstep

      include 'geos2fv.h'

c =================
c Dynamical fields:
c =================

      real     u(imr,jnp,nl)     !zonal wind on D-grid
      real     v(imr,jnp,nl)     !meridional wind
      real    pt(imr,jnp,nl)     !scaled virture potential temperature
      real     q(imr,jnp,nl,1)   !specific humidity & tracer mixing ratios
      real  delp(imr,jnp,nl)     !pressure thickness in pascal

      real   pe1(imr,nl+1)       !pressure at layer edges
      real    ps(imr, jnp)       !surface pressure (pascal)

      real    ts(imr, jnp)       ! sea surface temperature
      real   lwi(imr, jnp)       ! land-water-mask for dyn_vect

      real  ud(imr,jnp)          !D-grid zonal wind
      real  vd(imr,jnp)          !D-grid meridional wind
      real*8  hs8(imr,jnp)       !surface geopotential
      real*8  stdv8(imr,jnp)       !surface geopotential stdv
      real    hs(imr,jnp)       !surface geopotential
      real  phis(imr,jnp)        !surface geopotential
      real  stdv(imr,jnp)        !surface geopotential stdv

C Eta coordinate arrays and constants

      real ak(nl+1)
      real bk(nl+1)
      real ptop                  !pressure at model top
      real pint                  !Interface pressure
      integer ks                 !number of layers in pure p region

      real ps2(imr, jnp)
      real uwnd(imr,jnp,plev)
      real vwnd(imr,jnp,plev)
      real tmpu(imr,jnp,plev)
      real sphu(imr,jnp,plev)

      real  pe2(imr, plev+1, jnp)
      real phis2(imr,jnp)

      real u2(imr,plev)
      real v2(imr,plev)
      real q2(imr,plev)

      real pk2(imr,plev+1)
      real peln(imr,plev+1)
      real pkz2(imr,plev)
      real ph2(imr,plev+1)
      real  t2(imr,plev)

      real q_new(imr,nl)

      integer i,j,k

      integer fid, err, im, jm, km, lm, nvars, ngatts

      real grav
      real rh2o
      real rair
      real zvir
      real cappa
      real cpair
!ams  real*8 rkap
!ams  real*8 r8tmp
      real   rkap
      real   r8tmp

      real pmax, pmin, vmax
      real diff(imr, jnp)
      real dlp, beta

      real dp1(imr, nl)
      real dp2(imr, plev)
      integer npt
      logical geos2


      geos2 = .true.

c     Get file names and dates from command line
c     ------------------------------------------
	call parse_cmd ( nymd, nhms, imr, jnp, nstep )

C Hard wired year/date

C get geos-2 data


C Constants:

        grav   = 9.80616
        rh2o   = 4.61e2
        rair   = 287.04
        zvir   = rh2o/rair - 1.
        cpair  = 1004.64
        cappa  = rair/cpair
        rkap   = cpair/rair

        write(6,*) 'grav=',grav,' zvir=',zvir,' cappa=',cappa
        write(6,*) 'rkap=', rkap
        write(6,*) ' '

C get SST if writing GFIO file
       if ( ftype .eq. GFIO ) then

        if ( trim(sfname) .ne. 'NONE' ) then


         call GFIO_Open ( trim(sfname), READ_ONLY, fid, err )
         if ( err .ne. 0 ) then
            print *, 'geos2fv: cannot open SST file '//trim(sfname)
            stop
         endif

         call GFIO_DimInquire ( fid, im, jm, km, lm, nvars, ngatts, err)
         if ( err .ne. 0 ) then
            print *, 'geos2fv: error reading SST file '//trim(sfname)
            stop
         endif
         if ( im .ne. imr .or. jm .ne. jnp ) then
            print *, 'geos2fv: SST file has inconsistent dimensions'
            print *, 'geos2fv: SST file has         im, jm: ', im, jm 
            print *, 'geos2fv: SST file should have im, jm: ', imr, jnp
            stop
         end if

         call GFIO_GetVarT ( fid, 'SST', nymd, nhms, 
     &                       im, jm, 0, 1, Ts, err )
         if ( err .ne. 0 ) then
              print *, 'geos2fv: could not read sst'
              stop
         end if

         print *
         print *, 'geos2fv: just read SST file ', trim(sfname)

         Ts = Ts + 273.   ! convert to Kelvin

      else
 
         Ts = 15. + 273.

      end if

         pmax = vmax(Ts,pmin,imr*jnp)
         write(6,*) 'SST: max=', pmax, ' min=', pmin
         print *

       end if

C get terrain (in geopotential unit) for fvcore
      open(iuhs,file=tfname,form='unformatted')
      read(iuhs) hs8
      read(iuhs) stdv8

      do j=1,jnp
         do i=1,imr
            phis(i,j) = hs8(i,j)
            stdv(i,j) = stdv8(i,j)
         enddo
      enddo

c Check topography for fvcore

      pmax = vmax(phis,pmin,imr*jnp)
      write(6,*) 'FVCORE: max hs=', pmax/grav, ' min=', pmin/grav

      if(geos2) then
      call get_geos2( nymd, nhms, imr, jnp, plev, phis2, ps2,
     &                uwnd, vwnd, tmpu, sphu, pe2)
      else
      call get_d_rst(imr, jnp, plev, phis2, ps2,
     &              uwnd, vwnd, tmpu, sphu, pe2)
      endif

c Check topography

      pmax = vmax(phis2,pmin,imr*jnp)
      write(6,*) 'Original: max hs=', pmax/grav, ' min=', pmin/grav

      pmax = vmax(ps2,pmin,imr*jnp)
      write(6,*) 'max PS (mb)=', pmax/100., ' min=', pmin/100.

      pmax = vmax(pe2,pmin,imr*jnp*(plev+1))
      write(6,*) 'max PE=', pmax/100., ' min=', pmin/100.

      pmax = vmax(uwnd,pmin,imr*jnp*plev)
      write(6,*) 'max U=', pmax, ' min=', pmin

      pmax = vmax(vwnd,pmin,imr*jnp*plev)
      write(6,*) 'max V=', pmax, ' min=', pmin

      pmax = vmax(tmpu,pmin,imr*jnp*plev)
      write(6,*) 'max T=', pmax, ' min=', pmin

      pmax = vmax(sphu,pmin,imr*jnp*plev)
      write(6,*) 'max Q=', pmax, ' min=', pmin

      do j=1, jnp
         do i=1,imr
            diff(i,j) = phis(i,j) - phis2(i,j)
         enddo
      enddo

      pmax = vmax(diff,pmin,imr*jnp)
      write(6,*) 'Phis (NEW-OLD) max =', pmax/grav, ' min=', pmin/grav

      write(6,*) 'ptop=', pe2(1,1,1)

C setup vertical coordinate
      call set_eta(nl,ks,ptop,pint,ak,bk)

      do 1000 j=1,jnp

C Comput pk2
      do k=1,plev+1
         do i=1,imr
             pk2(i,k) =  pe2(i,k,j)**cappa
            peln(i,k) =  log(pe2(i,k,j))
         enddo
      enddo

      do k=1,plev
         do i=1,imr
            dp2(i,k) = pe2(i,k+1,j) - pe2(i,k,j)
         enddo
      enddo

C Compute dry pt

      do k=1, plev
         do i=1,imr
C Comput pkz2
              pkz2(i,k) = (pk2(i,k+1) - pk2(i,k) )  /
     &                  (cappa*(peln(i,k+1) - peln(i,k)) )
         enddo
      enddo

      do k=1, plev
         do i=1,imr
      if(geos2) then
C Convert tmpu to potential temp
              t2(i,k) = tmpu(i,j,k)/pkz2(i,k)
      else
              t2(i,k) = tmpu(i,j,k)
      endif
         enddo
      enddo

      do k=1, plev
         do i=1,imr
              u2(i,k) = uwnd(i,j,k)
              v2(i,k) = vwnd(i,j,k)
              q2(i,k) = sphu(i,j,k)
         enddo
      enddo

C Do dry convective adjustment

      do it = 1, 5
      call dry_adj(imr, plev, plev-1, t2,
     &             u2, v2, q2, dp2, npt)

      if(npt .ne. 0) then
!!!         write(6,*) 'j= ',j,' dry unstable points=', npt
      else
         go to 333
      endif
      enddo
333   continue

C Convert pt to virture potential temp
      if(geos2) then
      do k=1, plev
         do i=1,imr
              t2(i,k) = t2(i,k)*(1.+zvir*q2(i,k))
         enddo
      enddo
      endif

         do i=1,imr
            ph2(i,plev+1) = phis2(i,j)
         enddo

      do k=plev,1,-1
         do i=1,imr
            ph2(i,k) = ph2(i,k+1) + cpair*t2(i,k)*
     &                (pk2(i,k+1) - pk2(i,k))
         enddo
      enddo

c****6***0*********0*********0*********0*********0*********0**********72
C Adjust ps2 to new terrain
c****6***0*********0*********0*********0*********0*********0**********72

      do i=1,imr
         if(phis(i,j) .ge. phis2(i,j)) then
c        if(phis(i,j) .ge. ph2(i,plev+1)) then
C Interpolation
C  --- searching ...
            do k=plev,1,-1
               if(phis(i,j) .le. ph2(i,k) .and.
     &            phis(i,j) .ge. ph2(i,k+1)) then

c                    dlp = pe2(i,k+1,j)-pe2(i,k,j)
c                    beta= (ph2(i,k)-phis(i,j)) / (ph2(i,k)-ph2(i,k+1))
c                 ps(i,j) = pe2(i,k,j) + dlp*beta

                     dlp = pk2(i,k+1)-pk2(i,k)
                     beta= (ph2(i,k)-phis(i,j)) / (ph2(i,k)-ph2(i,k+1))
                  r8tmp = pk2(i,k) + dlp*beta
                  ps(i,j) = r8tmp ** (1./cappa)
C Bug...
c                 ps(i,j) = r8tmp ** rkap

C Safety check
                  if(ps(i,j) .gt. ps2(i,j)) Then
                     write(6,*) k, 'Interpolation failed'
                     write(6,*) i,j, ps2(i,j), phis2(i,j)
                     write(6,*) ps(i,j), phis(i,j), dlp, beta
                     stop
                  endif
                  go to 123
               endif
            enddo
123         continue
         else
C Extrapolation !!!
            r8tmp = pk2(i,plev+1) + (phis2(i,j)-phis(i,j)) /
     *                           (cpair*t2(i,plev))
            ps(i,j) = r8tmp ** (1./cappa)
c           ps(i,j) = r8tmp ** rkap

         endif
      enddo

        do i = 1, imr
          pe1(i,   1) =  ptop
          pe2(i,   1,j) =  ptop
          pe1(i,nl+1) =  ps(i,j)
        enddo

      do k=2,nl
         do i=1,imr
            pe1(i,k) = ak(k) + ps(i,j)*bk(k)
         enddo
      enddo

      do k=1,nl
         do i=1,imr
              dp1(i,k) = pe1(i,k+1) - pe1(i,k)
           delp(i,j,k) = dp1(i,k)
         enddo
      enddo

c****6***0*********0*********0*********0*********0*********0**********72
C map u
c****6***0*********0*********0*********0*********0*********0**********72

      call mappm(plev, pe2(1,1,j), dp2, u2, nl, pe1, dp1, q_new,
     &           imr, 1, 3)

      do k=1,nl
         do i=1,imr
            u(i,j,k) = q_new(i,k)
         enddo
      enddo

c****6***0*********0*********0*********0*********0*********0**********72
C map v
c****6***0*********0*********0*********0*********0*********0**********72

      call mappm(plev, pe2(1,1,j), dp2, v2, nl, pe1, dp1, q_new,
     &           imr, 1, 3)

      do k=1,nl
         do i=1,imr
            v(i,j,k) = q_new(i,k)
         enddo
      enddo

c****6***0*********0*********0*********0*********0*********0**********72
C map water vapor
c****6***0*********0*********0*********0*********0*********0**********72

      call mappm(plev, pe2(1,1,j), dp2, q2, nl, pe1, dp1, q_new,
     &           imr, 0, 5)

      do k=1,nl
         do i=1,imr
            q(i,j,k,1) = q_new(i,k)
         enddo
      enddo


c****6***0*********0*********0*********0*********0*********0**********72
C map pt using geopotential conserving scheme.
c****6***0*********0*********0*********0*********0*********0**********72

      do k=1,plev
         do i=1,imr
            dp2(i,k) = pk2(i,k+1) - pk2(i,k)
         enddo
      enddo

      do k=1,nl+1
         do i=1,imr
            pe1(i,k) = pe1(i,k) ** cappa
         enddo
      enddo

      do k=1,nl
         do i=1,imr
            dp1(i,k) = pe1(i,k+1) - pe1(i,k)
         enddo
      enddo

      call mappm(plev, pk2, dp2, t2, nl, pe1, dp1, q_new,
     &           imr, 1, 3)

C Do Dry convective adjustment here...

      do k=1,nl
         do i=1,imr
            pt(i,j,k) = q_new(i,k)
         enddo
      enddo

1000  continue

      write(6,*) ' '
      write(6,*) 'Checking PS for fvcore'
      pmax = vmax(ps,pmin,imr*jnp)
      write(6,*) 'max PS (mb)=', pmax/100., ' min=', pmin/100.

      pmax = vmax(u,pmin,imr*jnp*nl)
      write(6,*) 'max U=', pmax, ' min=', pmin

      pmax = vmax(v,pmin,imr*jnp*nl)
      write(6,*) 'max V=', pmax, ' min=', pmin

      pmax = vmax(pt,pmin,imr*jnp*nl)
      write(6,*) 'max PT=', pmax, ' min=', pmin

      pmax = vmax(q,pmin,imr*jnp*nl)
      write(6,*) 'max Q=', pmax, ' min=', pmin

C A to D transform...
      do k=1, nl
         call atod(u(1,1,k),v(1,1,k),ud,vd,imr,jnp)
      do j=1,jnp
         do i=1,imr
            u(i,j,k) = ud(i,j)
            v(i,j,k) = vd(i,j)
         enddo
      enddo

      enddo


      pmax = vmax(u,pmin,imr*jnp*nl)
      write(6,*) 'max U=', pmax, ' min=', pmin

      pmax = vmax(v,pmin,imr*jnp*nl)
      write(6,*) 'max V=', pmax, ' min=', pmin


C write IC for fvgcm
C 8-BYTE
      print *, 'Number of levels is ', nl
      hs = hs8
      print *
      if ( ftype .eq. RESTART ) then
	   print *, ' [] writing RESTRAT file ', trim(ofname)
           open(iuic,file=ofname,form='unformatted')
	   write(iuic) nstep, nymd, nhms
	   write(iuic) ps,delp,u,v,pt
	   write(iuic) q
           close(iuic)
      else if ( ftype .eq. GRADS ) then
	   print *, ' [] writing GRADS file ', trim(ofname)
           open(iuic,file=ofname,form='unformatted')
           call write_grads ( iuic, hs, imr, jnp, 1 )
           call write_grads ( iuic, ps, imr, jnp, 1 )
           call write_grads ( iuic, delp, imr, jnp, nl )
           call write_grads ( iuic, u, imr, jnp, nl )
           call write_grads ( iuic, v, imr, jnp, nl )
           call write_grads ( iuic, pt, imr, jnp, nl )
           call write_grads ( iuic, q, imr, jnp, nl )
           close(iuic)

      else if ( ftype .eq. GFIO ) then

	   print *, ' [] writing GFIO file ', trim(ofname)

           lwi(:,:) = 0.0          ! aqua-planet for now.

           call Dyn_Init ( imr, jnp, nl, 1, ptop, ks, ak, bk, 
     &                     phis, stdv, Ts, lwi, ps,           
     &                     delp, u, v, pt, q, w_f, rc         )
           if ( rc .ne. 0 ) then
                print *, 'geos2fv: could not initialize w_f'
                stop
           else
                print *, 'geos2fv: initialized w_f'
           end if

           print *, 'geos2fv: about to put...'
           if ( prec .eq. 32 ) prec = 0
           if ( prec .eq. 64 ) prec = 1
           call dyn_put ( ofname, nymd, nhms, prec, w_f, rc, 
     &                    nstep=nstep, verbose=.true. )
           if ( rc .ne. 0 ) then
                print *, 'geos2fv: cannot write w_f ', rc
                stop
           end if

      else
           print *, 'Oops, cannot write output file type ', ftype
           stop
      endif

      stop
      end

      subroutine get_d_rst(im, jm, km, hs, ps, u, v, t, q, pe)

      implicit none

      integer im
      integer jm
      integer km
      integer i,j,k

      integer in
      integer jn
      parameter(in=288, jn=181)

      real hs(im,jm)		! surface geopotential (m/sec)**2
      real ps(im,jm)		! surface pressure (mb)

      real u(im,jm,km)	! U-Wind (m/sec)
      real v(im,jm,km)	! V-Wind (m/sec)
      real t(im,jm,km)	! temperature (K)
      real q(im,jm,km)	! specific humidity (g/kg)

      real pe(im,km+1,jm)	! Pressure mb

      real phis(in,jn)		! surface geopotential (m/sec)**2
      real ps2(in,jn)		! surface pressure (mb)

      real u2(in,jn,km)	! U-Wind (m/sec)
      real v2(in,jn,km)	! V-Wind (m/sec)
      real t2(in,jn,km)	! temperature (K)
      real q2(in,jn,km)	! specific humidity (g/kg)
      real coslon(in), sinlon(in)

      real ptop, pint
      real dl, pi
      real ak(km+1), bk(km+1)
      integer nstep, nymd, nhms
      integer ks

      pi  = 4.0 * atan(1.0)
      dl  = (pi+pi)/float(in)

      do i=1,in/2
         coslon(i)       = -cos((i-1)*dl)
         coslon(i+in/2) = -coslon(i)
         sinlon(i)       = -sin((i-1)*dl)
         sinlon(i+in/2) = -sinlon(i)
      enddo

C Set up coord
      call set_eta(km,KS,PTOP,PINT,ak,bk)

C Read in phis
      read(70) phis
C Read d-rst
      read(71) nstep, nymd, nhms
      read(71) ps2, u2, u2, v2, t2
      read(71) q2

      if(in .ne. im) then
C Interpolate all fields, including phis, to finer grid.
C phis
      call lo2hi(phis, hs, in, jn, im, jm, 0)
      call lo2hi( ps2, ps, in, jn, im, jm, 0)

      do 2000 k=1, km
C t
      call lo2hi(t2(1,1,k), t(1,1,k), in, jn, im, jm, 0)
C q
      call lo2hi(q2(1,1,k), q(1,1,k), in, jn, im, jm, 0)

C D -> A
      call d2a(u2(1,1,k),v2(1,1,k),u2(1,1,k),v2(1,1,k),
     &         in, jn, coslon, sinlon)
C u
      call lo2hi(u2(1,1,k), u(1,1,k), in, jn, im, jm, 1)
C v
      call lo2hi(v2(1,1,k), v(1,1,k), in, jn, im, jm, 1)

2000  continue

      else
C Copying ....
         do j=1, jm
            do i=1, im
               ps(i,j) = ps2(i,j)
               hs(i,j) = phis(i,j)
            enddo
         enddo

      do k=1, km
         do j=1, jm
            do i=1, im
               t(i,j,k) = t2(i,j,k)
               q(i,j,k) = q2(i,j,k)
               u(i,j,k) = u2(i,j,k)
               v(i,j,k) = v2(i,j,k)
            enddo
         enddo
      enddo
      endif

C Recompute pe
         do j=1, jm
            do i=1, im
               pe(i,1,j) = ptop
               pe(i,km+1,j) = ps(i,j)
            enddo
         enddo

      do k=2,km
         do j=1, jm
            do i=1, im
               pe(i,k,j) = ak(k) + bk(k)*ps(i,j)
            enddo
         enddo
      enddo

      return
      end

!........................................................................

      subroutine get_geos2 ( nymd, nhms, im, jm, km, 
     &                       hs, ps, u, v, t, q, pe)

      implicit none

      include 'geos2fv.h'       ! Input file name comes from here

      integer nymd, nhms
      integer im
      integer jm
      integer km
      integer i,j,k

      real hs(im,jm)		! surface geopotential (m/sec)**2
      real ps(im,jm)		! surface pressure (mb)
c     real slp(im,jm)		! sea level pressure (mb)

      real u(im,jm,km)	! U-Wind (m/sec)
      real v(im,jm,km)	! V-Wind (m/sec)
c     real h(im,jm,km)	! pertabation geopotential height (m)
      real t(im,jm,km)	! temperature (K)
      real q(im,jm,km)	! specific humidity (g/kg)

      real pe(im,km+1,jm)	! Pressure mb

C GEOS-2 DAS Data

      integer in
      integer jn

!ams      parameter(in=144, jn=91)
!ams      real phis(in,jn)		! surface geopotential (m/sec)**2
!ams      real ps2(in,jn)		! surface pressure (mb)
!ams      real u2(in,jn,km)	        ! U-Wind (m/sec)
!ams      real v2(in,jn,km)	        ! V-Wind (m/sec)
!ams      real t2(in,jn,km)	        ! temperature (K)
!ams      real q2(in,jn,km)	        ! specific humidity (g/kg)
!ams      real rh2(in,jn,km)            ! relative humidity (g/kg)

      real, allocatable ::  phis(:,:)	! surface geopotential (m/sec)**2
      real, allocatable ::  ps2(:,:)	! surface pressure (mb)
      real, allocatable ::  u2(:,:,:)   ! U-Wind (m/sec)
      real, allocatable ::  v2(:,:,:)	! V-Wind (m/sec)
      real, allocatable ::  t2(:,:,:)	! temperature (K)
      real, allocatable ::  q2(:,:,:)	! specific humidity (g/kg)
      real, allocatable ::  rh2(:,:,:)  ! relative humidity (g/kg)

      real, allocatable ::  sige(:)
      real ptop

      logical filter
      data filter /.false./
      integer ndeg

!!!      real pmin, pmax, vmax

       integer ier, imf, jmf, kmf


C Get geos-2 das data
C Sharon's version
c     call readprog(in, jn, km, phis, ps2, u2, v2, t2, q2,
c    &              ptop, sige)
C Arlindo's version
c      call readieee(in, jn, km, phis, ps2, u2, v2, t2, q2,
c     &              ptop, sige)


!     Read PROG/SIG files written by GFIO
!     -----------------------------------
      call ProgSig_Dim ( trim(ifname), imf, jmf, kmf, ier )
      if ( ier .ne. 0 ) then
         print *, 'get_geos2: cannot get dims from ', trim(ifname)
         call exit(7)
         stop
      else if ( km .ne. kmf ) then
         print *, 'get_geos2: imcompatible vertical dimensions'
         print *, 'get_geos2: km on file: ', kmf
         print *, 'get_geos2: km in here: ', km
         call exit(7)
         stop       
      end if

!     Allocate necessary memory
!     -------------------------
      in = imf; jn = jmf
      allocate ( sige(km+1), phis(in,jn), ps2(in,jn), 
     &            u2(in,jn,km), v2(in,jn,km), t2(in,jn,km), 
     &            q2(in,jn,km), rh2(in,jn,km), stat = ier )
      if ( ier .ne. 0 ) then
         print *, 'get_geos2: cannot allocate memory '
         call exit(7)
         stop
      end if

!     OK, just read the data. Note: ps, ptop in Pa, sphi in kg/kg
!     -----------------------------------------------------------
      call ProgSig_Read ( trim(ifname), nymd, nhms,  
     &                    in, jn, km, ptop, sige,  
     &                    phis, ps2, u2, v2, t2, q2, rh2, ier )
      if ( ier .ne. 0 ) then
         print *, 'getgeos2: cannot read ', trim(ifname)
         call exit(7)
         stop
      end if

!     Turn fields upside down
!     -----------------------
      u2(:,:,1:km) = u2(:,:,km:1:-1)  
      v2(:,:,1:km) = v2(:,:,km:1:-1)  
      t2(:,:,1:km) = t2(:,:,km:1:-1)  
      q2(:,:,1:km) = q2(:,:,km:1:-1)  
      rh2(:,:,1:km) = rh2(:,:,km:1:-1)  

C Shift 180 deg for CCM3 phys package.

      call shiftx(phis, in, jn, 1)
      call shiftx(ps2,  in, jn, 1)

      call shiftx(u2, in, jn, km)
      call shiftx(v2, in, jn, km)
      call shiftx(t2, in, jn, km)
      call shiftx(q2, in, jn, km)

      if( in .ne. im) then
C Interpolate all fields, including phis, to finer grid.
C phis
      call lo2hi(phis, hs, in, jn, im, jm, 0)
      call lo2hi( ps2, ps, in, jn, im, jm, 0)

      do 2000 k=1, km
C t
      call lo2hi(t2(1,1,k), t(1,1,k), in, jn, im, jm, 0)
C q
      call lo2hi(q2(1,1,k), q(1,1,k), in, jn, im, jm, 0)
C u
      call lo2hi(u2(1,1,k), u(1,1,k), in, jn, im, jm, 1)
C v
      call lo2hi(v2(1,1,k), v(1,1,k), in, jn, im, jm, 1)

2000  continue

      else
C Copying ....
         do j=1, jm
            do i=1, im
               ps(i,j) = ps2(i,j) ! mb -> Pa
               hs(i,j) = phis(i,j)
            enddo
         enddo

      do k=1, km
C Debug ..
c     pmax = vmax(t2(1,1,k),pmin,in*jn)
c     write(6,*) k, ' max T=', pmax, ' min=', pmin

         do j=1, jm
            do i=1, im
               t(i,j,k) = t2(i,j,k)
               q(i,j,k) = q2(i,j,k) 
               u(i,j,k) = u2(i,j,k)
               v(i,j,k) = v2(i,j,k)
            enddo
         enddo
      enddo
      endif

      if (filter) then
      ndeg = 80

      call plft2d(ps, 2, jm-1, ndeg)
      call plft2d(hs, 2, jm-1, ndeg)

      do k=1, km
         call plft2d(u(1,1,k), 2, jm-1, ndeg)
         call plft2d(v(1,1,k), 2, jm-1, ndeg)
         call plft2d(t(1,1,k), 2, jm-1, ndeg)
         call plft2d(q(1,1,k), 2, jm-1, ndeg)
      enddo
      endif

C Recompute pe based on ps and sige
         do j=1, jm
            do i=1, im
               pe(i,1,j) = ptop 
               pe(i,km+1,j) = ps(i,j)
            enddo
         enddo

      do k=2,km
         do j=1, jm
            do i=1, im
               pe(i,k,j) = (ps(i,j)-ptop)*sige(k) + ptop
            enddo
         enddo
      enddo

!     Free local workspace
!     --------------------
      deallocate ( sige, phis, ps2, u2, v2, t2, q2, rh2 )

      return
      end

      subroutine lo2hi(qh, q, imh, jmh, im, jm, iv)
      implicit none
      integer iv

C iv=0: Scalar
C iv=1: vector

C Interpolation to finer grid in y
C Mapping in x

      integer i, j
      integer im, jm
      integer imh, jmh
      integer ih, jh
      real sum1, sum2

      real qh(imh, jmh)       ! coarse grid data
      real q (im, jm)         ! 2x grid data

C Poles:
      if( iv .eq. 0) then
C Scalars
         sum1 = 0.
         sum2 = 0.

      do i=1, imh
         sum1 = sum1 + qh(i,1)
         sum2 = sum2 + qh(i,jmh)
      enddo

         sum1 = sum1 / float(imh)
         sum2 = sum2 / float(imh)

      do i=1, im
         q(i, 1) = sum1
         q(i,jm) = sum2
      enddo

      else

C Vector:
      do i=1, im-1, 2
            ih = i/2 + 1
         q(i, 1) = qh(ih,1)
         q(i,jm) = qh(ih,jmh)
      enddo

         do i=2,im-2, 2
            q(i, 1) = 0.5*(q(i-1,1)+q(i+1,1))
            q(i,jm) = 0.5*(q(i-1,jm)+q(i+1,jm))
         enddo

C i=im
            q(im, 1) = 0.5*(q(im-1, 1)+q(1, 1))
            q(im,jm) = 0.5*(q(im-1,jm)+q(1,jm))

      endif

C PPM Mapping in E-W:

      do 2000 j=3, jm-2, 2
         jh = j/2 + 1
         call xppmap(im, imh, qh(1,jh), q(1,j))
2000  continue

C N-S Interpolation:

      do 3000 j=2, jm-1, 2
         do i=1,im
            q(i,j) = 0.5*(q(i,j-1)+q(i,j+1))
         enddo
3000  continue

      return
      end

      subroutine xppmap(im, imh, qh, q)
      implicit none

      integer im, imh
      integer i,  ih

      real qh(*)
      real  q(*)
      real  temp

      real r3
      parameter ( r3 = 1./3.)
      real r16, r56
      parameter ( r16 = 1./16., r56 = 5./6.)

C Local Auto arrays:
      real dm(imh)
      real a3(3,imh)
      real a4(4,imh)

      call mist(imh,qh,dm)

      do i=1,imh
         a4(1,i) = qh(i)
      enddo

c        i=1
         a4(2,1) = 0.5*(qh(imh)+qh(1)) + (dm(imh) - dm(1))*r3
      do i=2,imh
         a4(2,i) = 0.5*(qh(i-1)+qh(i)) + (dm(i-1) - dm(i))*r3
      enddo

      do i=1,imh-1
         a4(3,i) = a4(2,i+1)
      enddo
         a4(3,imh) = a4(2,1)

      do i=1,imh
         a4(4,i) = 3.*(a4(1,i)+a4(1,i)  - (a4(2,i)+a4(3,i)))
      enddo

      call kmppm(dm, a4, imh, 0)

      do i=1,imh
         a4(1,i) = qh(i)
         a4(2,i) = qh(i)
         a4(3,i) = qh(i)
         a4(4,i) = 0.
      enddo

      do i=1,imh
         temp = a4(2,i)+a4(3,i) + r56*a4(4,i)
C left:
         a3(1,i) = r16*(temp + 6.*a4(2,i))
C Right:
         a3(3,i) = r16*(temp + 6.*a4(3,i))
C Center:
         a3(2,i) = 2.*qh(i) - (a3(1,i)+a3(3,i))
      enddo

      do i=1,im-1,2
           ih = i/2 + 1
         q(i) = a3(2,ih)
      enddo

      do i=2,im-2,2
           ih = i/2
         q(i) = a3(3,ih) + a3(1,ih+1)
      enddo
         q(im) = a3(3,imh) + a3(1,1)

      return
      end

c****6***0*********0*********0*********0*********0*********0**********72
      subroutine mist(im,p,dm)
c****6***0*********0*********0*********0*********0*********0**********72
      implicit none

      integer im
      integer i
      real p(im)
      real dm(im)
      real pmin, pmax

C i=1
         dm(1) = 0.25*(p(2) - p(im))
      do i=2,im-1
         dm(i) = 0.25*(p(i+1) - p(i-1))
      enddo
C i=im
         dm(im) = 0.25*(p(1) - p(im-1))

c Apply monotonicity constraint (Lin et al. 1994, MWR)

	i=1
       pmax = max(p(im), p(i), p(2)) - p(i)
       pmin = p(i) - min(p(im), p(i), p(2))
       dm(i) = sign(min(abs(dm(i)),pmax,pmin), dm(i))
      do 20 i=2,im-1
       pmax = max(p(i-1), p(i), p(i+1)) - p(i)
       pmin = p(i) - min(p(i-1), p(i), p(i+1))
       dm(i) = sign(min(abs(dm(i)),pmax,pmin), dm(i))
20    continue
	i = im
       pmax = max(p(i-1), p(i), p(1)) - p(i)
       pmin = p(i) - min(p(i-1), p(i), p(1))
       dm(i) = sign(min(abs(dm(i)),pmax,pmin), dm(i))

      return
      end

      subroutine lo2hio_int(qh, q, imh, jmh, im, jm, iv)
      implicit none
      integer iv

C iv=0: Scalar
C iv=1: vector

C Linear interpolation


      integer i, j
      integer im, jm
      integer imh, jmh
      integer ih, jh
      real sum1, sum2

      real qh(imh, jmh)       ! coarse grid data
      real q (im, jm)         ! 2x grid data

C Poles:
      if( iv .eq. 0) then
C Scalars
         sum1 = 0.
         sum2 = 0.

      do i=1, imh
         sum1 = sum1 + qh(i,1)
         sum2 = sum2 + qh(i,jmh)
      enddo

         sum1 = sum1 / float(imh)
         sum2 = sum2 / float(imh)

      do i=1, im
         q(i, 1) = sum1
         q(i,jm) = sum2
      enddo

      else

C Vector:
      do i=1, im-1, 2
            ih = i/2 + 1
         q(i, 1) = qh(ih,1)
         q(i,jm) = qh(ih,jmh)
      enddo

         do i=2,im-2, 2
            q(i, 1) = 0.5*(q(i-1,1)+q(i+1,1))
            q(i,jm) = 0.5*(q(i-1,jm)+q(i+1,jm))
         enddo

C i=im
            q(im, 1) = 0.5*(q(im-1, 1)+q(1, 1))
            q(im,jm) = 0.5*(q(im-1,jm)+q(1,jm))

      endif

      do 2000 j=3, jm-2, 2
         do i=1,im-1, 2
            ih = i/2 + 1
            jh = j/2 + 1
            q(i,j) = qh(ih,jh)
         enddo

         do i=2,im-2, 2
            q(i,j) = 0.5*(q(i-1,j)+q(i+1,j))
         enddo
C i=im
            q(im,j) = 0.5*(q(im-1,j)+q(1,j))
2000  continue

      do 3000 j=2, jm-1, 2

         do i=1,im
            q(i,j) = 0.5*(q(i,j-1)+q(i,j+1))
         enddo

3000  continue

      return
      end

      subroutine readieee(imr, jnp, km, phis, ps,
     &                    uwnd, vwnd, tmpu, sphu, ptop, sge)

C This program Reads Arlindo's data
C
C Modified from original code from S. Nebuda

      implicit none

      integer imr
      integer jnp
      integer im
      integer jm
      integer km
      integer nlayr
      integer ntime
      real     ptop

      parameter (im = 144)	! number of x points
      parameter (jm = 91)	! number of y points
      parameter (nlayr = 70)	! geos-2 70 level resolution
      parameter (ntime = 1)	! number of times


c  input
      real phis(im,jm)		! surface geopotential (m/sec)**2
      real ps(im,jm)		! surface pressure (mb)
      real slp(im,jm)		! sea level pressure (mb)
      real lwi(im,jm)		! Surface Types from Land/Surface Model
      real uwnd(im,jm,nlayr)	! U-Wind (m/sec)
      real vwnd(im,jm,nlayr)	! V-Wind (m/sec)
c     real hghtp(im,jm,nlayr)	! pertabation geopotential height (m)
      real tmpu(im,jm,nlayr)	! temperature (K)
      real sphu(im,jm,nlayr)	! specific humidity (g/kg)
c     real rh(im,jm,nlayr)	! relative humidity (%)


c  unique to the routine
!!!      character*60 filein	! filename of input
      include 'geos2fv.h'
      integer i,j,k		! loop variables
!!!      integer nri		! record number of input and output file
      real sig(nlayr)		! sigma levels of data
      real sge(nlayr+1)	! edge sigma levels of data
      real sige(nlayr+1)	! edge sigma levels of data
      real fac
      real wk(im,jm)
!!!      real pmin, pmax, vmax
      integer iu
      integer irec

c--------------- Grads input file ---------------------------

C  DSET    /monthlyb/nebuda/e0523.progsig.t960917.data
C  FORMAT SEQUENTIAL
C  TITLE  e0523d    Control assimilation for NSCAT
C  UNDEF  0.100000E+16
C  XDEF  144 LINEAR  -180.0    2.50000000
C  YDEF   91 LINEAR   -90.0    2.00000000
C  ZDEF   70 LEVELS    70.00   69.00   68.00   67.00   66.00
C                      65.00   64.00   63.00   62.00   61.00
C                      60.00   59.00   58.00   57.00   56.00
C                      55.00   54.00   53.00   52.00   51.00
C                      50.00   49.00   48.00   47.00   46.00
C                      45.00   44.00   43.00   42.00   41.00
C                      40.00   39.00   38.00   37.00   36.00
C                      35.00   34.00   33.00   32.00   31.00
C                      30.00   29.00   28.00   27.00   26.00
C                      25.00   24.00   23.00   22.00   21.00
C                      20.00   19.00   18.00   17.00   16.00
C                      15.00   14.00   13.00   12.00   11.00
C                      10.00    9.00    8.00    7.00    6.00
C                       5.00    4.00    3.00    2.00    1.00
C  TDEF     1 LINEAR  00:00Z17SEP96   6hr
C  VARS   10
C  PHIS        0   0  Surface Geopotential Heights (m/sec)**2
C  PS          0   0  Surface Pressure (mb)
C  SLP         0   0  Sea Level Pressure (mb)
C  LWI         0   0  Surface Types from Land/Surface Model
C  UWND       70   0  U-Wind (m/sec)
C  VWND       70   0  V-Wind (m/sec)
C  HGHTP      70   0  Perturbation Geopotential Height (m)
C  TMPU       70   0  Temperature (K)
C  SPHU       70   0  Specific Humidity (g/kg)
C  RH         70   0  Relative Humidity (Percent)
C  ENDVARS


c--------------- Variable Initialization ------------------------

      data sige /
     .      0.000000E+00 , 6.016350E-06 , 1.370820E-05 , 2.295240E-05 ,
     .      3.401840E-05 , 4.802090E-05 , 6.594130E-05 , 8.865010E-05 ,
     .      1.177850E-04 , 1.550160E-04 , 2.024040E-04 , 2.624830E-04 ,
     .      3.383480E-04 , 4.337660E-04 , 5.532970E-04 , 7.024390E-04 ,
     .      8.877800E-04 , 1.118005E-03 , 1.403066E-03 , 1.754897E-03 ,
     .      2.187742E-03 , 2.718547E-03 , 3.367386E-03 , 4.157950E-03 ,
     .      5.118086E-03 , 6.280394E-03 , 7.682883E-03 , 9.369687E-03 ,
     .      1.139184E-02 , 1.380810E-02 , 1.668584E-02 , 2.010197E-02 ,
     .      2.414387E-02 , 2.891042E-02 , 3.451000E-02 , 4.107631E-02 ,
     .      4.872000E-02 , 5.757372E-02 , 6.781108E-02 , 7.963646E-02 ,
     .      9.325208E-02 , 1.088781E-01 , 1.267550E-01 , 1.471300E-01 ,
     .      1.703000E-01 , 1.965500E-01 , 2.262500E-01 , 2.595500E-01 ,
     .      2.962800E-01 , 3.360700E-01 , 3.784500E-01 , 4.230300E-01 ,
     .      4.694000E-01 , 5.170351E-01 , 5.654188E-01 , 6.138495E-01 ,
     .      6.615992E-01 , 7.078959E-01 , 7.519437E-01 , 7.929700E-01 ,
     .      8.303000E-01 , 8.635700E-01 , 8.927438E-01 , 9.179000E-01 ,
     .      9.392300E-01 , 9.570000E-01 , 9.715000E-01 , 9.829000E-01 ,
     .      9.914000E-01 , 9.970951E-01 , 1.000000E+00 /

      if(im .ne. imr) then
         write(6,*) 'Stop!', im,  imr
         stop
      endif
      if(km .ne. nlayr) then
         write(6,*) 'Stop!', km,  nlayr
         stop
      endif

C Arlindo's dataset
!!!      filein  = 'uth01.fg.t19971221.dat'
C In pascal!!!
      ptop = 1.	    ! model top pressure level

!      Read the data for t=1
!      ---------------------
       iu = 20
       open(iu,file=ifname,form='unformatted',
     &      access='direct',recl=im*jm*4)

       irec = 0

       call read_grads ( iu, phis, im, jm, 1,  irec )

       call read_grads ( iu,  ps,   im, jm, 1,  irec )

       call read_grads ( iu, slp,  im, jm, 1,  irec )

       call read_grads ( iu, lwi,  im, jm, 1,  irec )

       do k = nlayr, 1, -1
       call read_grads ( iu, wk, im, jm, 1, irec )
        do j=1, jm
           do i=1,im
              uwnd(i,j,k) = wk(i,j)
           enddo
        enddo
       enddo

       do k = nlayr, 1, -1
       call read_grads ( iu, wk, im, jm, 1, irec )
        do j=1, jm
           do i=1,im
              vwnd(i,j,k) = wk(i,j)
           enddo
        enddo
       enddo

       do k = nlayr, 1, -1
       call read_grads ( iu, wk, im, jm, 1, irec )
        do j=1, jm
           do i=1,im
              tmpu(i,j,k) = wk(i,j)
           enddo
        enddo
       enddo

       do k = nlayr, 1, -1
       call read_grads ( iu, wk, im, jm, 1, irec )
        do j=1, jm
           do i=1,im
              sphu(i,j,k) = wk(i,j)
           enddo
        enddo
       enddo

       do k = nlayr, 1, -1
       call read_grads ( iu, wk, im, jm, 1, irec )
        do j=1, jm
           do i=1,im
c             rh(i,j,k) = wk(i,j)
           enddo
        enddo
       enddo

c------------- Compute New output ---------------------------------------

      do k = 1, nlayr+1
        sge(k) = sige(k)
      enddo

      do k = 1, nlayr
        sig(k) = (sige(k) + sige(k+1)) * 0.5
C	write(6,*) k, sig(k)
      enddo

C Convert mb to pascal
	fac = 100.

        do j = 1, jm
        do i = 1, im
          ps(i,j) = ps(i,j) * fac
        enddo
        enddo

        do k = 1, nlayr
        do j = 1, jm
        do i = 1, im
          sphu(i,j,k) =  0.001*sphu(i,j,k)
        enddo
        enddo
        enddo

c     enddo	! time loop

      close(iu)

      return
      end

      subroutine read_grads ( iu, a, im, jm, km, irec )

       real     a(im,jm,km)
       real*4   buffer(im,jm)

       do k = 1, km
          irec = irec + 1
          read(iu,rec=irec) buffer
          do j = 1, jm
	     do i = 1, im
	        a(i,j,k) = buffer(i,j)
             end do
          end do
       end do

       return
       end

      subroutine write_grads ( iu, a, im, jm, km )

       real     a(im,jm,km)
       real*4   buffer(im,jm)

       do k = 1, km
          do j = 1, jm
	     do i = 1, im
	        buffer(i,j) = a(i,j,k) 
             end do
          end do
          write(iu) buffer
       end do

       return
       end


      subroutine readprog(imr, jnp, km, phis, ps,
     &                    uwnd, vwnd, tmpu, sphu, ptop, sge)
C
C Modified from original code from S. Nebuda

      implicit none

      integer imr
      integer jnp
      integer im
      integer jm
      integer km
      integer nlayr
      integer ntime
      real     ptop

      parameter (im = 144)	! number of x points
      parameter (jm = 91)	! number of y points
      parameter (nlayr = 70)	! geos-2 70 level resolution
      parameter (ntime = 1)	! number of times


c  input
      real phis(im,jm)		! surface geopotential (m/sec)**2
      real ps(im,jm)		! surface pressure (mb)
      real slp(im,jm)		! sea level pressure (mb)
c     real lwi(im,jm)		! Surface Types from Land/Surface Model
      real uwnd(im,jm,nlayr)	! U-Wind (m/sec)
      real vwnd(im,jm,nlayr)	! V-Wind (m/sec)
c     real hghtp(im,jm,nlayr)	! pertabation geopotential height (m)
      real tmpu(im,jm,nlayr)	! temperature (K)
      real sphu(im,jm,nlayr)	! specific humidity (g/kg)
c     real rh(im,jm,nlayr)	! relative humidity (%)


c  unique to the routine
      character*60 filein	! filename of input
      integer i,j,k,n		! loop variables
      integer nri		! record number of input and output file
      real sig(nlayr)		! sigma levels of data
      real sge(nlayr+1)	! edge sigma levels of data
      real sige(nlayr+1)	! edge sigma levels of data
      real fac
      real*4 r4a(im,jm)
      real pmin, pmax, vmax
c--------------- Grads input file ---------------------------

C  DSET    /monthlyb/nebuda/e0523.progsig.t960917.data
C  FORMAT SEQUENTIAL
C  TITLE  e0523d    Control assimilation for NSCAT
C  UNDEF  0.100000E+16
C  XDEF  144 LINEAR  -180.0    2.50000000
C  YDEF   91 LINEAR   -90.0    2.00000000
C  ZDEF   70 LEVELS    70.00   69.00   68.00   67.00   66.00
C                      65.00   64.00   63.00   62.00   61.00
C                      60.00   59.00   58.00   57.00   56.00
C                      55.00   54.00   53.00   52.00   51.00
C                      50.00   49.00   48.00   47.00   46.00
C                      45.00   44.00   43.00   42.00   41.00
C                      40.00   39.00   38.00   37.00   36.00
C                      35.00   34.00   33.00   32.00   31.00
C                      30.00   29.00   28.00   27.00   26.00
C                      25.00   24.00   23.00   22.00   21.00
C                      20.00   19.00   18.00   17.00   16.00
C                      15.00   14.00   13.00   12.00   11.00
C                      10.00    9.00    8.00    7.00    6.00
C                       5.00    4.00    3.00    2.00    1.00
C  TDEF     1 LINEAR  00:00Z17SEP96   6hr
C  VARS   10
C  PHIS        0   0  Surface Geopotential Heights (m/sec)**2
C  PS          0   0  Surface Pressure (mb)
C  SLP         0   0  Sea Level Pressure (mb)
C  LWI         0   0  Surface Types from Land/Surface Model
C  UWND       70   0  U-Wind (m/sec)
C  VWND       70   0  V-Wind (m/sec)
C  HGHTP      70   0  Perturbation Geopotential Height (m)
C  TMPU       70   0  Temperature (K)
C  SPHU       70   0  Specific Humidity (g/kg)
C  RH         70   0  Relative Humidity (Percent)
C  ENDVARS


c--------------- Variable Initialization ------------------------

      data sige /
     .      0.000000E+00 , 6.016350E-06 , 1.370820E-05 , 2.295240E-05 ,
     .      3.401840E-05 , 4.802090E-05 , 6.594130E-05 , 8.865010E-05 ,
     .      1.177850E-04 , 1.550160E-04 , 2.024040E-04 , 2.624830E-04 ,
     .      3.383480E-04 , 4.337660E-04 , 5.532970E-04 , 7.024390E-04 ,
     .      8.877800E-04 , 1.118005E-03 , 1.403066E-03 , 1.754897E-03 ,
     .      2.187742E-03 , 2.718547E-03 , 3.367386E-03 , 4.157950E-03 ,
     .      5.118086E-03 , 6.280394E-03 , 7.682883E-03 , 9.369687E-03 ,
     .      1.139184E-02 , 1.380810E-02 , 1.668584E-02 , 2.010197E-02 ,
     .      2.414387E-02 , 2.891042E-02 , 3.451000E-02 , 4.107631E-02 ,
     .      4.872000E-02 , 5.757372E-02 , 6.781108E-02 , 7.963646E-02 ,
     .      9.325208E-02 , 1.088781E-01 , 1.267550E-01 , 1.471300E-01 ,
     .      1.703000E-01 , 1.965500E-01 , 2.262500E-01 , 2.595500E-01 ,
     .      2.962800E-01 , 3.360700E-01 , 3.784500E-01 , 4.230300E-01 ,
     .      4.694000E-01 , 5.170351E-01 , 5.654188E-01 , 6.138495E-01 ,
     .      6.615992E-01 , 7.078959E-01 , 7.519437E-01 , 7.929700E-01 ,
     .      8.303000E-01 , 8.635700E-01 , 8.927438E-01 , 9.179000E-01 ,
     .      9.392300E-01 , 9.570000E-01 , 9.715000E-01 , 9.829000E-01 ,
     .      9.914000E-01 , 9.970951E-01 , 1.000000E+00 /

      if(im .ne. imr) then
         write(6,*) 'Stop!', im,  imr
         stop
      endif
      if(km .ne. nlayr) then
         write(6,*) 'Stop!', km,  nlayr
         stop
      endif

C Set #1
c     filein  = 'e0523.progsig.t960917.data'
C Set #2
c     filein  = 'e0523.progsig.t961020.data'
c Set #3
c     filein  = 'e0523.progsig.t961025.data'
c Set #4
c     filein  = 'e0523d.progsig.t961028.data'
C Set #5  (NSCAT)
c     filein  = 'e0543d.progsig.t961020.data'
c Set #6 (NSCAT)
c     filein  = 'e0543d.progsig.t961028.data'
C
C Arlindo's dataset
      filein  = 'uth01.fg.t19971221.dat'


c--------------- Open Files -------------------------------------

C In pascal!!!
      ptop = 1.	    ! model top pressure level

      open(10, file=filein, form='unformatted', access='direct',
     .     recl=im*jm*4)
c    .     recl=im*jm)


c--------------- Step through time --------------------------------

      nri = 0
      do n = 1, ntime

c---------------- Read Input ---------------------------------------
C  PHIS        0   0  Surface Geopotential Heights (m/sec)**2
C  PS          0   0  Surface Pressure (mb)
C  SLP         0   0  Sea Level Pressure (mb)
C  LWI         0   0  Surface Types from Land/Surface Model
C  UWND       70   0  U-Wind (m/sec)
C  VWND       70   0  V-Wind (m/sec)
C  HGHTP      70   0  Perturbation Geopotential Height (m)
C  TMPU       70   0  Temperature (K)
C  SPHU       70   0  Specific Humidity (g/kg)
C  RH         70   0  Relative Humidity (Percent)

        nri = nri + 1
c       read(10,rec=nri) phis
        read(10,rec=nri) r4a
        do j=1, jm
           do i=1,im
              phis(i,j) = r4a(i,j)
           enddo
        enddo

        nri = nri + 1

c       read(10,rec=nri) ps
        read(10,rec=nri) r4a
        do j=1, jm
           do i=1,im
              ps(i,j) = r4a(i,j)
           enddo
        enddo
        nri = nri + 1

c       read(10,rec=nri) slp
        read(10,rec=nri) r4a
        do j=1, jm
           do i=1,im
              slp(i,j) = r4a(i,j)
           enddo
        enddo
        nri = nri + 1

c       read(10,rec=nri) lwi
        read(10,rec=nri) r4a
        do j=1, jm
           do i=1,im
c             lwi(i,j) = r4a(i,j)
           enddo
        enddo

        do k = nlayr, 1, -1
        nri = nri + 1
c       read(10,rec=nri) ((uwnd(i,j,k),i=1,im),j=1,jm)
        read(10,rec=nri) r4a
        do j=1, jm
           do i=1,im
              uwnd(i,j,k) = r4a(i,j)
           enddo
        enddo
C DEbug
c     pmax = vmax(uwnd(1,1,k),pmin,im*jm)
c     write(6,*) k, ' max U=', pmax, ' min=', pmin
        enddo

        do k = nlayr, 1, -1
        nri = nri + 1
c       read(10,rec=nri) ((vwnd(i,j,k),i=1,im),j=1,jm)
        read(10,rec=nri) r4a
        do j=1, jm
           do i=1,im
              vwnd(i,j,k) = r4a(i,j)
           enddo
        enddo
        enddo

        do k = nlayr, 1, -1
        nri = nri + 1
c       read(10,rec=nri) ((hghtp(i,j,k),i=1,im),j=1,jm)
C The following line must be commented out for Arlindo's dataset
c       read(10,rec=nri) r4a
        do j=1, jm
           do i=1,im
c             hghtp(i,j,k) = r4a(i,j)
           enddo
        enddo
        enddo

        do k = nlayr, 1, -1
        nri = nri + 1
c       read(10,rec=nri) ((tmpu(i,j,k),i=1,im),j=1,jm)
        read(10,rec=nri) r4a
        do j=1, jm
           do i=1,im
              tmpu(i,j,k) = r4a(i,j)
           enddo
        enddo
C DEbug
c     pmax = vmax(tmpu(1,1,k),pmin,im*jm)
c     write(6,*) k, ' max T=', pmax, ' min=', pmin
        enddo

        do k = nlayr, 1, -1
        nri = nri + 1
c       read(10,rec=nri) ((sphu(i,j,k),i=1,im),j=1,jm)
        read(10,rec=nri) r4a
        do j=1, jm
           do i=1,im
              sphu(i,j,k) = r4a(i,j)
           enddo
        enddo
        enddo

        do k = nlayr, 1, -1
        nri = nri + 1
c       read(10,rec=nri) ((rh(i,j,k),i=1,im),j=1,jm)
        read(10,rec=nri) r4a
        do j=1, jm
           do i=1,im
c             rh(i,j,k) = r4a(i,j)
           enddo
        enddo
        enddo

c------------- Compute New output ---------------------------------------

      do k = 1, nlayr+1
        sge(k) = sige(k)
      enddo

      do k = 1, nlayr
        sig(k) = (sige(k) + sige(k+1)) * 0.5
C	write(6,*) k, sig(k)
      enddo

C Convert mb to pascal
	fac = 100.

        do j = 1, jm
        do i = 1, im
          ps(i,j) = ps(i,j) * fac
        enddo
        enddo

        do k = 1, nlayr
        do j = 1, jm
        do i = 1, im
          sphu(i,j,k) =  0.001*sphu(i,j,k)
        enddo
        enddo
        enddo

      enddo	! time loop

      close(10)

      return
      end

      real function vmax(a,pmin,n)
      implicit none
      integer n, i
      real pmin, pmax
      real a(*)
      pmax =a(n)
      pmin =a(n)
      do 10 i=1,n-1
      pmax = max(pmax,a(i))
      pmin = min(pmin,a(i))
10    continue
      vmax = pmax
      return
      end

      subroutine shiftx(a, im, jm, km)

C Shift the position of i=1 from the international dateline to
C Greenwich.

C im must be even integer

      implicit none
      integer imax
      parameter (imax = 720)
      integer i, j, k, im, jm, km
      integer shift

      real a(im,jm,*), a1(imax)

      if(imax .lt. im) then
         write(6,*) 'Increase imax in shiftx'
         stop
      endif

      shift = im/2

      if(shift .ne. (shift/2)*2) then
         write(6,*) 'im in shift is NOT an even integer'
         stop
      endif

      do 100 k=1,km
        do 50 j=1,jm
        do i=1,im
           if(i .le. shift) then
                a1(i+shift) = a(i,j,k)
           else
                a1(i-shift) = a(i,j,k)
           endif
        enddo

C Copy back to A

        do i=1,im
           a(i,j,k) = a1(i)
        enddo
50      continue
100     continue

        return
        end

      subroutine set_32(km,KS,PTOP,PINT,ak,bk)

C A and B smoothed by R. Swinbank

      implicit none
      integer NL, KS, k, km
      parameter (NL = 32)
      real ak(km+1),bk(km+1)
      real a(NL+1),b(NL+1),PRESS(NL+1)
      real ptop, pint, p0, dp

      data a /300.0000,  454.1491,  652.5746,  891.9637, 1159.7102,
     &       1492.8248, 1902.5026, 2400.4835, 2998.6740, 3708.6584,
     &       4541.1041, 5505.0739, 6607.2675, 7851.2298, 9236.5661,
     &      10866.3427, 12420.400, 13576.500, 14365.400, 14807.800,
     &       14915.500, 14691.400, 14129.400, 13214.800, 11923.200,
     &       10220.700,  8062.000,  5849.500,  3777.000,  2017.200,
     &         720.600,     0.000,     0.000 /

      data b /0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
     &        0.0000000, 0.003633 , 0.014628 , 0.033276 , 0.060071 ,
     &        0.095722 , 0.141171 , 0.197623 , 0.266571 , 0.349839 ,
     &        0.449632 , 0.568589 , 0.685887 , 0.793252 , 0.883128 ,
     &        0.948792 , 0.9851119, 1.0000000 /

      if( km .ne. nl) then
          write(6,*) 'Stop in set_eta(km=32)'
          stop
      endif

      KS = 15
      PTOP =  a(1)
      PINT = a(KS+1)

      do k=1,NL+1
         ak(k) = a(k)
         bk(k) = b(k)
      enddo

      return
      end
c****6***0*********0*********0*********0*********0*********0**********72
      subroutine set_eta(km,KS,PTOP,PINT,ak,bk)
c****6***0*********0*********0*********0*********0*********0**********72
      implicit none
      integer NL, KS, k, km
      parameter (NL = 55)
      real ak(km+1),bk(km+1)
      real a(NL+1),b(NL+1),PRESS(NL+1)
      real ptop, pint, p0, dp

      data a /1.0000,    2.0000,    3.2700,    4.7585,     6.6000,
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

      data b /                      0.0000000, 0.0000000, 0.0000000,
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

      if( km .ne. nl) then
          write(6,*) 'Stop in set_eta(km=55)', km, nl
          stop
      endif

      KS = 38
      PTOP =  a(1)
      PINT = a(KS+1)

      do k=1,NL+1
         ak(k) = a(k)
         bk(k) = b(k)
      enddo

      return
      end


C****6***0*********0*********0*********0*********0*********0**********72
      subroutine mappm(km, pe1, dp1, q1, kn, pe2, dp2, q2, im, iv, kord)
C****6***0*********0*********0*********0*********0*********0**********72
C IV = 0: constituents
C
C Mass flux preserving mapping: q1(im,km) -> q2(im,kn)
C
C pe1: pressure at layer edges (from model top to bottom surface)
C      in the original vertical coordinate
C pe2: pressure at layer edges (from model top to bottom surface)
C      in the new vertical coordinate

      parameter (kmax = 200)
      parameter (R3 = 1./3., R23 = 2./3.)

      real dp1(im,km),   dp2(im,kn),
     &      q1(im,km),    q2(im,kn),
     &     pe1(im,km+1), pe2(im,kn+1)
      integer kord

C local work arrays
      real a4(4,im,km)

      do k=1,km
         do i=1,im
            a4(1,i,k) = q1(i,k)
         enddo
      enddo

      call ppm2m(a4, dp1, im, km, iv, kord)

C Lowest layer: constant distribution
      do i=1, im
         a4(2,i,km) = q1(i,km)
         a4(3,i,km) = q1(i,km)
         a4(4,i,km) = 0.
      enddo

      do 5555 i=1,im
         k0 = 1
      do 555 k=1,kn

         if(pe2(i,k+1) .le. pe1(i,1)) then
C Entire grid above old ptop
            q2(i,k) = a4(2,i,1)
         elseif(pe2(i,k) .ge. pe1(i,km+1)) then
C Entire grid below old ps
            q2(i,k) = a4(3,i,km)
         elseif(pe2(i,k  ) .lt. pe1(i,1) .and.
     &          pe2(i,k+1) .gt. pe1(i,1))  then
C Part of the grid above ptop
            q2(i,k) = a4(1,i,1)
c        elseif(pe2(i,k) .lt. pe1(i,km+1) .and.
c    &         (pe2(i,k+1)-pe1(i,km+1))/dp2(i,kn) .gt. 0.01) then
C Part of the grid below old ps
c           q2(i,k) = a4(1,i,km)
         else

         do 45 L=k0,km
C locate the top edge at pe2(i,k)
         if( pe2(i,k) .ge. pe1(i,L) .and.
     &       pe2(i,k) .le. pe1(i,L+1)    ) then
             k0 = L
             PL = (pe2(i,k)-pe1(i,L)) / dp1(i,L)
             if(pe2(i,k+1) .le. pe1(i,L+1)) then

C entire new grid is within the original grid

               PR = (pe2(i,k+1)-pe1(i,L)) / dp1(i,L)
               TT = R3*(PR*(PR+PL)+PL**2)
               q2(i,k) = a4(2,i,L) + 0.5*(a4(4,i,L)+a4(3,i,L)
     &                 - a4(2,i,L))*(PR+PL) - a4(4,i,L)*TT
              goto 555
             else

C Fractional area...

              delp = pe1(i,L+1) - pe2(i,k)
              TT   = R3*(1.+PL*(1.+PL))
              qsum = delp*(a4(2,i,L)+0.5*(a4(4,i,L)+
     &               a4(3,i,L)-a4(2,i,L))*(1.+PL)-a4(4,i,L)*TT)
              dpsum = delp
              k1 = L + 1
             goto 111
             endif
         endif
45       continue

111      continue
         do 55 L=k1,km
         if( pe2(i,k+1) .gt. pe1(i,L+1) ) then

C Whole layer..

            qsum  =  qsum + dp1(i,L)*q1(i,L)
            dpsum = dpsum + dp1(i,L)
         else
           delp = pe2(i,k+1)-pe1(i,L)
           esl  = delp / dp1(i,L)
           qsum = qsum + delp * (a4(2,i,L)+0.5*esl*
     &           (a4(3,i,L)-a4(2,i,L)+a4(4,i,L)*(1.-r23*esl)) )
          dpsum = dpsum + delp
           k0 = L
           goto 123
         endif
55       continue
        delp = pe2(i,k+1) - pe1(i,km+1)
        if(delp .gt. 0.) then
C Extended below old ps
           qsum = qsum + delp * a4(3,i,km)
          dpsum = dpsum + delp
        endif
123     q2(i,k) = qsum / dpsum
      endif
555   continue
5555  continue

      return
      end

c****6***0*********0*********0*********0*********0*********0**********72
      subroutine ppm2m(a4,delp,im,km,iv,kord)
c****6***0*********0*********0*********0*********0*********0**********72
c iv = 0: positive definite scalars
c iv = 1: others

      implicit none

      integer im, km, lmt, iv
      integer kord
      integer i, k, km1
      real a4(4,im,km), delp(im,km)

c local arrays.
      real dc(im,km),delq(im,km)
      real h2(im,km)
      real a1, a2, a3, b2, c1, c2, c3, d1, d2, f1, f2, f3, f4
      real s1, s2, s3, s4, ss3, s32, s34, s42, sc
      real qmax, qmin, cmax, cmin
      real dm, qm, dq, tmp

C Local scalars:
      real qmp
      real lac

c     real x, y, z
c     real median
c     median(x,y,z) = min(max(x,y), max(y,z), max(z,x))

      km1 = km - 1

      do 500 k=2,km
      do 500 i=1,im
      delq(i,k-1) = a4(1,i,k) - a4(1,i,k-1)
500   a4(4,i,k  ) = delp(i,k-1) + delp(i,k)

      do 1220 k=2,km1
      do 1220 i=1,im
      c1 = (delp(i,k-1)+0.5*delp(i,k))/a4(4,i,k+1)
      c2 = (delp(i,k+1)+0.5*delp(i,k))/a4(4,i,k)
      tmp = delp(i,k)*(c1*delq(i,k) + c2*delq(i,k-1)) /
     &                              (a4(4,i,k)+delp(i,k+1))
      qmax = max(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1)) - a4(1,i,k)
      qmin = a4(1,i,k) - min(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1))
      dc(i,k) = sign(min(abs(tmp),qmax,qmin), tmp)
1220  continue

c****6***0*********0*********0*********0*********0*********0**********72
c 4th order interpolation of the provisional cell edge value
c****6***0*********0*********0*********0*********0*********0**********72

      do 12 k=3,km1
      do 12 i=1,im
      c1 = delq(i,k-1)*delp(i,k-1) / a4(4,i,k)
      a1 = a4(4,i,k-1) / (a4(4,i,k) + delp(i,k-1))
      a2 = a4(4,i,k+1) / (a4(4,i,k) + delp(i,k))
      a4(2,i,k) = a4(1,i,k-1) + c1 + 2./(a4(4,i,k-1)+a4(4,i,k+1)) *
     &          ( delp(i,k)*(c1*(a1 - a2)+a2*dc(i,k-1)) -
     &                          delp(i,k-1)*a1*dc(i,k  ) )
12    continue

c three-cell parabolic subgrid distribution at model top

c     do 10 i=1,im
c three-cell PP-distribution
c Compute a,b, and c of q = aP**2 + bP + c using cell averages and delp
c a3 = a / 3
c b2 = b / 2
c     s1 = delp(i,1)
c     s2 = delp(i,2) + s1

c     s3 = delp(i,2) + delp(i,3)
c     s4 = s3 + delp(i,4)
c     ss3 =  s3 + s1
c     s32 = s3*s3
c     s42 = s4*s4
c     s34 = s3*s4
c model top
c     a3 = (delq(i,2) - delq(i,1)*s3/s2) / (s3*ss3)

c     if(abs(a3) .gt. 1.e-14) then
c        b2 =  delq(i,1)/s2 - a3*(s1+s2)
c        sc = -b2/(3.*a3)
c        if(sc .lt. 0. .or. sc .gt. s1) then
c            a4(2,i,1) = a4(1,i,1) - s1*(a3*s1 + b2)
c        else
c            a4(2,i,1) = a4(1,i,1) - delq(i,1)*s1/s2
c        endif
c     else
c Linear
c        a4(2,i,1) = a4(1,i,1) - delq(i,1)*s1/s2
c     endif
c     dc(i,1) = a4(1,i,1) - a4(2,i,1)
c compute coef. for the off-centered area preserving cubic poly.
c     dm = delp(i,1) / (s34*ss3*(delp(i,2)+s3)*(s4+delp(i,1)))
c     f1 = delp(i,2)*s34 / ( s2*ss3*(s4+delp(i,1)) )
c     f2 = (delp(i,2)+s3) * (ss3*(delp(i,2)*s3+s34+delp(i,2)*s4)
c    &      + s42*(delp(i,2)+s3+s32/s2))
c     f3 = -delp(i,2)*( ss3*(s32*(s3+s4)/(s4-delp(i,2))
c    &      + (delp(i,2)*s3+s34+delp(i,2)*s4))
c    &      + s42*(delp(i,2)+s3) )
c     f4 = ss3*delp(i,2)*s32*(delp(i,2)+s3) / (s4-delp(i,2))
c     a4(2,i,2) = f1*a4(1,i,1)+(f2*a4(1,i,2)+f3*a4(1,i,3)+
c    &            f4*a4(1,i,4))*dm
c****6***0*********0*********0*********0*********0*********0**********72
c No over- and undershoot condition
c     cmax = max(a4(1,i,1), a4(1,i,2))
c     cmin = min(a4(1,i,1), a4(1,i,2))
c     a4(2,i,2) = max(cmin,a4(2,i,2))
c     a4(2,i,2) = min(cmax,a4(2,i,2))
c 10    continue

C Area preserving cubic with 2nd deriv. = 0 at the boundaries
C Top
      do i=1,im
      d1 = delp(i,1)
      d2 = delp(i,2)
      qm = (d2*a4(1,i,1)+d1*a4(1,i,2)) / (d1+d2)
      dq = 2.*(a4(1,i,2)-a4(1,i,1)) / (d1+d2)
      c1 = 4.*(a4(2,i,3)-qm-d2*dq) / ( d2*(2.*d2*d2+d1*(d2+3.*d1)) )
      c3 = dq - 0.5*c1*(d2*(5.*d1+d2)-3.*d1**2)
      a4(2,i,2) = qm - 0.25*c1*d1*d2*(d2+3.*d1)
      a4(2,i,1) = d1*(2.*c1*d1**2-c3) + a4(2,i,2)
      dc(i,1) =  a4(1,i,1) - a4(2,i,1)
C No over- and undershoot condition
      cmax = max(a4(1,i,1), a4(1,i,2))
      cmin = min(a4(1,i,1), a4(1,i,2))
      a4(2,i,2) = max(cmin,a4(2,i,2))
      a4(2,i,2) = min(cmax,a4(2,i,2))
      enddo

      if(iv .eq. 0) then
      do i=1,im
         a4(2,i,1) = max(0.,a4(2,i,1))
         a4(2,i,2) = max(0.,a4(2,i,2))
      enddo
      endif

c****6***0*********0*********0*********0*********0*********0**********72

c Bottom
c Area preserving cubic with 2nd deriv. = 0 at the surface
      do 15 i=1,im
      d1 = delp(i,km)
      d2 = delp(i,km1)
      qm = (d2*a4(1,i,km)+d1*a4(1,i,km1)) / (d1+d2)
      dq = 2.*(a4(1,i,km1)-a4(1,i,km)) / (d1+d2)
      c1 = (a4(2,i,km1)-qm-d2*dq) / (d2*(2.*d2*d2+d1*(d2+3.*d1)))
      c3 = dq - 2.0*c1*(d2*(5.*d1+d2)-3.*d1**2)
      a4(2,i,km) = qm - c1*d1*d2*(d2+3.*d1)
      a4(3,i,km) = d1*(8.*c1*d1**2-c3) + a4(2,i,km)
      dc(i,km) = a4(3,i,km) -  a4(1,i,km)
c****6***0*********0*********0*********0*********0*********0**********72
c No over- and undershoot condition
      cmax = max(a4(1,i,km), a4(1,i,km1))
      cmin = min(a4(1,i,km), a4(1,i,km1))
      a4(2,i,km) = max(cmin,a4(2,i,km))
      a4(2,i,km) = min(cmax,a4(2,i,km))
c****6***0*********0*********0*********0*********0*********0**********72
15    continue

      if(iv .eq. 0) then
      do i=1,im
         a4(2,i,km) = max(0.,a4(2,i,km))
         a4(3,i,km) = max(0.,a4(3,i,km))
      enddo
      endif

      do 20 k=1,km1
      do 20 i=1,im
      a4(3,i,k) = a4(2,i,k+1)
20    continue
c
c f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
c

c Top 2 and bottom 2 layers always use monotonic mapping

      do k=1,2
         do i=1,im
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(1,k),a4(1,1,k),im, 0)
      enddo

      if(kord .eq. 7) then
c****6***0*********0*********0*********0*********0*********0**********72
C Huynh's 2nd constraint
c****6***0*********0*********0*********0*********0*********0**********72
      do k=2, km1
         do i=1,im
            h2(i,k) = delq(i,k) - delq(i,k-1)
         enddo
      enddo

      do 4000 k=3, km-2
      do 3000 i=1, im
C Right edges
         qmp   = a4(1,i,k)                 + 2.0*delq(i,k-1)
         lac   = a4(1,i,k) + 1.5*h2(i,k-1) + 0.5*delq(i,k-1)
         qmin  = min(a4(1,i,k), qmp, lac)
         qmax  = max(a4(1,i,k), qmp, lac)
c        a4(3,i,k) = median(a4(3,i,k), qmin, qmax)
         a4(3,i,k) = min(max(a4(3,i,k), qmin), qmax)
C Left  edges
         qmp   = a4(1,i,k)                 - 2.0*delq(i,k)
         lac   = a4(1,i,k) + 1.5*h2(i,k+1) - 0.5*delq(i,k)
         qmin  = min(a4(1,i,k), qmp, lac)
         qmax  = max(a4(1,i,k), qmp, lac)
c        a4(2,i,k) = median(a4(2,i,k), qmin, qmax)
         a4(2,i,k) = min(max(a4(2,i,k), qmin), qmax)
C Recompute A6
         a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
3000  continue
C Additional constraint to prevent negatives
         if (iv .eq. 0) then
             call kmppm(dc(1,k),a4(1,1,k),im, 2)
         endif
4000  continue

      else

         lmt = kord - 3
         lmt = max(0, lmt)
         if (iv .eq. 0) lmt = min(2, lmt)

      do k=3, km-2
         do i=1,im
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(1,k),a4(1,1,k),im, lmt)
      enddo
      endif

      do 5000 k=km1,km
         do i=1,im
         a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call kmppm(dc(1,k),a4(1,1,k),im, 0)
5000  continue

      return
      end

c****6***0*********0*********0*********0*********0*********0**********72
      subroutine kmppm(dm, a4, km, lmt)
c****6***0*********0*********0*********0*********0*********0**********72
      implicit none

      real r12
      parameter (r12 = 1./12.)

      integer km, lmt
      integer i
      real a4(4,km),dm(km)
      real da1, da2, a6da
      real fmin

      if (lmt .eq. 3) return
c Full constraint

      if(lmt .eq. 0) then
      do 100 i=1,km
      if(dm(i) .eq. 0.) then
         a4(2,i) = a4(1,i)
         a4(3,i) = a4(1,i)
         a4(4,i) = 0.
      else
         da1  = a4(3,i) - a4(2,i)
         da2  = da1**2
         a6da = a4(4,i)*da1
         if(a6da .lt. -da2) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
         elseif(a6da .gt. da2) then
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
         endif
      endif
100   continue
      elseif (lmt .eq. 2) then
c Positive definite

c Positive definite constraint
      do 250 i=1,km
      if(abs(a4(3,i)-a4(2,i)) .ge. -a4(4,i)) go to 250
      fmin = a4(1,i)+0.25*(a4(3,i)-a4(2,i))**2/a4(4,i)+a4(4,i)*r12
      if(fmin.ge.0.) go to 250
      if(a4(1,i).lt.a4(3,i) .and. a4(1,i).lt.a4(2,i)) then
            a4(3,i) = a4(1,i)
            a4(2,i) = a4(1,i)
            a4(4,i) = 0.
      elseif(a4(3,i) .gt. a4(2,i)) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
      else
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
      endif
250   continue

      elseif (lmt .eq. 1) then
      write(6,*) 'semi-monotonic not implemented!'
      stop
      endif
      return
      end

      subroutine atod(ua,va,u,v,im,jm)

C Convert A-grid winds to D-grid
C 4th order version

      real r16
      parameter (r16 = 1./16.)

      real   ua(im,jm),   va(im,jm)
!ams  real*8  u(im,jm),    v(im,jm)
      real    u(im,jm),    v(im,jm)
C
C****6***0*********0*********0*********0*********0*********0**********72

      jmr = jm-1
      jnp = jm

      DO 10 i=1,IM
      u(i,  2) = 0.5*(ua(i,2) + ua(i,1))
10    u(i,JNP) = 0.5*(ua(i,JMR) + ua(i,JNP))
C
      do 20 j=3,JMR
      DO 20 i=1,IM
      u(i,j)=(9.*(ua(i,j) + ua(i,j-1))-(ua(i,j+1) + ua(i,j-2)))*R16
20    continue
C **********************************************
C
C     JS = 1+JMR/8
      JS = 3
      JN = JNP - JS + 1
C
      do 30 j=2,JS-1
      DO 30 i=2,IM
      v(i,j) = 0.5*(va(i,j) + va(i-1,j))
30    continue
C
      do j=2,JS-1
      v(1,j) = 0.5*(va(1,j) + va(IM,j))
      enddo
C
      do 35 j=JN+1,JMR
      DO 35 i=2,IM
      v(i,j) = 0.5*(va(i,j) + va(i-1,j))
35    continue
C
      do j=JN+1,JMR
      v(1,j) = 0.5*(va(1,j) + va(IM,j))
      enddo
C
      do 40 j=JS,JN
      DO 40 i=3,IM-1
      v(i,j) = (9.*(va(i,  j) + va(i-1,j)) -
     &             (va(i+1,j) + va(i-2,j))  ) * R16
40    continue
C
      do 50 j=JS,JN
C i=1
      v(1,j) = (9.*(va(1,  j) + va(IM,j)) -
     &             (va(2,j) + va(IM-1,j))  ) * R16
C i=2
      v(2,j) = (9.*(va(2,  j) + va(1,j)) -
     &             (va(3,j) + va(IM,j))  ) * R16
C i=IM
      v(IM,j) = (9.*(va(IM,  j) + va(IM-1,j)) -
     &               (va(1  ,  j) + va(IM-2,j))  ) * R16
50    continue
      return
      end

C****6***0*********0*********0*********0*********0*********0**********72
      subroutine plft2d(p, JS, JN, NDEG)
C****6***0*********0*********0*********0*********0*********0**********72

C This is a weak LOCAL polar filter.
C Developer: Shian-Jiann Lin

      implicit none

c     include 'vrslv.com'

      integer imr, jmr

      parameter(imr=144, jmr=90)
c     parameter(imr=288, jmr=180)

      integer   JNP, Nmax
      parameter (JNP = JMR + 1)
      parameter (Nmax= IMR/2)

      integer js, jn, ndeg
      integer i, j, n, ideg, jj, jc
!ams      real*8 p(imr,jnp),cosp(JMR+1),cose(JMR+1),a(0:Nmax+1)
!ams      real*8 se(JMR+1), sc(JMR+1),sine(JNP),sinp(JNP)
      real p(imr,jnp),cosp(JMR+1),cose(JMR+1),a(0:Nmax+1)
      real se(JMR+1), sc(JMR+1),sine(JNP),sinp(JNP)
!ams  real*8 pi, dp, dl, e0, ycrit, coszc, smax, rn, rn2, esl, tmp
      real   pi, dp, dl, e0, ycrit, coszc, smax, rn, rn2, esl, tmp
      data IDEG /0/
      save sc, se

      if(IDEG .ne. NDEG) then
	IDEG = NDEG
C  (e0 = 2.6)
      e0 = 0.5 * sqrt(27.)
      PI = 4. * ATAN(1.)
      call setrig(IMR,JNP,DP,DL,cosp,cose,sinp,sine)

      ycrit = IDEG*PI/180.
      coszc = cos(ycrit)

      Smax = JMR/2
      write(6,*) 'Critical latitude in local pft = ',NDEG

      a(0) = 1.
      do n=1,Nmax+1
      rn = n
      rn2 = 2*n
      a(n) = sqrt(rn2+1.) * ((rn2+1.)/rn2)**rn
      enddo

      do j=2,JMR
      sc(j) = coszc / cosp(j)

      IF(sc(j) .gt.1. .and. sc(j) .le. 1.5 ) THEN
         esl = 1./ sc(j)
         sc(j) =  1. +  (1.-esl) / (1.+esl)
      ELSEIF(sc(j) .gt.1.5 .and. sc(j) .le. e0 ) THEN
         esl = 1./ sc(j)
         sc(j) =  1. + 2./ (27.*esl**2 - 2.)
      ELSEIF(sc(j).gt. e0) THEN
C Search
      do jj=1,Nmax
      if(sc(j).le. a(jj)) then
      jc = jj
      goto 111
      endif
      enddo

      jc = Nmax + 1
111   continue
      tmp = ((sc(j) - a(jc-1))/(a(jc) - a(jc-1)))**0.25
      sc(j) =  jc + min(1., tmp)
      sc(j) =  min(Smax,sc(j))
      ENDIF
      enddo
C ====================================================
      do j=2,JMR+1
      se(j) = coszc / cose(j)
      IF(se(j) .gt.1. .and. se(j) .le. 1.5 ) THEN
         esl = 1./ se(j)
         se(j) =  1. + (1.-esl) / (1.+esl)
      ELSEIF(se(j) .gt.1.5 .and. se(j) .le. e0 ) THEN
         esl = 1./ se(j)
         se(j) =  1. + 2./ (27.*esl**2 - 2.)
      ELSEIF(se(j).gt. e0) THEN
C Search
      do jj=1,Nmax
      if(se(j) .le. a(jj)) then
      jc = jj
      goto 222
      endif
      enddo

      jc = Nmax + 1
222   continue
      tmp = ((se(j) - a(jc-1))/(a(jc) - a(jc-1)))**0.25
      se(j) =  jc + min(1., tmp)
      se(j) =  min(Smax,se(j))
      ENDIF
      enddo

	do i=1,IMR
	se(  2) = sc(2)
	se(JNP) = sc(JMR)
	enddo

      do j=2,JMR
c        write(6,*) j,sc(j)
      enddo
      ENDIF

      if(JN.eq.JMR) then
C Cell-centered variables
      CALL lpft(p,IMR,JNP,2,JMR,Sc)
      else
C Cell-edge variables
      CALL lpft(p,IMR,JNP,2,JNP,Se)
      endif
      return
      end


C****6***0*********0*********0*********0*********0*********0**********72
      subroutine lpft(p,IM,JM,j1,j2,S)
C****6***0*********0*********0*********0*********0*********0**********72

      implicit none

c     include 'vrslv.com'

      integer imr, jmr

      parameter(imr=144, jmr=90)
c     parameter(imr=288, jmr=180)

      integer   JNP
      parameter (JNP = JMR+1)

      integer im, jm, j1, j2
      integer i, j, n, nt
!ams  real*8 P(IMR,JNP),S(JNP),ptmp(0:IMR+1),q(IMR+1)
      real   P(IMR,JNP),S(JNP),ptmp(0:IMR+1),q(IMR+1)
!ams  real*8 frac, rsc, bt, fac
      real   frac, rsc, bt, fac

      do 2500 j=j1,j2
      if(S(j) .gt. 1.02) then

      NT  = INT(S(j))
      frac = S(j) - NT

      if(frac .gt. 0.01) then
      rsc = 1./ (1.+frac)
      bt = 0.5 * frac

      do i=1,IMR
      ptmp(i) = p(i,j)
      enddo

      ptmp(0)     = p(IMR,j)
      ptmp(IMR+1) = p(1,j)

      do i=1,IMR
      p(i,j) = rsc * (ptmp(i) + bt*(ptmp(i-1)+ptmp(i+1)))
      enddo
      endif

      NT = NT-1
      if(NT.GE.1) then
      fac =  0.25**NT

      do 500 N=1,NT

      ptmp(0) = p(IMR,j)
      do i=1,IMR
      ptmp(i) = p(i,j)
      enddo

C left
      do i=1,IMR
      q(i) = ptmp(i) + ptmp(i-1)
      enddo
      q(IMR+1) = q(1)

C right
      do i=1,IMR
      p(i,j) = q(i) + q(i+1)
      enddo

500   continue

      do i=1,IMR
      p(i,j) = p(i,j)*fac
      enddo
      endif
      ENDIF

2500  continue

      return
      end


C****6***0*********0*********0*********0*********0*********0**********72
      subroutine setrig(im,jm,DP,DL,cosp,cose,sinp,sine)
C****6***0*********0*********0*********0*********0*********0**********72
      implicit none

      integer im, jm
      integer j, jm1
!ams  real*8 sine(jm),cosp(jm),sinp(jm),cose(jm)
      real   sine(jm),cosp(jm),sinp(jm),cose(jm)
!ams  real*8 dp, dl
!ams  real*8 PI, ph5
      real   dp, dl
      real   PI, ph5

      jm1 = jm - 1
      PI  = 4.D0 * DATAN(1.D0)
      DL  = (PI+PI)/DBLE(im)
      DP  = PI/DBLE(jm1)

      do 10 j=2,jm
         ph5  = -0.5D0*PI + (DBLE(J-1)-0.5D0)*(PI/DBLE(jm1))
!ams10    sine(j) = DSIN(ph5)
10    sine(j) = SIN(ph5)

      cosp( 1) =  0.
      cosp(jm) =  0.

      do 80 j=2,jm1
80    cosp(j) = (sine(j+1)-sine(j)) / DP

C Define cosine at edges..

      do 90 j=2,jm
90    cose(j) = 0.5 * (cosp(j-1) + cosp(j))
      cose(1) = cose(2)

      sinp( 1) = -1.
      sinp(jm) =  1.

      do 100 j=2,jm1
100   sinp(j) = 0.5 * (sine(j) + sine(j+1))

C     write(6,*) 'SETRIG called. ',im,jm
      return
      end


      subroutine dry_adj(im, km,  klo, pt, u, v, q, dp, npt)
      implicit none
      integer i, k, im, km
      integer npt, klo
      real tiny
      parameter (tiny = 0.01)
      real u(im,km)
      real v(im,km)
      real pt(im,km)
      real q(im,km)
      real dp(im,km)

      npt = 0
C Top down

      do k=1, klo-1
         do i=1,im
            if((pt(i,k)+tiny) .lt. pt(i,k+1)) then
               pt(i,k) = (pt(i,k)*dp(i,k)+pt(i,k+1)*dp(i,k+1))
     &                  / (dp(i,k) + dp(i,k+1))
               pt(i,k+1) = pt(i,k)
               u(i,k) = (u(i,k)*dp(i,k)+u(i,k+1)*dp(i,k+1))
     &                  / (dp(i,k) + dp(i,k+1))
               u(i,k+1) = u(i,k)
               v(i,k) = (v(i,k)*dp(i,k)+v(i,k+1)*dp(i,k+1))
     &                  / (dp(i,k) + dp(i,k+1))
               v(i,k+1) = v(i,k)
               q(i,k) = (q(i,k)*dp(i,k)+q(i,k+1)*dp(i,k+1))
     &                  / (dp(i,k) + dp(i,k+1))
               q(i,k+1) = q(i,k)
               npt = npt + 1
            endif
         enddo
      enddo

C From Bottom up

      if(npt .ne. 0) then
      do k=klo, 2
         do i=1,im
            if(pt(i,k) .gt. (pt(i,k-1)+tiny)) then
               pt(i,k) = (pt(i,k)*dp(i,k)+pt(i,k-1)*dp(i,k-1))
     &                  / (dp(i,k) + dp(i,k-1))
               pt(i,k-1) = pt(i,k)
               u(i,k) = (u(i,k)*dp(i,k)+u(i,k-1)*dp(i,k-1))
     &                  / (dp(i,k) + dp(i,k-1))
               u(i,k-1) = u(i,k)
               v(i,k) = (v(i,k)*dp(i,k)+v(i,k-1)*dp(i,k-1))
     &                  / (dp(i,k) + dp(i,k-1))
               v(i,k-1) = v(i,k)
               q(i,k) = (q(i,k)*dp(i,k)+q(i,k-1)*dp(i,k-1))
     &                  / (dp(i,k) + dp(i,k-1))
               q(i,k-1) = q(i,k)
               npt = npt + 1
            endif
         enddo
      enddo
      endif

      return
      end

c****6***0*********0*********0*********0*********0*********0**********72
      subroutine d2a(u,v,ua,va,imr,jnp,coslon,sinlon)
c****6***0*********0*********0*********0*********0*********0**********72

c This is primarily for turbulence package designed for A-grid.
c Also used for output to A-grid.

      implicit none
      integer   imr, imh, jmr, jnp
      real r16
      parameter (r16 = 1./16.)

      integer i, j, js, jn, im1
      real un, vn, us, vs

c Convert D-grid winds to A-grid
c u --> ua, v --> va

      real u(imr,jnp),v(imr,jnp),ua(imr,jnp),va(imr,jnp)
     &    ,coslon(imr),sinlon(imr),utmp(imr,jnp),vtmp(imr,jnp)

      imh = imr/2
      jmr = jnp-1

c Near poles.
      do 20 i=1,imr
      utmp(i,2) = 0.5*(u(i,2) + u(i,3))
      utmp(i,jmr) = 0.5*(u(i,jmr) + u(i,jnp))
20    continue

      do 25 j=3,jmr-1
      do 25 i=1,imr
      utmp(i,j) = ( 9.*(u(i,j+1)+u(i,j)) -
     &                 (u(i,j+2)+u(i,j-1)) ) * r16
25    continue

      js = 3
      jn = jnp - js + 1
      im1 = imr-1

      do 30 j=2,js-1
      do 30 i=1,im1
30    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 35 j=2,js-1
35    vtmp(imr,j) = 0.5*(v(imr,j) + v(1,j))

      do 45 j=jn+1,jmr
      do 45 i=1,im1
45    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 50 j=jn+1,jmr
50    vtmp(imr,j) = 0.5*(v(imr,j) + v(1,j))

      do 60 j=js,jn
      do 60 i=2,imr-2
      vtmp(i,j) = ( 9.*(v(i,  j) + v(i+1,j)) -
     &                 (v(i-1,j) + v(i+2,j))  ) * r16
60    continue

      do 70 j=js,jn
      vtmp(1,j) = ( 9.*(v(1,j) + v(2,j)) -
     &                 (v(imr,j) + v(3,j))  ) * r16
      vtmp(imr,j) = ( 9.*(v(imr,j) + v(1,j)) -
     &                 (v(im1,j) + v(2,j))  ) * r16
      vtmp(im1,j) = ( 9.*(v(im1,  j) + v(imr,j)) -
     &                 (v(imr-2,j) + v(1,j))  ) * r16
70    continue

c Projection at Poles
c NP
      un = 0.
      vn = 0.
c SP
      us = 0.
      vs = 0.
      j=jmr
      do 80 i=1,imh
      un = un + (utmp(i+imh,j)-utmp(i,j))*sinlon(i)
     &        + (vtmp(i+imh,j)-vtmp(i,j))*coslon(i)
      vn = vn + (utmp(i,j)-utmp(i+imh,j))*coslon(i)
     &        + (vtmp(i+imh,j)-vtmp(i,j))*sinlon(i)
      us = us + (utmp(i+imh,2)-utmp(i,2))*sinlon(i)
     &        + (vtmp(i,2)-vtmp(i+imh,2))*coslon(i)
      vs = vs + (utmp(i+imh,2)-utmp(i,2))*coslon(i)
     &        + (vtmp(i+imh,2)-vtmp(i,2))*sinlon(i)
80    continue

      un = un/imr
      vn = vn/imr
      us = us/imr
      vs = vs/imr

      do 90 i=1,imh
c SP
      ua(i,1)   = -us*sinlon(i) - vs*coslon(i)
      va(i,1)   =  us*coslon(i) - vs*sinlon(i)
      ua(i+imh,1)   = -ua(i,1)
      va(i+imh,1)   = -va(i,1)
c NP
      ua(i,jnp) = -un*sinlon(i) + vn*coslon(i)
      va(i,jnp) = -un*coslon(i) - vn*sinlon(i)
      ua(i+imh,jnp) = -ua(i,jnp)
90    va(i+imh,jnp) = -va(i,jnp)

      do 100 j=2,jmr
      do 100 i=1,imr
      ua(i,j) = utmp(i,j)
100   va(i,j) = vtmp(i,j)

      return
      end

c......................................................................

c     Get file names and dates from command line
c     ------------------------------------------
      subroutine parse_cmd ( nymd, nhms, im, jm, nstep )


      include 'geos2fv.h'

      integer im, jm, nstep
      integer        iarg, argc, iargc
      character*255  argv, res

!     defaults
!     --------
      ftype = GFIO  ! GFIO output
      ifname = 'b290_03_w98.pre-anal.sig.t19971221'
      nymd = 19971221
      nhms = 000000
      ofname = 'b290_03_w98.fv.t19971221'
      tfname = '#none#'
      sfname = '#none#'
      prec = 32

C Caution: Using nstep =0 for fvgcm will force the lsm to do a hard
C          and random initialization
      nstep = 0


      print *, 'Starting...'

!     parse command line
!     ------------------
      argc =  iargc()
      narg = argc
      if ( argc .lt. 2 ) call usage()
      iarg = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) go to 111
         call GetArg ( iArg, argv )
         if(index(argv,'-i') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()               
            iarg = iarg + 1
            call GetArg ( iArg, ifname )
            narg = narg - 2
         else if(index(argv,'-o') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()               
            iarg = iarg + 1
            call GetArg ( iArg, ofname )
            narg = narg - 2
         else if(index(argv,'-t') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()               
            iarg = iarg + 1
            call GetArg ( iArg, tfname )
            narg = narg - 2
         else if(index(argv,'-s') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()               
            iarg = iarg + 1
            call GetArg ( iArg, sfname )
            narg = narg - 2
         else if(index(argv,'-gfio') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()               
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            if(index(argv,'64') .gt. 0 ) prec = 64
            else                         prec = 32
            ftype = GFIO
            narg = narg - 2
         else if(index(argv,'-n') .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()               
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) nstep
            narg = narg - 2
            print *, nstep
         else if(index(argv,'-grads') .gt. 0 ) then
            ftype = GRADS
            narg = narg - 1
         else if(index(argv,'-rs') .gt. 0 ) then
            ftype = RESTART
            narg = narg - 1
         else
           if ( narg .lt. 2 ) call usage()
           read(argv,*) nymd
           iarg = iarg + 1
           call GetArg ( iArg, argv )
           read(argv,*) nhms
         end if
      end do

 111  continue

!     Default files based on date/resolution
!     --------------------------------------
      if ( jm .gt. 99 ) then
         write(res,'(i3,a,i3)') im, 'x', jm
      else
         write(res,'(i3,a,i2)') im, 'x', jm
      end if

      if ( trim(sfname) .eq. '#none#' ) then
           write(sfname,'(a,i4.4,a)') 
     &                  '/share/fvgcm/sstsice_' // trim(res)
     &                  //'.weekly.y', nymd/10000, '.nc' 
      endif

      if ( trim(tfname) .eq. '#none#' ) then
           tfname = '/share/fvgcm/surf_r.data_' // trim(res) // '.usgs'
      end if

      lf = index(ofname,' ') - 1
      if ( ftype .eq. RESTART ) ofname = ofname(1:lf) ! // '.rs'
      if ( ftype .eq. GRADS   ) ofname = ofname(1:lf) // '.grd'
      if ( ftype .eq. GFIO    ) ofname = ofname(1:lf) // '.hdf'

      print *
      print *, 'Input  file name: ', ifname(1:60)
      print *, 'Output file name: ', ofname(1:60)
      print *, 'Topo   file name: ', tfname(1:60)
      print *, 'SST    file name: ', sfname(1:60)
      if ( ftype .eq. RESTART ) print *, 'Output file is RESTART'
      if ( ftype .eq. GRADS   ) print *, 'Output file is GRADS'
      if ( ftype .eq. GFIO    ) print *, 'Output file is GFIO'
      print *, 'Precision: ', prec
      print *, 'nymd, nhms, nstep: ', nymd, nhms, nstep
      print *

      return
      end

      subroutine usage()
      print *, 'Usage:  '
      print *
      print *, ' geos2fv.x [-i ifname] [-o ofname] [-t tfname] [-s sfname]' 
      print *, '           [-gfio 32|64] [-grads] [-rs] [-n nstep] nymd nhms'
      print *
      print *, 'where:'
      print *
      print *, '  -i ifname  input GEOS prog.sig file in HDF format, '
      print *, '             written by GFIO'
      print *
      print *, '  -o ofname  output file basename'
      print *
      print *, '  -t tfname  topography file name, e.g., surf.data_144x91;'
      print *, '             find these files on bjerknes:/share/fvgcm/fvdata'
      print *, '             The default is  a resolution appropriate file '
      print *, '             under /share/fvgcm/'
      print *
      print *, '  -s sfname  SST file name, e.g., sstsice_144x91.weekly.y1997.nc;' 
      print *, '             find these files on dixon0:/share/fvgcm/fvdata.'
      print *, '             You only need this if producing GFIO files.'
      print *, '             The default is  a resolution/year appropriate file '
      print *, '             under /share/fvgcm/'
      print *
      print *, '  -gfio      specify GFIO output with 32 or 64 bits; the'
      print *, '             default is GFIO 32 bits'
      print *
      print *, '  -grads     output file is in GrADS IEEE format'
      print *
      print *, '  -rs        output is FVGCM restart (must have been'
      print *, '             compiled with -r8 for this to work'
      print *
      print *, '  -n nstep   FVGCM step (for restarts); default: 0'
      print *
      print *, '  nymd       year-month-day, e.g., 19971221'
      print *
      print *, '  nhms       hour-min-sec, e.g., 120000'
      print *
      print *, 'Send algorithm related questions to SJ Lin (lin@dao),'
      print *, 'GFIO, lats4d and other format related questions to'
      print *, 'Arlindo da Silva (dasilva@dao).'
      print *
      print *, 'Last update: Seg Dez  6 08:13:42 EST 1999'
      print *

      call exit(7)
      end                      

