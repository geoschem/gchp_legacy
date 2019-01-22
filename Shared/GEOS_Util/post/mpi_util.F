#include "unused_dummy.H"
C **********************************************************************
      subroutine init_dynamics_lattice ( lattice,comm,imglobal,jmglobal,lm )
C **********************************************************************
      use dynamics_lattice_module
      implicit none
      include 'mymalloc_interface'
      type ( dynamics_lattice_type ) lattice
      integer comm,imglobal,jmglobal,lm
      real    dummy

#if mpi
      include 'mpif.h'
#else
      integer mpi_real,mpi_double_precision
#endif
      integer  myid,ierror,im,jm,i,j,m,n,npes,rm
      integer  isum,jsum,imx,nx, img, ppe(imglobal)

#if (mpi)
      call mpi_comm_rank ( comm,myid,ierror )
#else
      myid = 0
      mpi_real = 4
      mpi_double_precision = 8
#endif

      call mymalloc ( lattice%ppeg,imglobal )

      lattice%comm = comm
      lattice%myid = myid
      lattice%nx   = size( lattice%im )
      lattice%ny   = size( lattice%jm )

      npes = lattice%nx*lattice%ny

      lattice%imglobal = imglobal
      lattice%jmglobal = jmglobal
      lattice%lm       = lm

      if( kind(dummy).eq.4 ) lattice%mpi_rkind = mpi_real
      if( kind(dummy).eq.8 ) lattice%mpi_rkind = mpi_double_precision

c Initialize lattice%im
c ---------------------
      im = imglobal/lattice%nx
      rm = imglobal-lattice%nx*im
      lattice%imax = im
      do n=0,lattice%nx-1
                      lattice%im(n) = im
      if( n.le.rm-1 ) lattice%im(n) = im+1
                      lattice%imax  = max( lattice%imax,lattice%im(n) )
      enddo

c Initialize lattice%jm
c ---------------------
      jm = jmglobal/lattice%ny
      rm = jmglobal-lattice%ny*jm
      lattice%jmax = jm
      do n=0,lattice%ny-1
                      lattice%jm(n) = jm
      if( n.le.rm-1 ) lattice%jm(n) = jm+1
                      lattice%jmax  = max( lattice%jmax,lattice%jm(n) )
      enddo

c Initialize relative PE address
c ------------------------------
      lattice%pei = mod(myid,lattice%nx)
      lattice%pej =     myid/lattice%nx

c Initialize global index for local locations
c -------------------------------------------
      call mymalloc ( lattice%iglobal,lattice%im(lattice%pei) )
      call mymalloc ( lattice%jglobal,lattice%jm(lattice%pej) )

      isum = 0
      do n=0,lattice%nx-1
         if ( n.eq.lattice%pei ) then
            do i=1,lattice%im(n)
            lattice%iglobal(i) = i+isum
            enddo
         endif
      isum = isum + lattice%im(n)
      enddo

      jsum = 0
      do m=0,lattice%ny-1
         if ( m.eq.lattice%pej ) then
            do j=1,lattice%jm(m)
            lattice%jglobal(j) = j+jsum
            enddo
         endif
      jsum = jsum + lattice%jm(m)
      enddo

c Initialize local index for global locations
c -------------------------------------------
      call mymalloc ( lattice%ilocal,lattice%imglobal )
      call mymalloc ( lattice%jlocal,lattice%jmglobal )

         n = 0
      isum = 0
      do i = 1,imglobal
      if(i.gt.isum+lattice%im(n) ) then
      isum =  isum+lattice%im(n)
         n =  n + 1
      endif
      lattice%ilocal(i) = i-isum
      enddo

         m = 0
      jsum = 0
      do j = 1,jmglobal
      if(j.gt.jsum+lattice%jm(m) ) then
      jsum =  jsum+lattice%jm(m)
         m =  m + 1
      endif
      lattice%jlocal(j) = j-jsum
      enddo

c Initialize relative PE address for global i-j locations
c -------------------------------------------------------
      call mymalloc ( lattice%peiglobal,imglobal )
      call mymalloc ( lattice%pejglobal,jmglobal )

      isum = 0
      do n=0,lattice%nx-1
         do i=1,lattice%im(n)
         lattice%peiglobal( i+isum ) = n
         enddo
      isum = isum + lattice%im(n)
      enddo

      jsum = 0
      do m=0,lattice%ny-1
         do j=1,lattice%jm(m)
         lattice%pejglobal( j+jsum ) = m
         enddo
      jsum = jsum + lattice%jm(m)
      enddo

c Initialize Pole PE ghosts for each iglobal
c ------------------------------------------
      isum = 0
      do nx=0,lattice%nx-1
        imx = lattice%im(nx)
        do i=1,imx
        ppe(i+isum) = nx
        enddo
        isum = isum + imx
      enddo

      do i=1,imglobal/2
      lattice%ppeg(i) = ppe(i+imglobal/2)
      lattice%ppeg(i+imglobal/2) = ppe(i)
      enddo

c Allocate Lattice%img
c --------------------
      if(.not.associated(lattice%img)) then
      allocate ( lattice%img(0:nx-1,imglobal) )
      do m=1,imglobal
      do n=0,nx-1
      lattice%img(n,m) = 0
      enddo
      enddo
      else
             m=size(lattice%img)
          if(m.ne.nx*imglobal) then
          print *, 'Allocated Lattice Size (',m,') does not match request (',nx*imglobal,') for lattice%img!'
          call my_finalize
          call my_exit (101)
          endif
      endif

c Allocate Lattice%im0
c --------------------
      if(.not.associated(lattice%im0)) then
      allocate ( lattice%im0(0:nx-1,imglobal) )
      do m=1,imglobal
      do n=0,nx-1
      lattice%im0(n,m) = 0
      enddo
      enddo
      else
             m=size(lattice%im0)
          if(m.ne.nx*imglobal) then
          print *, 'Allocated Lattice Size (',m,') does not match request (',nx*imglobal,') for lattice%im0!'
          call my_finalize
          call my_exit (101)
          endif
      endif

c Determine Pole PE ghosts for each Processor
c -------------------------------------------
      isum = 0
      do nx=0,lattice%nx-1
        imx  = lattice%im(nx)
        lattice%npeg(nx)   = 1
        lattice% im0(nx,1) = 1
        img  = 1
        lattice%img (nx,lattice%npeg(nx)) = img
        do i=2,imx
           if( lattice%ppeg(i+isum) .eq. lattice%ppeg(i-1+isum) ) then
           img  = img  + 1
           lattice%img (nx,lattice%npeg(nx)) = img
           else
           lattice%npeg(nx) = lattice%npeg(nx) + 1
           lattice%im0 (nx,lattice%npeg(nx)) = i
           img  = 1
           lattice%img (nx,lattice%npeg(nx)) = img
           endif
        enddo
      isum = isum + imx
      enddo

c Print Lattice Characteristics
c -----------------------------
      if( myid.eq.0 ) then
      print *, 'Number of Processors in X: ',lattice%nx
      print *, 'Number of Processors in Y: ',lattice%ny
      print *
      endif
#if (mpi)
      do n=0,npes-1
         if( n.eq.myid ) then
         write(6,1000) myid,lattice%pei,lattice%pej,lattice%im(lattice%pei),lattice%jm(lattice%pej)
         endif
      call mpi_barrier (lattice%comm,ierror)
      enddo
      if( myid.eq.npes-1 ) print *
#endif

 1000 format(1x,'absolute PE id: ',i4,'  relative PE (i,j): (',i4,',',i4,')  (im,jm) = (',i4,',',i4,')')
      return
      end

C **********************************************************************
      subroutine scatter_1d ( qglobal,qlocal,lattice )
C **********************************************************************
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      real   qglobal( lattice%imglobal )
      real   qlocal ( lattice%im(lattice%myid) )
#if mpi
      include 'mpif.h'
      integer status(mpi_status_size)
#endif
      integer comm
      integer i,n,loc,im,imx,imglobal,myid,npes,ierror,mpi_rkind
      real, allocatable ::   buf(:)

      comm      = lattice%comm
      myid      = lattice%myid
      npes      = lattice%nx
      im        = lattice%im(myid)
      imglobal  = lattice%imglobal
      mpi_rkind = lattice%mpi_rkind

      if( myid.eq.0 ) then
      do i=1,im
      qlocal(i) = qglobal(i)
      enddo
#if (mpi)
      loc = im
      do n=1,npes-1
      imx = lattice%im(n)
      allocate ( buf(imx) )
         do i=1,imx
         loc = loc + 1
         buf(i) = qglobal(loc)
         enddo
      call mpi_send ( buf,imx,mpi_rkind,n,n,comm,ierror )
      deallocate ( buf )
      enddo
      else
      call mpi_recv ( qlocal,im,mpi_rkind,0,myid,comm,status,ierror )
#endif
      endif

      return
      end

C **********************************************************************
      subroutine scatter_2d ( qglobal,qlocal,lattice )
C **********************************************************************
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      real   qglobal( lattice%imglobal,lattice%jmglobal )
      real   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )
#if mpi
      include 'mpif.h'
      integer status(mpi_status_size)
#endif
      integer comm
      integer myid,npe,ierror,mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real, allocatable ::   buf(:,:)

      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal
      mpi_rkind = lattice%mpi_rkind

      if( myid.eq.0 ) then
          jsum = 0
          do ny=0,lattice%ny-1
          jmy = lattice%jm(ny)
             isum = 0
             do nx=0,lattice%nx-1
             imx = lattice%im(nx)

             if( nx.eq.0 .and. ny.eq.0 ) then
                 do j=1,jmy
                 do i=1,imx
                 qlocal(i,j) = qglobal(i,j)
                 enddo
                 enddo
                 isum = isum + imx
             else
#if (mpi)
                 allocate ( buf(imx,jmy) )
                 do j=1,jmy
                 do i=1,imx
                 buf(i,j) = qglobal(i+isum,j+jsum)
                 enddo
                 enddo
                 isum = isum + imx
             
                 npe = nx + ny*lattice%nx
                 call mpi_send ( buf,imx*jmy,mpi_rkind,npe,npe,comm,ierror )
                 deallocate ( buf )
#endif
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
#if (mpi)
          call mpi_recv ( qlocal,im*jm,mpi_rkind,0,myid,comm,status,ierror )
#endif
      endif

#if (mpi)
      call mpi_barrier ( comm,ierror )
#endif
      return
      end

C **********************************************************************
      subroutine iscatter_2d ( qglobal,qlocal,lattice )
C **********************************************************************
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer   qglobal( lattice%imglobal,lattice%jmglobal )
      integer   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )
#if mpi
      include 'mpif.h'
      integer status(mpi_status_size)
#endif
      integer comm
      integer myid,npe,ierror
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      integer, allocatable ::   buf(:,:)

      comm     = lattice%comm
      myid     = lattice%myid
      iloc     = lattice%pei
      jloc     = lattice%pej
      im       = lattice%im(iloc)
      jm       = lattice%jm(jloc)
      imglobal = lattice%imglobal
      jmglobal = lattice%jmglobal

      if( myid.eq.0 ) then
          jsum = 0
          do ny=0,lattice%ny-1
          jmy = lattice%jm(ny)
             isum = 0
             do nx=0,lattice%nx-1
             imx = lattice%im(nx)

             if( nx.eq.0 .and. ny.eq.0 ) then
                 do j=1,jmy
                 do i=1,imx
                 qlocal(i,j) = qglobal(i,j)
                 enddo
                 enddo
                 isum = isum + imx
             else
#if (mpi)
                 allocate ( buf(imx,jmy) )
                 do j=1,jmy
                 do i=1,imx
                 buf(i,j) = qglobal(i+isum,j+jsum)
                 enddo
                 enddo
                 isum = isum + imx
             
                 npe = nx + ny*lattice%nx
                 call mpi_send ( buf,imx*jmy,mpi_integer,npe,npe,comm,ierror )
                 deallocate ( buf )
#endif
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
#if (mpi)
          call mpi_recv ( qlocal,im*jm,mpi_integer,0,myid,comm,status,ierror )
#endif
      endif

#if (mpi)
      call mpi_barrier ( comm,ierror )
#endif
      return
      end

C **********************************************************************
      subroutine scatter_3d ( qglobal,qlocal,lattice )
C **********************************************************************
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      real   qglobal( lattice%imglobal,lattice%jmglobal,lattice%lm )
      real   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )
#if mpi
      include 'mpif.h'
      integer status(mpi_status_size)
#endif
      integer comm,lm,L
      integer myid,npe,ierror,mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real, allocatable ::   buf(:,:,:)

      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      lm        = lattice%lm
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal
      mpi_rkind = lattice%mpi_rkind

      if( myid.eq.0 ) then
          jsum = 0
          do ny=0,lattice%ny-1
          jmy = lattice%jm(ny)
             isum = 0
             do nx=0,lattice%nx-1
             imx = lattice%im(nx)

             if( nx.eq.0 .and. ny.eq.0 ) then
                 do L=1,lm 
                 do j=1,jmy
                 do i=1,imx
                 qlocal(i,j,L) = qglobal(i,j,L)
                 enddo
                 enddo
                 enddo
                 isum = isum + imx
             else
#if (mpi)
                 allocate ( buf(imx,jmy,lm) )
                 do L=1,lm
                 do j=1,jmy
                 do i=1,imx
                 buf(i,j,L) = qglobal(i+isum,j+jsum,L)
                 enddo
                 enddo
                 enddo
                 isum = isum + imx
             
                 npe = nx + ny*lattice%nx
                 call mpi_send ( buf,imx*jmy*lm,mpi_rkind,npe,npe,comm,ierror )
                 deallocate ( buf )
#endif
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
#if (mpi)
          call mpi_recv ( qlocal,im*jm*lm,mpi_rkind,0,myid,comm,status,ierror )
#endif
      endif

#if (mpi)
      call mpi_barrier ( comm,ierror )
#endif
      return
      end

C **********************************************************************
      subroutine gather_1d ( qglobal,qlocal,lattice )
C **********************************************************************
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      real   qglobal( lattice%imglobal )
      real   qlocal ( lattice%im(lattice%myid) )
#if mpi
      include 'mpif.h'
      integer status(mpi_status_size)
#endif
      integer comm, mpi_rkind
      integer i,n,loc,im,imx,myid,npes,ierror

      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      npes      = lattice%nx
      im        = lattice%im(myid)

      if( myid.eq.0 ) then
      do i=1,im
      qglobal(i) = qlocal(i)
      enddo
#if (mpi)
      loc = im
      do n=1,npes-1
      imx = lattice%im(n)
      call mpi_recv ( qglobal(1+loc),imx,mpi_rkind,n,n,comm,status,ierror )
      loc = loc + imx
      enddo
      else
      call mpi_send ( qlocal,im,mpi_rkind,0,myid,comm,ierror )
#endif
      endif

#if (mpi)
      call mpi_barrier ( comm,ierror )
#endif
      return
      end

C **********************************************************************
      subroutine gather_2d ( qglobal,qlocal,lattice )
C **********************************************************************
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      real   qglobal( lattice%imglobal,lattice%jmglobal )
      real   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )
#if mpi
      include 'mpif.h'
      integer status(mpi_status_size)
#endif
      integer comm, mpi_rkind
      integer myid,npe,ierror
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real, allocatable ::   buf(:,:)

      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal

      if( myid.eq.0 ) then
          jsum = 0
          do ny=0,lattice%ny-1
          jmy = lattice%jm(ny)
             isum = 0
             do nx=0,lattice%nx-1
             imx = lattice%im(nx)

             if( nx.eq.0 .and. ny.eq.0 ) then
                 do j=1,jmy
                 do i=1,imx
                 qglobal(i,j) = qlocal(i,j)
                 enddo
                 enddo
                 isum = isum + imx
             else
#if (mpi)
                 allocate ( buf(imx,jmy) )
                 npe = nx + ny*lattice%nx
                 call mpi_recv ( buf,imx*jmy,mpi_rkind,npe,npe,comm,status,ierror )
                 do j=1,jmy
                 do i=1,imx
                 qglobal(i+isum,j+jsum) = buf(i,j)
                 enddo
                 enddo
                 isum = isum + imx
                 deallocate ( buf )
#endif
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
#if (mpi)
          call mpi_send ( qlocal,im*jm,mpi_rkind,0,myid,comm,ierror )
#endif
      endif

#if (mpi)
      call mpi_barrier ( comm,ierror )
#endif
      return
      end

C **********************************************************************
      subroutine gather_3d ( qglobal,qlocal,lattice )
C **********************************************************************
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      real   qglobal( lattice%imglobal,lattice%jmglobal,lattice%lm )
      real   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )
#if mpi
      include 'mpif.h'
      integer status(mpi_status_size)
#endif
      integer comm,lm,L
      integer myid,npe,ierror, mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real, allocatable ::   buf(:,:,:)

      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      lm        = lattice%lm
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal

      if( myid.eq.0 ) then
          jsum = 0
          do ny=0,lattice%ny-1
          jmy = lattice%jm(ny)
             isum = 0
             do nx=0,lattice%nx-1
             imx = lattice%im(nx)

             if( nx.eq.0 .and. ny.eq.0 ) then
                 do L=1,lm
                 do j=1,jmy
                 do i=1,imx
                 qglobal(i,j,L) = qlocal(i,j,L)
                 enddo
                 enddo
                 enddo
                 isum = isum + imx
             else
#if (mpi)
                 allocate ( buf(imx,jmy,lm) )
                 npe = nx + ny*lattice%nx
                 call mpi_recv ( buf,imx*jmy*lm,mpi_rkind,npe,npe,comm,status,ierror )
                 do L=1,lm
                 do j=1,jmy
                 do i=1,imx
                 qglobal(i+isum,j+jsum,L) = buf(i,j,L)
                 enddo
                 enddo
                 enddo
                 isum = isum + imx
                 deallocate ( buf )
#endif
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
#if (mpi)
          call mpi_send ( qlocal,im*jm*lm,mpi_rkind,0,myid,comm,ierror )
#endif
      endif

#if (mpi)
      call mpi_barrier ( comm,ierror )
#endif
      return
      end

C **********************************************************************
      subroutine ghostx (a,b,im,jm,lm,n,lattice,flag)
C **********************************************************************
C ****                                                              ****
C ****  This program fills GHOST values in the Y-direction          ****
C ****                                                              ****
C ****  a ....... Input  Array  Non-Ghosted                         ****
C ****  b ....... Output Array      Ghosted                         ****
C ****  im ...... Local  Zonal      Dimension                       ****
C ****  jm ...... Local  Meridional Dimension                       ****
C ****  lm ...... Local  Vertical   Dimension                       ****
C ****  n ....... Number of GHOST values                            ****
C ****  lattice . Grid Lattice for MPI                              ****
C ****  flag .... Character*(*) to do 'east' or 'west' only         ****
C ****                                                              ****
C **********************************************************************
      use MAPL_BaseMod, only: MAPL_UNDEF
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
#if mpi
      include 'mpif.h'
      integer  stat(mpi_status_size,4)
#endif
      integer  myid, nx, comm, ierror, mpi_rkind
      integer  send_reqeast, send_reqwest
      integer  recv_reqeast, recv_reqwest
      character*(*) flag

      integer im,jm,lm,n,i,j,L
      integer east,west
      real    undef

      real a(1  :im  ,1:jm,lm)
      real b(1-n:im+n,1:jm,lm)
      real bufs(n,jm,lm,2)
      real bufr(n,jm,lm,2)

      call timebeg ('  ghostx  ')
      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      nx        = lattice%nx
      undef     = MAPL_UNDEF

      if( n.gt.im ) then
      print *
      print *, 'Processor ',myid,' does not contain enough grid-points in X (',im,') to perform ',n,' point ghosting!'
      call my_finalize
      call my_exit (101)
      endif

      east = mod( myid+nx+1,nx ) + (myid/nx)*nx
      west = mod( myid+nx-1,nx ) + (myid/nx)*nx

      do L=1,lm
      do j=1,jm
         do i=1,im
         b(i,j,L) = a(i,j,L)
         enddo
         do i=1,n
         bufs(i,j,L,1) = a(i,j,L)
         bufs(i,j,L,2) = a(im-n+i,j,L)
         bufr(i,j,L,1) = undef
         bufr(i,j,L,2) = undef
         b( 1-i,j,L)   = undef ! Initialize ghost regions
         b(im+i,j,L)   = undef ! Initialize ghost regions
         enddo
      enddo
      enddo

        if( nx.gt.1 ) then
#if (mpi)
          if( east.eq.west ) then
              call mpi_sendrecv( bufs,2*n*jm*lm,mpi_rkind,east,east,
     .                           bufr,2*n*jm*lm,mpi_rkind,west,myid,comm,stat,ierror )
          else

            if( flag.ne.'east' ) then
                                 call mpi_isend ( bufs(1,1,1,2),n*jm*lm,mpi_rkind,east,east,comm,send_reqeast,ierror )
                                 call mpi_irecv ( bufr(1,1,1,2),n*jm*lm,mpi_rkind,west,myid,comm,recv_reqwest,ierror )
            endif
            if( flag.ne.'west' ) then
                                 call mpi_isend ( bufs(1,1,1,1),n*jm*lm,mpi_rkind,west,west,comm,send_reqwest,ierror )
                                 call mpi_irecv ( bufr(1,1,1,1),n*jm*lm,mpi_rkind,east,myid,comm,recv_reqeast,ierror )
            endif
            if( flag.ne.'east' ) then
                                 call mpi_wait  ( send_reqeast,stat(1,1),ierror )
                                 call mpi_wait  ( recv_reqwest,stat(1,2),ierror )
            endif
            if( flag.ne.'west' ) then
                                 call mpi_wait  ( send_reqwest,stat(1,3),ierror )
                                 call mpi_wait  ( recv_reqeast,stat(1,4),ierror )
            endif

          endif
#endif
        else
          do L=1,lm
          do j=1,jm
          do i=1,n
          bufr(i,j,L,1) = bufs(i,j,L,1)
          bufr(i,j,L,2) = bufs(i,j,L,2)
          enddo
          enddo
          enddo
        endif

      do L=1,lm
      do j=1,jm
         do i=-n+1,0
         b(i,j,L) = bufr(i+n,j,L,2)
         enddo
         do i=im+1,im+n
         b(i,j,L) = bufr(i-im,j,L,1)
         enddo
      enddo
      enddo

      call timeend ('  ghostx  ')
      return
      end

C **********************************************************************
      subroutine ghosty (a,b,im,jm,lm,shift,msgn,n,lattice,flag)
C **********************************************************************
C ****                                                              ****
C ****  This program fills GHOST values in the Y-direction          ****
C ****                                                              ****
C ****  a ....... Input  Array  Non-Ghosted                         ****
C ****  b ....... Output Array      Ghosted                         ****
C ****  im ...... Local  Zonal      Dimension                       ****
C ****  jm ...... Local  Meridional Dimension                       ****
C ****  lm ...... Local  Vertical   Dimension                       ****
C ****  shift ... Integer Flag: 0 for A-Grid, 1 for C-Grid VWND     ****
C ****  msgn .... Integer Flag for Scaler (1) or Vector (-1)        ****
C ****  n ....... Number of GHOST values                            ****
C ****  lattice . Grid Lattice for MPI                              ****
C ****  flag .... Character*(*) to do 'north', 'south', or 'pole'   ****
C ****                                                              ****
C **********************************************************************
      use MAPL_BaseMod, only: MAPL_UNDEF
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
#if mpi
      include 'mpif.h'
      integer  status(mpi_status_size)
#endif
      integer  myid, nx, comm, ierror, mpi_rkind
      character*(*) flag

      integer im,jm,lm,shift,n,i,j,L
      integer north,south,request,msgn
      real    undef

      real a(1:im,  1:jm  ,lm)
      real b(1:im,1-n:jm+n,lm)

      real, allocatable :: apls(:,:,:)
      real, allocatable :: amns(:,:,:)
      real, allocatable ::  buf(:,:)

      call timebeg ('  ghosty  ')
      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      nx        = lattice%nx
      undef     = MAPL_UNDEF

      _UNUSED_DUMMY(msgn)
      if( (lattice%pej.eq.0            .and. n.gt.jm-1) .or.  ! Pole values cannot be used for ghosting
     .    (lattice%pej.eq.lattice%ny-1 .and. n.gt.jm-1) .or.  ! Pole values cannot be used for ghosting
     .                                      (n.gt.jm) ) then
      print *
      print *, 'Processor ',myid,' does not contain enough grid-points in Y (',jm,') to perform ',n,' point ghosting!'
      call my_finalize
      call my_exit (101)
      endif

      do L=1,lm
      do j=1,jm
      do i=1,im
      b(i,j,L) = a(i,j,L)
      enddo
      enddo
      b(:, 1-n:0   ,L) = undef  ! Initialize ghost regions
      b(:,jm+1:jm+n,L) = undef  ! Initialize ghost regions
      enddo

c Ghost South Pole
c ----------------
      if( lattice%pej.eq.0 .and. flag.ne.'north' ) then
          do L=1,lm
          do j=1,n
          do i=1,im
          b(i,1-j,L) = a(i,1+j-shift,L)
          enddo
          enddo
          enddo
      endif

c Ghost North Pole
c ----------------
      if( lattice%pej.eq.lattice%ny-1 .and. flag.ne.'south' ) then
          do L=1,lm
          do j=1,n
          do i=1,im
          b(i,jm+j-shift,L) = a(i,jm-j,L)
          enddo
          enddo
          enddo
      endif

c Ghost Non-Pole Points North
c ---------------------------
      if( flag.ne.'south' .and. flag.ne.'pole' ) then
          if( lattice%pej.eq.lattice%ny-1 .and. lattice%pej.ne.0 ) then
              south = myid - nx
              allocate (  buf(im*n*lm,1) )
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  buf(i+(j-1)*im+(L-1)*im*n,1) = a(i,j,L)
                 enddo
                 enddo
                 enddo
#if (mpi) 
              call mpi_isend( buf,n*im*lm,mpi_rkind,south,south,comm,request,ierror )
              call mpi_wait ( request,status,ierror )
#endif
              deallocate ( buf )
           endif
           if( lattice%pej.eq.0 .and. lattice%pej.ne.lattice%ny-1 ) then
              north = myid + nx
              allocate ( apls(im,n,lm) )
#if (mpi) 
              call mpi_recv ( apls,n*im*lm,mpi_rkind,north,myid,comm,status,ierror )
#else
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  apls(i,j,L) = buf(i+(j-1)*im+(L-1)*im*n,1)
                 enddo
                 enddo
                 enddo
#endif
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                 b(i,jm+j,L) = apls(i,j,L)
                 enddo
                 enddo
                 enddo
              deallocate ( apls )
           endif
           if( lattice%pej.ne.0 .and. lattice%pej.ne.lattice%ny-1 ) then
              north = myid + nx
              south = myid - nx
              allocate ( apls(im,n,lm) )
              allocate (  buf(im*n*lm,1) )
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  buf(i+(j-1)*im+(L-1)*im*n,1) = a(i,j,L)
                 enddo
                 enddo
                 enddo
#if (mpi) 
              call mpi_isend( buf,n*im*lm,mpi_rkind,south,south,comm,request,ierror )
              call mpi_recv ( apls,n*im*lm,mpi_rkind,north,myid,comm,status,ierror )
              call mpi_wait ( request,status,ierror )
#else
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  apls(i,j,L) = buf(i+(j-1)*im+(L-1)*im*n,1)
                 enddo
                 enddo
                 enddo
#endif
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                 b(i,jm+j,L) = apls(i,j,L)
                 enddo
                 enddo
                 enddo
              deallocate ( apls )
              deallocate ( buf )
           endif
      endif

c Ghost Non-Pole Points South
c ---------------------------
      if( flag.ne.'north' .and. flag.ne.'pole' ) then
          if( lattice%pej.eq.0 .and. lattice%pej.ne.lattice%ny-1 ) then
              north = myid + nx
              allocate (  buf(im*n*lm,1) )
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  buf(i+(j-1)*im+(L-1)*im*n,1) = a(i,jm-n+j,L)
                 enddo
                 enddo
                 enddo
#if (mpi) 
              call mpi_isend( buf,n*im*lm,mpi_rkind,north,north,comm,request,ierror )
              call mpi_wait ( request,status,ierror )
#endif
              deallocate ( buf )
           endif
           if( lattice%pej.eq.lattice%ny-1 .and. lattice%pej.ne.0 ) then
              south = myid - nx
              allocate ( amns(im,n,lm) )
#if (mpi) 
              call mpi_recv ( amns,n*im*lm,mpi_rkind,south,myid,comm,status,ierror )
#else
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  amns(i,j,L) = buf(i+(j-1)*im+(L-1)*im*n,1)
                 enddo
                 enddo
                 enddo
#endif
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                 b(i,j-n,L) = amns(i,j,L)
                 enddo
                 enddo
                 enddo
              deallocate ( amns )
           endif
           if( lattice%pej.ne.0 .and. lattice%pej.ne.lattice%ny-1 ) then
              north = myid + nx
              south = myid - nx
              allocate ( amns(im,n,lm) )
              allocate (  buf(im*n*lm,1) )
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  buf(i+(j-1)*im+(L-1)*im*n,1) = a(i,jm-n+j,L)
                 enddo
                 enddo
                 enddo
#if (mpi) 
              call mpi_isend( buf,n*im*lm,mpi_rkind,north,north,comm,request,ierror )
              call mpi_recv ( amns,n*im*lm,mpi_rkind,south,myid,comm,status,ierror )
              call mpi_wait ( request,status,ierror )
#else
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  amns(i,j,L) = buf(i+(j-1)*im+(L-1)*im*n,1)
                 enddo
                 enddo
                 enddo
#endif
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                 b(i,j-n,L) = amns(i,j,L)
                 enddo
                 enddo
                 enddo
              deallocate ( amns )
              deallocate ( buf )
           endif
      endif

      call timeend ('  ghosty  ')
      return
      end

      subroutine par_dot ( q1,q2,qdot,im,jm,lattice )
C***********************************************************************
C  PURPOSE                                                              
C     Compute dot product for MPI grid
C
C   q1 .... Input First  Vector
C   q2 .... Input Second Vector
C   qdot .. Output Dot Product
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer     im,jm
      real     q1(im,jm),q2(im,jm)
      real   qdot(jm)
      real   q12(lattice%imglobal,jm)
      real, allocatable :: buf(:,:)
#if mpi
      include 'mpif.h'
      integer status(mpi_status_size)
#endif

      real    sum
      integer i,j,n,i0,peid,peid0,ierror, mpi_rkind

      mpi_rkind = lattice%mpi_rkind

c Compute Local Product
c ---------------------
      do j=1,jm
      do i=1,im
      q12(i,j) = q1(i,j)*q2(i,j)
      enddo
      enddo

c Get Data from other PEs to ensure reproducibility
c -------------------------------------------------
#if (mpi) 
      peid0 = lattice%pej*lattice%nx
      if( lattice%pei.eq.0 ) then
          i0 = im
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          allocate ( buf( lattice%im(n),jm ) )
          call mpi_recv ( buf,lattice%im(n)*jm,mpi_rkind,peid,peid,lattice%comm,status,ierror )
          do j=1,jm
          do i=1,lattice%im(n)
          q12(i+i0,j) = buf(i,j)
          enddo
          enddo
          deallocate ( buf )
          i0 = i0 + lattice%im(n)
          enddo
      else
          call mpi_send ( q12,im*jm,mpi_rkind,peid0,lattice%myid,lattice%comm,ierror )
      endif

c Begin Dot Product Calculation
c -----------------------------
      if( lattice%pei.eq.0 ) then
#endif

      do j=1,jm
      sum = q12(1,j)
      do i=2,lattice%imglobal
      sum = sum + q12(i,j)
      enddo
      qdot(j) = sum
      enddo

c Send Dot Product to other PEs
c -----------------------------
#if (mpi) 
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          call mpi_send ( qdot,jm,mpi_rkind,peid,peid0,lattice%comm,ierror )
          enddo
      else
          call mpi_recv ( qdot,jm,mpi_rkind,peid0,peid0,lattice%comm,status,ierror )

      endif  ! End PEI   Check
#endif

      return
      end

      subroutine par_sum ( q,qsum,im,jm,lattice )
C***********************************************************************
C  PURPOSE                                                              
C     Compute sum for MPI grid
C
C   q ..... Input  Vector
C   qsum .. Output Sum
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer     im,jm
      real      q(im,jm)
      real   qsum(jm)
      real   qg(lattice%imglobal,jm)
      real, allocatable :: buf(:,:)
#if mpi
      include 'mpif.h'
      integer status(mpi_status_size)
#endif

      real    sum
      integer i,j,n,i0,peid,peid0,ierror, mpi_rkind

      mpi_rkind = lattice%mpi_rkind

c Get Data from other PEs to ensure reproducibility
c -------------------------------------------------
      peid0 = lattice%pej*lattice%nx
      if( lattice%pei.eq.0 ) then
          do j=1,jm
          do i=1,im
          qg(i,j) = q(i,j)
          enddo
          enddo
#if (mpi) 
          i0 = im
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          allocate ( buf( lattice%im(n),jm ) )
          call mpi_recv ( buf,lattice%im(n)*jm,mpi_rkind,peid,peid,lattice%comm,status,ierror )
          do j=1,jm
          do i=1,lattice%im(n)
          qg(i+i0,j) = buf(i,j)
          enddo
          enddo
          deallocate ( buf )
          i0 = i0 + lattice%im(n)
          enddo
      else
          call mpi_send ( q,im*jm,mpi_rkind,peid0,lattice%myid,lattice%comm,ierror )
      endif

c Begin Sum Calculation
c ---------------------
      if( lattice%pei.eq.0 ) then
#endif

      do j=1,jm
      sum = qg(1,j)
      do i=2,lattice%imglobal
      sum = sum + qg(i,j)
      enddo
      qsum(j) = sum
      enddo

c Send Dot Product to other PEs
c -----------------------------
#if (mpi) 
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          call mpi_send ( qsum,jm,mpi_rkind,peid,peid0,lattice%comm,ierror )
          enddo
      else
          call mpi_recv ( qsum,jm,mpi_rkind,peid0,peid0,lattice%comm,status,ierror )
#endif

      endif  ! End PEI   Check

      return
      end

      subroutine zmean ( q,qz,im,jm,undef,lattice )
C***********************************************************************
C  PURPOSE                                                              
C     Compute zonal mean for generalized MPI grid
C
C  Note:  m=0  Mass   Point
C         m=1  U-Wind Point
C  lcheck      Flag   for UNDEF check
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer   im,jm
      real    q(im,jm)
      real   qz(jm)
      real   qg(lattice%imglobal,jm)
      real, allocatable :: buf(:,:)
#if mpi
      include 'mpif.h'
      integer status(mpi_status_size)
#endif

      real isum,qsum,undef
      integer i,j,n,i0,peid,peid0,ierror, mpi_rkind

      mpi_rkind = lattice%mpi_rkind

c Get Data from other PEs to ensure reproducibility
c -------------------------------------------------
      peid0 = lattice%pej*lattice%nx

      if( lattice%pei.eq.0 ) then
          do j=1,jm
          do i=1,im
          qg(i,j) = q(i,j)
          enddo
          enddo
#if (mpi) 
          i0 = im
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          allocate ( buf( lattice%im(n),jm ) )
          call mpi_recv ( buf,lattice%im(n)*jm,mpi_rkind,peid,peid,lattice%comm,status,ierror )
          do j=1,jm
          do i=1,lattice%im(n)
          qg(i+i0,j) = buf(i,j)
          enddo
          enddo
          deallocate ( buf )
          i0 = i0 + lattice%im(n)
          enddo
      else
          call mpi_send ( q,im*jm,mpi_rkind,peid0,lattice%myid,lattice%comm,ierror )
#endif
      endif

c Begin Zonal Mean Calculation
c ----------------------------
      if( lattice%pei.eq.0 ) then

      do j=1,jm
         qsum = 0.0
         isum = 0.0
      do i=1,lattice%imglobal
         if( qg(i,j).ne.undef ) then
         qsum = qsum + qg(i,j)
         isum = isum + 1
         endif
      enddo
         if( isum.ne.0.0 ) then
         qz(j) = qsum/isum
         else
         qz(j) = undef
         endif
      enddo

c Send Zonal Mean Data to other PEs
c ---------------------------------
#if (mpi) 
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          call mpi_send ( qz,jm,mpi_rkind,peid,peid0,lattice%comm,ierror )
          enddo
      else
          call mpi_recv ( qz,jm,mpi_rkind,peid0,peid0,lattice%comm,status,ierror )
#endif

      endif  ! End PEI   Check

      return
      end

      subroutine my_barrier (comm)
      implicit none
      integer  comm,ierror
#if (mpi)
      call timebeg     (' barrier  '  )
      call mpi_barrier (  comm,ierror )
      call timeend     (' barrier  '  )
#endif
      return
      end

      subroutine my_finalize
      implicit none
      integer  ierror
#if (mpi)
      call mpi_finalize (ierror )
#endif
      return
      end

      subroutine my_exit (irc)
      implicit none
      integer irc
#if (mpi)
      integer  ierror
      call system ('touch gcm_error')
      call mpi_finalize (ierror)
#endif
      call exit (irc)
      return
      end

      subroutine printchar (string,lattice)
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      character*(*) string
      integer i
      do i=0,lattice%nx*lattice%ny-1
         if( i.eq.lattice%myid ) print *, 'myid: ',i,string
      call my_barrier (lattice%comm)
      enddo
      return
      end

      subroutine printint (string,n,lattice)
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      character*(*) string
      integer i,n
      do i=0,lattice%nx*lattice%ny-1
         if( i.eq.lattice%myid ) print *, 'myid: ',i,string,n
      call my_barrier (lattice%comm)
      enddo
      return
      end

      subroutine printreal (string,q,num,lattice)
      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      character*(*) string
      integer i,num,n
      real    q(num)
      do i=0,lattice%nx*lattice%ny-1
         if( i.eq.lattice%myid ) print *, 'myid: ',i,string,(q(n),n=1,num)
      call my_barrier (lattice%comm)
      enddo
      return
      end

      subroutine timepri2 (ku,lattice)
C***********************************************************************        
C  Purpose                                                                      
C  -------                                                                      
C     Utility to Print Task Timings
C
C  Argument Description                                                         
C  --------------------                                                          
C     ku ........ Output Unit Number
C
C***********************************************************************        
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *        
C***********************************************************************        

      use dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice

      include 'timer.com'
      integer     , allocatable :: ntasksg(:)
      character*10, allocatable ::  tasksg(:,:)
      real        , allocatable :: cputotg(:,:)
      real        , allocatable ::    dum0(:)
      real        , allocatable ::    dum1(:)

      integer ku

c MPI Utilities
c -------------
#if mpi
      include 'mpif.h'
      integer  status(mpi_status_size)
#endif
      integer  ierror,peid

      integer npes,maxt,i,j,n,k, mpi_rkind

      _UNUSED_DUMMY(ku)
      mpi_rkind = lattice%mpi_rkind

#if mpi
c Get total number of tasks from each processor
c ---------------------------------------------
      npes = lattice%nx*lattice%ny
      allocate ( ntasksg(0:npes-1) )
      if( lattice%myid.ne.0 ) then
          call mpi_send ( ntasks,1,mpi_integer,0,lattice%myid,lattice%comm,ierror )
      else
             ntasksg(0) = ntasks
          do peid = 1,lattice%nx*lattice%ny-1
             call mpi_recv ( ntasksg(peid),1,mpi_logical,peid,peid,lattice%comm,status,ierror )
          enddo 
      endif
      call my_barrier (lattice%comm)

      if( lattice%myid.eq.0 ) then
          maxt = ntasksg(0)
          do peid = 1,lattice%nx*lattice%ny-1
          if( ntasksg(peid).gt.maxt ) maxt = ntasksg(peid)
          enddo
          allocate (  tasksg(maxt,0:npes-1) )
          allocate ( cputotg(maxt,0:npes-1) )
      endif
      call my_barrier (lattice%comm)

      if( lattice%myid.ne.0 ) then
          call mpi_send ( tasks,ntasks*10,mpi_character,0,lattice%myid,lattice%comm,ierror )
      else
             tasksg(1:ntasks,0) = tasks(1:ntasks)
          do peid = 1,lattice%nx*lattice%ny-1
             call mpi_recv ( tasksg(1,peid),ntasksg(peid)*10,mpi_character,peid,peid,lattice%comm,status,ierror )
          enddo 
      endif
      call my_barrier (lattice%comm)

      if( lattice%myid.ne.0 ) then
          call mpi_send ( cputot,ntasks,mpi_rkind,0,lattice%myid,lattice%comm,ierror )
      else
             cputotg(1:ntasks,0) = cputot(1:ntasks)
          do peid = 1,lattice%nx*lattice%ny-1
             call mpi_recv ( cputotg(1,peid),ntasksg(peid),mpi_rkind,peid,peid,lattice%comm,status,ierror )
          enddo 
      endif
      call my_barrier (lattice%comm)

      if( lattice%myid.eq.0 ) then
       allocate ( dum0(0:lattice%nx-1) )
       allocate ( dum1(0:lattice%nx-1) )
      do n=1,ntasks
      print *, tasks(n)
         do j=lattice%ny-1,0,-1
         do i=0,lattice%nx-1
         dum1(i) = 0
         peid = i+j*lattice%nx
            do k=1,ntasksg(peid)
            if( tasksg(k,peid).eq.tasks(n) ) dum1(i) = cputotg(k,peid)
            enddo
         enddo
            if( n.eq.1 ) dum0(:) = dum1(:)
            write(6,1001) ( int(dum1(i)),int(dum1(i)/dum0(i)*100),i=0,lattice%nx-1 )
         enddo
      enddo
      deallocate ( dum0 )
      deallocate ( dum1 )
      endif
      call my_barrier (lattice%comm)

      deallocate ( ntasksg )
 1001 format(1x,20(i5,1x,'(',i3,')',2x))
#endif

      return
      end
