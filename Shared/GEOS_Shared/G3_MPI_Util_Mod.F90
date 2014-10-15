      module G3_MPI_Util_Mod

      use MAPL_Mod
      implicit none

! Define Lattice
! --------------
      type dynamics_lattice_type
           integer            :: imglobal      ! Global  Size in X
           integer            :: jmglobal      ! Global  Size in Y
           integer            :: lm            ! Global  Size in Z (Note: Same as Local Size)
           integer            :: imax          ! Maximum local im
           integer            :: jmax          ! Maximum local jm
           integer            :: comm          ! MPI Communicator
           integer            :: nx,ny         ! Size of PE lattice
           integer            :: pei,pej       ! Relative address of local PE on lattice
           integer            :: myid          ! Absolute address of local PE
           integer            :: mpi_rkind     ! mpi_real or mpi_double_precision based on kind
           integer, pointer   :: npeg(:)       ! Number of pole PE ghosts per processor
           integer, pointer   ::  img(:,:)     ! Number of grid-points associated with each pole PE ghost
           integer, pointer   ::  im0(:,:)     ! Beginning address     associated with each pole PE ghost
           integer, pointer   :: ppeg(:)       ! Relative address of pole PE ghost for each iglobal
           integer, pointer   ::   im(:)       ! Array  of local zonal      dimension for each PE in x
           integer, pointer   ::   jm(:)       ! Array  of local meridional dimension for each PE in y
           integer, pointer   ::    ilocal(:)  ! Array  of  local i-index for global i-location
           integer, pointer   ::    jlocal(:)  ! Array  of  local j-index for global j-location
           integer, pointer   ::   iglobal(:)  ! Array  of global i-index for  local i-location
           integer, pointer   ::   jglobal(:)  ! Array  of global j-index for  local j-location
           integer, pointer   :: peiglobal(:)  ! Relative PE address associated with iglobal
           integer, pointer   :: pejglobal(:)  ! Relative PE address associated with jglobal
      endtype dynamics_lattice_type

interface G3_GMEAN
   module procedure g3_gmean_2d_r4
   module procedure g3_gmean_3d_r4
   module procedure g3_gmean_2d_r8
   module procedure g3_gmean_3d_r8
   module procedure g3_amean_2d_r8
end interface

interface G3_GATHER
   module procedure g3_gather_1d_r4
   module procedure g3_gather_2d_r4
   module procedure g3_gather_3d_r4
   module procedure g3_gather_1d_r8
   module procedure g3_gather_2d_r8
   module procedure g3_gather_3d_r8
end interface

interface G3_SCATTER
   module procedure g3_scatter_1d_r4
   module procedure g3_scatter_2d_r4
   module procedure g3_scatter_3d_r4
   module procedure g3_scatter_1d_r8
   module procedure g3_scatter_2d_r8
   module procedure g3_scatter_3d_r8
end interface

      include 'mpif.h'
      contains

! **********************************************************************
      subroutine create_dynamics_lattice (lattice,comm,IMG,JMG,LM,nx,ny)
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type) lattice
      integer comm,IMG,JMG,LM,nx,ny
      integer n,m

! Lattice%im
! ----------
      if(.not.associated(lattice%im)) then
      allocate ( lattice%im(0:nx-1) )
      do n=0,nx-1
      lattice%im(n) = 0
      enddo
      else
             m=size(lattice%im)
          if(m.ne.nx) then
          print *, 'Allocated Lattice Size (',m,') does not match request (',nx,') for lattice%im!'
          stop
          endif
      endif

! Lattice%jm
! ----------
      if(.not.associated(lattice%jm)) then
      allocate ( lattice%jm(0:ny-1) )
      do n=0,ny-1
      lattice%jm(n) = 0
      enddo
      else
             m=size(lattice%jm)
          if(m.ne.ny) then
          print *, 'Allocated Lattice Size (',m,') does not match request (',ny,') for lattice%jm!'
          stop
          endif
      endif

! Lattice%npeg
! ------------
      if(.not.associated(lattice%npeg)) then
      allocate ( lattice%npeg(0:nx-1) )
      do n=0,nx-1
      lattice%npeg(n) = 0
      enddo
      else
             m=size(lattice%npeg)
          if(m.ne.nx) then
          print *, 'Allocated Lattice Size (',m,') does not match request (',nx,') for lattice%npeg!'
          stop
          endif
      endif

! Initialize Lattice
! ------------------
      call init_dynamics_lattice ( lattice,comm,IMG,JMG,LM )

      return
      end subroutine create_dynamics_lattice

      subroutine init_dynamics_lattice ( lattice,comm,imglobal,jmglobal,lm )
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer comm,imglobal,jmglobal,lm
      real    dummy

      integer  status(mpi_status_size)
      integer  myid,ierror,im,jm,i,j,m,n,npes,rm
      integer  isum,jsum,imx,nx, img, ppe(imglobal)

      call mpi_comm_rank ( comm,myid,ierror )
      call malloc_1d_i ( lattice%ppeg,imglobal )

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

! Initialize lattice%im
! ---------------------
      im = imglobal/lattice%nx
      rm = imglobal-lattice%nx*im
      lattice%imax = im
      do n=0,lattice%nx-1
                      lattice%im(n) = im
      if( n.le.rm-1 ) lattice%im(n) = im+1
                      lattice%imax  = max( lattice%imax,lattice%im(n) )
      enddo

! Initialize lattice%jm
! ---------------------
      jm = jmglobal/lattice%ny
      rm = jmglobal-lattice%ny*jm
      lattice%jmax = jm
      do n=0,lattice%ny-1
                      lattice%jm(n) = jm
      if( n.le.rm-1 ) lattice%jm(n) = jm+1
                      lattice%jmax  = max( lattice%jmax,lattice%jm(n) )
      enddo

! Initialize relative PE address
! ------------------------------
      lattice%pei = mod(myid,lattice%nx)
      lattice%pej =     myid/lattice%nx

! Initialize global index for local locations
! -------------------------------------------
      call malloc_1d_i ( lattice%iglobal,lattice%im(lattice%pei) )
      call malloc_1d_i ( lattice%jglobal,lattice%jm(lattice%pej) )

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

! Initialize local index for global locations
! -------------------------------------------
      call malloc_1d_i ( lattice%ilocal,lattice%imglobal )
      call malloc_1d_i ( lattice%jlocal,lattice%jmglobal )

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

! Initialize relative PE address for global i-j locations
! -------------------------------------------------------
      call malloc_1d_i ( lattice%peiglobal,imglobal )
      call malloc_1d_i ( lattice%pejglobal,jmglobal )

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

! Initialize Pole PE ghosts for each iglobal
! ------------------------------------------
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

! Allocate Lattice%img
! --------------------
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

! Allocate Lattice%im0
! --------------------
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

! Determine Pole PE ghosts for each Processor
! -------------------------------------------
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

#if 0
! Print Lattice Characteristics
! -----------------------------
      if( myid.eq.0 ) then
      print *, 'Number of Processors in X: ',lattice%nx
      print *, 'Number of Processors in Y: ',lattice%ny
      print *
      endif
      do n=0,npes-1
         if( n.eq.myid ) then
         write(6,1000) myid,lattice%pei,lattice%pej,lattice%im(lattice%pei),lattice%jm(lattice%pej)
         endif
      call mpi_barrier (lattice%comm,ierror)
      enddo
      if( myid.eq.npes-1 ) print *
 1000 format(1x,'absolute PE id: ',i4,'  relative PE (i,j): (',i4,',',i4,')  (im,jm) = (',i4,',',i4,')')
#endif

      return
      end subroutine init_dynamics_lattice

! **********************************************************************
      subroutine g3_scatter_1d_r4 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=4)   qglobal( lattice%imglobal )
      real(kind=4)   qlocal ( lattice%im(lattice%myid) )
      integer status(mpi_status_size)
      integer comm
      integer i,n,loc,im,imx,imglobal,myid,npes,ierror,mpi_rkind
      real(kind=4), allocatable ::   buf(:)

      comm      = lattice%comm
      myid      = lattice%myid
      npes      = lattice%nx
      im        = lattice%im(myid)
      imglobal  = lattice%imglobal
      mpi_rkind = mpi_real

      if( myid.eq.0 ) then
      do i=1,im
      qlocal(i) = qglobal(i)
      enddo
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
      endif

      return
      end subroutine g3_scatter_1d_r4

! **********************************************************************
      subroutine g3_scatter_1d_r8 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=8)   qglobal( lattice%imglobal )
      real(kind=8)   qlocal ( lattice%im(lattice%myid) )
      integer status(mpi_status_size)
      integer comm
      integer i,n,loc,im,imx,imglobal,myid,npes,ierror,mpi_rkind
      real(kind=8), allocatable ::   buf(:)

      comm      = lattice%comm
      myid      = lattice%myid
      npes      = lattice%nx
      im        = lattice%im(myid)
      imglobal  = lattice%imglobal
      mpi_rkind = mpi_double_precision

      if( myid.eq.0 ) then
      do i=1,im
      qlocal(i) = qglobal(i)
      enddo
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
      endif

      return
      end subroutine g3_scatter_1d_r8

! **********************************************************************
      subroutine g3_scatter_2d_r4 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=4)   qglobal( lattice%imglobal,lattice%jmglobal )
      real(kind=4)   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )
      integer status(mpi_status_size)
      integer comm
      integer myid,npe,ierror,mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=4), allocatable ::   buf(:,:)

      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal
      mpi_rkind = mpi_real

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
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
          call mpi_recv ( qlocal,im*jm,mpi_rkind,0,myid,comm,status,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine g3_scatter_2d_r4

! **********************************************************************
      subroutine g3_scatter_2d_r8 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=8)   qglobal( lattice%imglobal,lattice%jmglobal )
      real(kind=8)   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )
      integer status(mpi_status_size)
      integer comm
      integer myid,npe,ierror,mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=8), allocatable ::   buf(:,:)

      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal
      mpi_rkind = mpi_double_precision

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
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
          call mpi_recv ( qlocal,im*jm,mpi_rkind,0,myid,comm,status,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine g3_scatter_2d_r8

! **********************************************************************
      subroutine iscatter_2d ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer   qglobal( lattice%imglobal,lattice%jmglobal )
      integer   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )
      integer status(mpi_status_size)
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
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
          call mpi_recv ( qlocal,im*jm,mpi_integer,0,myid,comm,status,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine iscatter_2d

! **********************************************************************
      subroutine g3_scatter_3d_r4 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=4)   qglobal( lattice%imglobal,lattice%jmglobal,lattice%lm )
      real(kind=4)   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )
      integer status(mpi_status_size)
      integer comm,lm,L
      integer myid,npe,ierror,mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=4), allocatable ::   buf(:,:,:)

      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      lm        = lattice%lm
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal
      mpi_rkind = mpi_real

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
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
          call mpi_recv ( qlocal,im*jm*lm,mpi_rkind,0,myid,comm,status,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine g3_scatter_3d_r4

! **********************************************************************
      subroutine g3_scatter_3d_r8 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=8)   qglobal( lattice%imglobal,lattice%jmglobal,lattice%lm )
      real(kind=8)   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )
      integer status(mpi_status_size)
      integer comm,lm,L
      integer myid,npe,ierror,mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=8), allocatable ::   buf(:,:,:)

      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      lm        = lattice%lm
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal
      mpi_rkind = mpi_double_precision

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
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
          call mpi_recv ( qlocal,im*jm*lm,mpi_rkind,0,myid,comm,status,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine g3_scatter_3d_r8

! **********************************************************************
      subroutine g3_gather_1d_r4 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=4)   qglobal( lattice%imglobal )
      real(kind=4)   qlocal ( lattice%im(lattice%myid) )
      integer status(mpi_status_size)
      integer comm, mpi_rkind
      integer i,n,loc,im,imx,myid,npes,ierror

      mpi_rkind = mpi_real
      comm      = lattice%comm
      myid      = lattice%myid
      npes      = lattice%nx
      im        = lattice%im(myid)

      if( myid.eq.0 ) then
      do i=1,im
      qglobal(i) = qlocal(i)
      enddo
      loc = im
      do n=1,npes-1
      imx = lattice%im(n)
      call mpi_recv ( qglobal(1+loc),imx,mpi_rkind,n,n,comm,status,ierror )
      loc = loc + imx
      enddo
      else
      call mpi_send ( qlocal,im,mpi_rkind,0,myid,comm,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine g3_gather_1d_r4

! **********************************************************************
      subroutine g3_gather_1d_r8 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=8)   qglobal( lattice%imglobal )
      real(kind=8)   qlocal ( lattice%im(lattice%myid) )
      integer status(mpi_status_size)
      integer comm, mpi_rkind
      integer i,n,loc,im,imx,myid,npes,ierror

      mpi_rkind = mpi_double_precision
      comm      = lattice%comm
      myid      = lattice%myid
      npes      = lattice%nx
      im        = lattice%im(myid)

      if( myid.eq.0 ) then
      do i=1,im
      qglobal(i) = qlocal(i)
      enddo
      loc = im
      do n=1,npes-1
      imx = lattice%im(n)
      call mpi_recv ( qglobal(1+loc),imx,mpi_rkind,n,n,comm,status,ierror )
      loc = loc + imx
      enddo
      else
      call mpi_send ( qlocal,im,mpi_rkind,0,myid,comm,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine g3_gather_1d_r8

! **********************************************************************
      subroutine g3_gather_2d_r4 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=4)   qglobal( lattice%imglobal,lattice%jmglobal )
      real(kind=4)   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )
      integer status(mpi_status_size)
      integer comm, mpi_rkind
      integer myid,npe,ierror
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=4), allocatable ::   buf(:,:)

      mpi_rkind = mpi_real
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
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
          call mpi_send ( qlocal,im*jm,mpi_rkind,0,myid,comm,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine g3_gather_2d_r4

! **********************************************************************
      subroutine g3_gather_2d_r8 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=8)   qglobal( lattice%imglobal,lattice%jmglobal )
      real(kind=8)   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )
      integer status(mpi_status_size)
      integer comm, mpi_rkind
      integer myid,npe,ierror
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=8), allocatable ::   buf(:,:)

      mpi_rkind = mpi_double_precision
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
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
          call mpi_send ( qlocal,im*jm,mpi_rkind,0,myid,comm,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine g3_gather_2d_r8

! **********************************************************************
      subroutine g3_gather_3d_r4 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=4)   qglobal( lattice%imglobal,lattice%jmglobal,lattice%lm )
      real(kind=4)   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )
      integer status(mpi_status_size)
      integer comm,lm,L
      integer myid,npe,ierror, mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=4), allocatable ::   buf(:,:,:)

      mpi_rkind = mpi_real
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
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
          call mpi_send ( qlocal,im*jm*lm,mpi_rkind,0,myid,comm,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine g3_gather_3d_r4

! **********************************************************************
      subroutine g3_gather_3d_r8 ( qglobal,qlocal,lattice )
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=8)   qglobal( lattice%imglobal,lattice%jmglobal,lattice%lm )
      real(kind=8)   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )
      integer status(mpi_status_size)
      integer comm,lm,L
      integer myid,npe,ierror, mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=8), allocatable ::   buf(:,:,:)

      mpi_rkind = mpi_double_precision
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
             endif
             enddo
          jsum = jsum + jmy
          enddo
      else
          call mpi_send ( qlocal,im*jm*lm,mpi_rkind,0,myid,comm,ierror )
      endif

      call mpi_barrier ( comm,ierror )
      return
      end subroutine g3_gather_3d_r8

! **********************************************************************
      subroutine ghostx (a,b,im,jm,lm,n,lattice,flag)
! **********************************************************************
! ****                                                              ****
! ****  This program fills GHOST values in the Y-direction          ****
! ****                                                              ****
! ****  a ....... Input  Array  Non-Ghosted                         ****
! ****  b ....... Output Array      Ghosted                         ****
! ****  im ...... Local  Zonal      Dimension                       ****
! ****  jm ...... Local  Meridional Dimension                       ****
! ****  lm ...... Local  Vertical   Dimension                       ****
! ****  n ....... Number of GHOST values                            ****
! ****  lattice . Grid Lattice for MPI                              ****
! ****  flag .... Character*(*) to do 'east' or 'west' only         ****
! ****                                                              ****
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer  stat(mpi_status_size,4)
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

      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      nx        = lattice%nx
      undef     = 1.0e15

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
          if( east.eq.west ) then
              call mpi_sendrecv( bufs,2*n*jm*lm,mpi_rkind,east,east, &
                                 bufr,2*n*jm*lm,mpi_rkind,west,myid,comm,stat,ierror )
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

      return
      end subroutine ghostx

! **********************************************************************
      subroutine ghosty (a,b,im,jm,lm,shift,msgn,n,lattice,flag)
! **********************************************************************
! ****                                                              ****
! ****  This program fills GHOST values in the Y-direction          ****
! ****                                                              ****
! ****  a ....... Input  Array  Non-Ghosted                         ****
! ****  b ....... Output Array      Ghosted                         ****
! ****  im ...... Local  Zonal      Dimension                       ****
! ****  jm ...... Local  Meridional Dimension                       ****
! ****  lm ...... Local  Vertical   Dimension                       ****
! ****  shift ... Integer Flag: 0 for A-Grid, 1 for C-Grid VWND     ****
! ****  msgn .... Integer Flag for Scaler (1) or Vector (-1)        ****
! ****  n ....... Number of GHOST values                            ****
! ****  lattice . Grid Lattice for MPI                              ****
! ****  flag .... Character*(*) to do 'north', 'south', or 'pole'   ****
! ****                                                              ****
! **********************************************************************
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer  status(mpi_status_size)
      integer  myid, nx, comm, ierror, mpi_rkind
      character*(*) flag

      integer im,jm,lm,shift,m,n,i,j,L,i0,req(im)
      integer north,south,imx,ibeg,npes,request,msgn
      real    undef

      real a(1:im,  1:jm  ,lm)
      real b(1:im,1-n:jm+n,lm)

      real, allocatable :: apls(:,:,:)
      real, allocatable :: amns(:,:,:)
      real, allocatable ::  buf(:,:)

      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      nx        = lattice%nx
      undef     = 1.0E15

      if( (lattice%pej.eq.0            .and. n.gt.jm-1) .or. & ! Pole values cannot be used for ghosting
          (lattice%pej.eq.lattice%ny-1 .and. n.gt.jm-1) .or. & ! Pole values cannot be used for ghosting
                                            (n.gt.jm) ) then
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

! Ghost South Pole
! ----------------
      if( lattice%pej.eq.0 .and. flag.ne.'north' ) then
          do L=1,lm
          do j=1,n
          do i=1,im
          b(i,1-j,L) = a(i,1+j-shift,L)
          enddo
          enddo
          enddo
      endif

! Ghost North Pole
! ----------------
      if( lattice%pej.eq.lattice%ny-1 .and. flag.ne.'south' ) then
          do L=1,lm
          do j=1,n
          do i=1,im
          b(i,jm+j-shift,L) = a(i,jm-j,L)
          enddo
          enddo
          enddo
      endif

! Ghost Non-Pole Points North
! ---------------------------
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
              call mpi_isend( buf,n*im*lm,mpi_rkind,south,south,comm,request,ierror )
              call mpi_wait ( request,status,ierror )
              deallocate ( buf )
           endif
           if( lattice%pej.eq.0 .and. lattice%pej.ne.lattice%ny-1 ) then
              north = myid + nx
              allocate ( apls(im,n,lm) )
              call mpi_recv ( apls,n*im*lm,mpi_rkind,north,myid,comm,status,ierror )
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
              call mpi_isend( buf,n*im*lm,mpi_rkind,south,south,comm,request,ierror )
              call mpi_recv ( apls,n*im*lm,mpi_rkind,north,myid,comm,status,ierror )
              call mpi_wait ( request,status,ierror )
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

! Ghost Non-Pole Points South
! ---------------------------
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
              call mpi_isend( buf,n*im*lm,mpi_rkind,north,north,comm,request,ierror )
              call mpi_wait ( request,status,ierror )
              deallocate ( buf )
           endif
           if( lattice%pej.eq.lattice%ny-1 .and. lattice%pej.ne.0 ) then
              south = myid - nx
              allocate ( amns(im,n,lm) )
              call mpi_recv ( amns,n*im*lm,mpi_rkind,south,myid,comm,status,ierror )
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
              call mpi_isend( buf,n*im*lm,mpi_rkind,north,north,comm,request,ierror )
              call mpi_recv ( amns,n*im*lm,mpi_rkind,south,myid,comm,status,ierror )
              call mpi_wait ( request,status,ierror )
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

      return
      end subroutine ghosty

      subroutine par_dot ( q1,q2,qdot,im,jm,lattice )
!***********************************************************************
!  PURPOSE                                                              
!     Compute dot product for MPI grid
!
!   q1 .... Input First  Vector
!   q2 .... Input Second Vector
!   qdot .. Output Dot Product
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      type ( dynamics_lattice_type ) lattice
      integer     im,jm
      real     q1(im,jm),q2(im,jm)
      real   qdot(jm)
      real   q12(lattice%imglobal,jm)
      real, allocatable :: buf(:,:)
      integer status(mpi_status_size)

      real    sum
      integer i,j,n,i0,peid,peid0,ierror, mpi_rkind

      mpi_rkind = lattice%mpi_rkind

! Compute Local Product
! ---------------------
      do j=1,jm
      do i=1,im
      q12(i,j) = q1(i,j)*q2(i,j)
      enddo
      enddo

! Get Data from other PEs to ensure reproducibility
! -------------------------------------------------
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

! Begin Dot Product Calculation
! -----------------------------
      if( lattice%pei.eq.0 ) then

      do j=1,jm
      sum = q12(1,j)
      do i=2,lattice%imglobal
      sum = sum + q12(i,j)
      enddo
      qdot(j) = sum
      enddo

! Send Dot Product to other PEs
! -----------------------------
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          call mpi_send ( qdot,jm,mpi_rkind,peid,peid0,lattice%comm,ierror )
          enddo
      else
          call mpi_recv ( qdot,jm,mpi_rkind,peid0,peid0,lattice%comm,status,ierror )

      endif  ! End PEI   Check

      return
      end subroutine par_dot

      subroutine par_sum ( q,qsum,im,jm,lattice )
!***********************************************************************
!  PURPOSE                                                              
!     Compute sum for MPI grid
!
!   q ..... Input  Vector
!   qsum .. Output Sum
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      type ( dynamics_lattice_type ) lattice
      integer     im,jm
      real      q(im,jm)
      real   qsum(jm)
      real   qg(lattice%imglobal,jm)
      real, allocatable :: buf(:,:)
      integer status(mpi_status_size)

      real    sum
      integer i,j,n,i0,peid,peid0,ierror, mpi_rkind

      mpi_rkind = lattice%mpi_rkind

! Get Data from other PEs to ensure reproducibility
! -------------------------------------------------
      peid0 = lattice%pej*lattice%nx
      if( lattice%pei.eq.0 ) then
          do j=1,jm
          do i=1,im
          qg(i,j) = q(i,j)
          enddo
          enddo
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

! Begin Sum Calculation
! ---------------------
      if( lattice%pei.eq.0 ) then

      do j=1,jm
      sum = qg(1,j)
      do i=2,lattice%imglobal
      sum = sum + qg(i,j)
      enddo
      qsum(j) = sum
      enddo

! Send Dot Product to other PEs
! -----------------------------
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          call mpi_send ( qsum,jm,mpi_rkind,peid,peid0,lattice%comm,ierror )
          enddo
      else
          call mpi_recv ( qsum,jm,mpi_rkind,peid0,peid0,lattice%comm,status,ierror )

      endif  ! End PEI   Check

      return
      end subroutine par_sum

      subroutine zmean ( q,qz,im,jm,undef,lattice )
!***********************************************************************
!  PURPOSE                                                              
!     Compute zonal mean for generalized MPI grid
!
!  Note:  m=0  Mass   Point
!         m=1  U-Wind Point
!  lcheck      Flag   for UNDEF check
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      type ( dynamics_lattice_type ) lattice
      integer   im,jm
      real    q(im,jm)
      real   qz(jm)
      real   qg(lattice%imglobal,jm)
      real, allocatable :: buf(:,:)
      integer status(mpi_status_size)

      real isum,qsum,undef
      integer i,j,n,i0,peid,peid0,ierror, mpi_rkind

      mpi_rkind = lattice%mpi_rkind

! Get Data from other PEs to ensure reproducibility
! -------------------------------------------------
      peid0 = lattice%pej*lattice%nx

      if( lattice%pei.eq.0 ) then
          do j=1,jm
          do i=1,im
          qg(i,j) = q(i,j)
          enddo
          enddo
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

! Begin Zonal Mean Calculation
! ----------------------------
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

! Send Zonal Mean Data to other PEs
! ---------------------------------
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          call mpi_send ( qz,jm,mpi_rkind,peid,peid0,lattice%comm,ierror )
          enddo
      else
          call mpi_recv ( qz,jm,mpi_rkind,peid0,peid0,lattice%comm,status,ierror )

      endif  ! End PEI   Check

      return
      end subroutine zmean

      subroutine g3_gmean_2d_r4 ( q,qave4,lattice )
!***********************************************************************
!  PURPOSE                                                              
!     Compute global mean for generalized MPI grid
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      type ( dynamics_lattice_type ) lattice
      real      q( lattice%im(lattice%pei),lattice%jm(lattice%pej) )
      real*8 cosp( lattice%jm(lattice%pej) )
      real*8 qdum
      real*8 qave 
      real   qave4
      real   qg(lattice%imglobal,lattice%jmglobal)
      integer status(mpi_status_size)

      real*8  dlat,dlon,phi
      integer i,j,ierror
      integer im,jm

      im = lattice%im(lattice%pei)
      jm = lattice%jm(lattice%pej)

      dlon = 2_8*MAPL_PI/ lattice%imglobal
      dlat =     MAPL_PI/(lattice%jmglobal-1)
      do j=1,jm
      phi = -0.5_8*MAPL_PI + (lattice%jglobal(j)-1)*dlat
      cosp(j) = dcos(phi)
      enddo

      qdum = 0.0_8
      do j=1,jm
      do i=1,im
          qdum = qdum + q(i,j) * cosp(j)*dlon*dlat
      enddo
      enddo

      qdum = qdum / (4*MAPL_PI)

      call mpi_allreduce ( qdum,qave,1,mpi_double_precision,mpi_sum,lattice%comm,ierror )
      qave4 = qave

      return
      end subroutine g3_gmean_2d_r4

      subroutine g3_gmean_3d_r4 ( q,qave4,dp,lattice )
!***********************************************************************
!  PURPOSE                                                              
!     Compute global mean for generalized MPI grid
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      type ( dynamics_lattice_type ) lattice
      real      q( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )
      real     dp( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )
      real*8 cosp( lattice%jm(lattice%pej) )
      real*8 qdum
      real*8 qave 
      real   qave4
      real   qg(lattice%imglobal,lattice%jmglobal)
      integer status(mpi_status_size)

      real*8  dlat,dlon,phi
      integer i,j,L,ierror
      integer im,jm,lm

      im = lattice%im(lattice%pei)
      jm = lattice%jm(lattice%pej)
      lm = lattice%lm

      dlon = 2_8*MAPL_PI/ lattice%imglobal
      dlat =     MAPL_PI/(lattice%jmglobal-1)
      do j=1,jm
      phi = -0.5_8*MAPL_PI + (lattice%jglobal(j)-1)*dlat
      cosp(j) = dcos(phi)
      enddo

      qdum = 0.0_8
      do L=1,lm
      do j=1,jm
      do i=1,im
          qdum = qdum + q(i,j,L) * dp(i,j,L)*cosp(j)*dlon*dlat
      enddo
      enddo
      enddo

      qdum = qdum / (4*MAPL_PI)

      call mpi_allreduce ( qdum,qave,1,mpi_double_precision,mpi_sum,lattice%comm,ierror )
      qave4 = qave

      return
      end subroutine g3_gmean_3d_r4

      subroutine g3_gmean_2d_r8 ( q,qave,lattice )
!***********************************************************************
!  PURPOSE                                                              
!     Compute global mean for generalized MPI grid
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      type ( dynamics_lattice_type ) lattice
      real      q( lattice%im(lattice%pei),lattice%jm(lattice%pej) )
      real*8 cosp( lattice%jm(lattice%pej) )
      real*8 qdum
      real*8 qave 
      real   qg(lattice%imglobal,lattice%jmglobal)
      integer status(mpi_status_size)

      real*8  dlat,dlon,phi
      integer i,j,ierror
      integer im,jm

      im = lattice%im(lattice%pei)
      jm = lattice%jm(lattice%pej)

      dlon = 2_8*MAPL_PI/ lattice%imglobal
      dlat =     MAPL_PI/(lattice%jmglobal-1)
      do j=1,jm
      phi = -0.5_8*MAPL_PI + (lattice%jglobal(j)-1)*dlat
      cosp(j) = dcos(phi)
      enddo

      qdum = 0.0_8
      do j=1,jm
      do i=1,im
          qdum = qdum + q(i,j) * cosp(j)*dlon*dlat
      enddo
      enddo

      qdum = qdum / (4*MAPL_PI)

      call mpi_allreduce ( qdum,qave,1,mpi_double_precision,mpi_sum,lattice%comm,ierror )

      return
      end subroutine g3_gmean_2d_r8

      subroutine g3_gmean_3d_r8 ( q,qave,dp,lattice )
!***********************************************************************
!  PURPOSE                                                              
!     Compute global mean for generalized MPI grid
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      type ( dynamics_lattice_type ) lattice
      real      q( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )
      real     dp( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )
      real*8 cosp( lattice%jm(lattice%pej) )
      real*8 qdum
      real*8 qave 
      real   qg(lattice%imglobal,lattice%jmglobal)
      integer status(mpi_status_size)

      real*8  dlat,dlon,phi
      integer i,j,L,ierror
      integer im,jm,lm

      im = lattice%im(lattice%pei)
      jm = lattice%jm(lattice%pej)
      lm = lattice%lm

      dlon = 2_8*MAPL_PI/ lattice%imglobal
      dlat =     MAPL_PI/(lattice%jmglobal-1)
      do j=1,jm
      phi = -0.5_8*MAPL_PI + (lattice%jglobal(j)-1)*dlat
      cosp(j) = dcos(phi)
      enddo

      qdum = 0.0_8
      do L=1,lm
      do j=1,jm
      do i=1,im
          qdum = qdum + q(i,j,L) * dp(i,j,L)*cosp(j)*dlon*dlat
      enddo
      enddo
      enddo

      qdum = qdum / (4*MAPL_PI)

      call mpi_allreduce ( qdum,qave,1,mpi_double_precision,mpi_sum,lattice%comm,ierror )

      return
      end subroutine g3_gmean_3d_r8

      subroutine gmean ( q,qave4,dp,im,jm,lm,n,lattice )
!***********************************************************************
!  PURPOSE
!     Compute global mean for generalized MPI grid
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      type ( dynamics_lattice_type ) lattice
      integer   im,jm,lm
      real    q(im,jm,lm)
      real   dp(im,jm,lm)
      real*8 cosp(jm)
      real*8 qdum
      real*8 qave
      real   qave4
      real   qg(lattice%imglobal,lattice%jmglobal)
      integer status(mpi_status_size)

      real*8  dlat,dlon,phi
      integer i,j,L,n,ierror

      dlon = 2_8*MAPL_PI/ lattice%imglobal
      dlat =     MAPL_PI/(lattice%jmglobal-1)
      do j=1,jm
      phi = -0.5_8*MAPL_PI + (lattice%jglobal(j)-1)*dlat
      cosp(j) = dcos(phi)
      enddo

      if( lm.gt.1 ) then
          qdum = 0.0_8
          do L=1,lm
          do j=1,jm
          do i=1,im
          qdum = qdum + q(i,j,L)**n * dp(i,j,L)*cosp(j)*dlon*dlat
          enddo
          enddo
          enddo
      else
          qdum = 0.0_8
          do j=1,jm
          do i=1,im
          qdum = qdum + q(i,j,1)**n * cosp(j)*dlon*dlat
          enddo
          enddo
      endif
      qdum = qdum / (4*MAPL_PI)

      call mpi_allreduce ( qdum,qave,1,mpi_double_precision,mpi_sum,lattice%comm,ierror )
      qave4 = qave

      return
      end subroutine gmean

      subroutine g3_amean_2d_r8 ( q,qave,area,im,jm,comm )
!***********************************************************************
!  PURPOSE                                                              
!     Compute global mean for generalized MPI grid
!
!***********************************************************************
!*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
!***********************************************************************

      implicit none
      integer      im,jm, comm
      real      q( im,jm )
      real   area( im,jm )
      real*8 qdum1
      real*8 qdum2
      real*8 qave1
      real*8 qave2
      real*8 qave 
      integer status(mpi_status_size)

      integer i,j,ierror

      qdum1 = 0.0_8
      qdum2 = 0.0_8
      do j=1,jm
      do i=1,im
          qdum1 = qdum1 + q(i,j)*area(i,j)
          qdum2 = qdum2 +        area(i,j)
      enddo
      enddo

      call mpi_allreduce ( qdum1,qave1,1,mpi_double_precision,mpi_sum,comm,ierror )
      call mpi_allreduce ( qdum2,qave2,1,mpi_double_precision,mpi_sum,comm,ierror )

      qave = qave1 / qave2

      return
      end subroutine g3_amean_2d_r8

      subroutine my_barrier (comm)
      implicit none
      integer  comm,ierror
      call mpi_barrier (  comm,ierror )
      return
      end subroutine my_barrier

      subroutine my_finalize
      implicit none
      integer  ierror
      call mpi_finalize (ierror )
      return
      end subroutine my_finalize

      subroutine my_exit (irc)
      implicit none
      integer irc
      integer  ierror
      call system ('touch gcm_error')
      call mpi_finalize (ierror)
      call exit (irc)
      return
      end subroutine my_exit

      subroutine printchar (string,lattice)
      implicit none
      type ( dynamics_lattice_type ) lattice
      character*(*) string
      integer i
      do i=0,lattice%nx*lattice%ny-1
         if( i.eq.lattice%myid ) print *, 'myid: ',i,string
      call my_barrier (lattice%comm)
      enddo
      return
      end subroutine printchar

      subroutine printint (string,n,lattice)
      implicit none
      type ( dynamics_lattice_type ) lattice
      character*(*) string
      integer i,n
      do i=0,lattice%nx*lattice%ny-1
         if( i.eq.lattice%myid ) print *, 'myid: ',i,string,n
      call my_barrier (lattice%comm)
      enddo
      return
      end subroutine printint

      subroutine printreal (string,q,num,lattice)
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
      end subroutine printreal

      subroutine destroy_dynamics_lattice (lattice)
      implicit none
      type ( dynamics_lattice_type) lattice
      if(associated( lattice%im        )) deallocate ( lattice%im        )
      if(associated( lattice%jm        )) deallocate ( lattice%jm        )
      if(associated( lattice%npeg      )) deallocate ( lattice%npeg      )
      if(associated( lattice%ppeg      )) deallocate ( lattice%ppeg      )
      if(associated( lattice%img       )) deallocate ( lattice%img       )
      if(associated( lattice%im0       )) deallocate ( lattice%im0       )
      if(associated( lattice%ilocal    )) deallocate ( lattice%ilocal    )
      if(associated( lattice%jlocal    )) deallocate ( lattice%jlocal    )
      if(associated( lattice%iglobal   )) deallocate ( lattice%iglobal   )
      if(associated( lattice%jglobal   )) deallocate ( lattice%jglobal   )
      if(associated( lattice%peiglobal )) deallocate ( lattice%peiglobal )
      if(associated( lattice%pejglobal )) deallocate ( lattice%pejglobal )
      return
      end subroutine destroy_dynamics_lattice

      subroutine malloc_1d_r (a,im)
      implicit none
      real(kind=8), dimension(:), pointer :: a
      integer i,im,m
      if(.not.associated(a)) then
      allocate(a(im))
      do i=1,im
      a(i) = 0.0
      enddo
      else
             m=size(a)
          if(m.ne.im) then
          print *, 'Allocated Array Size (',m,') does not match request (',im,')!'  
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_1d_r

      subroutine malloc_2d_r (a,im,jm)
      implicit none
      real(kind=8), dimension(:,:), pointer :: a
      integer i,j,m,im,jm
      if(.not.associated(a)) then
      allocate(a(im,jm))
      do j=1,jm
      do i=1,im
      a(i,j) = 0.0
      enddo
      enddo
      else
             m=size(a)
          if(m.ne.im*jm) then
          print *, 'Allocated Array Size (',m,') does not match request (',im*jm,')!'
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_2d_r

      subroutine malloc_3d_r (a,im,jm,lm)
      implicit none
      real(kind=8), dimension(:,:,:), pointer :: a
      integer i,j,l,im,jm,lm,m
      if(.not.associated(a)) then
      allocate(a(im,jm,lm))
      do l=1,lm
      do j=1,jm
      do i=1,im
      a(i,j,l) = 0.0
      enddo
      enddo
      enddo
      else
             m=size(a)
          if(m.ne.im*jm*lm) then
          print *, 'Allocated Array Size (',m,') does not match request (',im*jm*lm,')!'
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_3d_r

      subroutine malloc_4d_r (a,im,jm,lm,nm)
      implicit none
      real(kind=8), dimension(:,:,:,:), pointer :: a
      integer i,j,l,n,im,jm,lm,nm,m
      if(.not.associated(a)) then
      allocate(a(im,jm,lm,nm))
      do n=1,nm
      do l=1,lm
      do j=1,jm
      do i=1,im
      a(i,j,l,n) = 0.0
      enddo
      enddo
      enddo
      enddo
      else
             m=size(a)
          if(m.ne.im*jm*lm*nm) then
          print *, 'Allocated Array Size (',m,') does not match request (',im*jm*lm*nm,')!'
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_4d_r

      subroutine malloc_1d_i (a,im)
      implicit none
      integer, dimension(:), pointer :: a
      integer i,im,m
      if(.not.associated(a)) then
      allocate(a(im))
      do i=1,im
      a(i) = 0
      enddo
      else
             m=size(a)
          if(m.ne.im) then
          print *, 'Allocated Array Size (',m,') does not match request (',im,')!'
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_1d_i

      subroutine malloc_2d_i (a,im,jm)
      implicit none
      integer, dimension(:,:), pointer :: a
      integer i,j,m,im,jm
      if(.not.associated(a)) then
      allocate(a(im,jm))
      do j=1,jm
      do i=1,im
      a(i,j) = 0
      enddo
      enddo
      else
             m=size(a)
          if(m.ne.im*jm) then
          print *, 'Allocated Array Size (',m,') does not match request (',im*jm,')!'
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_2d_i

      subroutine malloc_3d_i (a,im,jm,lm)
      implicit none
      integer, dimension(:,:,:), pointer :: a
      integer i,j,l,im,jm,lm,m
      if(.not.associated(a)) then
      allocate(a(im,jm,lm))
      do l=1,lm
      do j=1,jm
      do i=1,im
      a(i,j,l) = 0
      enddo
      enddo
      enddo
      else
             m=size(a)
          if(m.ne.im*jm*lm) then
          print *, 'Allocated Array Size (',m,') does not match request (',im*jm*lm,')!'
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_3d_i

      subroutine malloc_4d_i (a,im,jm,lm,nm)
      implicit none
      integer, dimension(:,:,:,:), pointer :: a
      integer i,j,l,n,im,jm,lm,nm,m
      if(.not.associated(a)) then
      allocate(a(im,jm,lm,nm))
      do n=1,nm
      do l=1,lm
      do j=1,jm
      do i=1,im
      a(i,j,l,n) = 0
      enddo
      enddo
      enddo
      enddo
      else
             m=size(a)
          if(m.ne.im*jm*lm*nm) then
          print *, 'Allocated Array Size (',m,') does not match request (',im*jm*lm*nm,')!'
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_4d_i

      subroutine malloc_1d_c8 (a,i)
      implicit none
      character*8, dimension(:), pointer :: a
      integer i,m
      if(.not.associated(a)) then
      allocate(a(i))
      else
             m=size(a)
          if(m.ne.i) then
          print *, 'Allocated Array Size (',m,') does not match request (',i,')!'  
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_1d_c8

      subroutine malloc_2d_c8 (a,i,j)
      implicit none
      character*8, dimension(:,:), pointer :: a
      integer i,j,m
      if(.not.associated(a)) then
      allocate(a(i,j))
      else
             m=size(a)
          if(m.ne.i*j) then
          print *, 'Allocated Array Size (',m,') does not match request (',i*j,')!'
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_2d_c8

      subroutine malloc_3d_c8 (a,i,j,k)
      implicit none
      character*8, dimension(:,:,:), pointer :: a
      integer i,j,k,m
      if(.not.associated(a)) then
      allocate(a(i,j,k))
      else
             m=size(a)
          if(m.ne.i*j*k) then
          print *, 'Allocated Array Size (',m,') does not match request (',i*j*k,')!'
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_3d_c8

      subroutine malloc_4d_c8 (a,i,j,k,l)
      implicit none
      character*8, dimension(:,:,:,:), pointer :: a
      integer i,j,k,l,m
      if(.not.associated(a)) then
      allocate(a(i,j,k,l))
      else
             m=size(a)
          if(m.ne.i*j*k*l) then
          print *, 'Allocated Array Size (',m,') does not match request (',i*j*k*l,')!'
          call my_finalize
          call my_exit (101)
          endif
      endif
      return
      end subroutine malloc_4d_c8

  end module G3_MPI_Util_Mod
