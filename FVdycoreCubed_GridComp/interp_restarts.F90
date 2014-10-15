program interp_restarts

  !--------------------------------------------------------------------!
  ! purpose: driver for interpolation of GEOS FV and Moist restarts    !
  !          to the cubed-sphere grid with optional vertical levels    !
  !--------------------------------------------------------------------!
  use ESMF
  use m_set_eta,      only: set_eta
  use mpp_mod,        only: mpp_error, FATAL, NOTE
  use fms_mod,        only: print_memory_usage, fms_init, fms_end, file_exist
  use fv_control_mod, only: fv_init, fv_end
  use fv_arrays_mod,  only: fv_atmos_type, FVPRC, REAL8
  use fv_control_mod, only: npx,npy,npz,ntiles,hydrostatic,Make_NH
  use fv_mp_mod,      only: ng, gid, npes_x, npes_y, masterproc, domain, tile, mp_gather
  use external_ic_mod,only: get_external_ic
  use constants_mod,  only: pi, omega, grav, kappa, rdgas, rvgas, cp_air
  use fv_diagnostics_mod,only: prt_maxmin
  use memutils_mod, only: print_memuse_stats

  implicit none

#include "mpif.h"

  real, parameter:: zvir = rvgas/rdgas - 1.

  type(fv_atmos_type) :: Atm(1)

  character(62) :: fname1, fname2, str_arg
  external :: getarg, iargc
  integer iargc

  real(REAL8) :: dt

  logical :: do_grads = .false.
  real(ESMF_KIND_R4), allocatable :: r4_ll(:,:)
  real(ESMF_KIND_R8), allocatable :: r8_global(:,:,:)
  real(ESMF_KIND_R4), allocatable :: r4_global(:,:,:)
  real(ESMF_KIND_R8), allocatable :: r8_akbk(:)
  real(ESMF_KIND_R8), allocatable :: varo_r8(:,:)
  real(ESMF_KIND_R4), allocatable :: varo_r4(:,:)
  integer i,j,k,l,iq, j1,j2, ks, itmp
  integer im,jm,km,nlon,nlat,nq,ntracers(4)
  real(ESMF_KIND_R8) :: ptop, pint
  real(ESMF_KIND_R8), allocatable :: ua(:,:,:), va(:,:,:)

  integer :: is,ie, js,je
  integer :: status
  integer :: header(6)
  integer :: IUNIT=15
  integer :: GUNIT=16
  integer :: OUNIT=17
  integer :: MUNIT=17

  integer :: record, lsize, gsizes(2), distribs(2), dargs(2), psizes(2)
  integer :: filetype
  integer :: mcol, mrow, irow, jcol, mpiio_rank 
  integer :: rank, total_pes
  integer :: mpistatus(MPI_STATUS_SIZE)
  integer (kind=MPI_OFFSET_KIND) :: offset
  integer (kind=MPI_OFFSET_KIND) :: slice_2d

  logical :: use_mpiio = .false.

  real :: xmod, ymod

  integer :: nmoist,ngocart,npchem,nagcm

! Start up FMS/MPP
  print_memory_usage = .true.
  call fms_init()
  call print_memuse_stats('interp_restarts: fms_init')

  if ( (IARGC() /= 5) .and. (IARGC() /= 7) ) then
     call mpp_error(FATAL, 'ABORT: need 5 arguments number_of_constituents, output_res_x,output_res_y,output_res_z,hydrostatic OR 7 args adding nlon nlat for viewing output with grads')
  endif

  CALL GETARG(1, str_arg)
  read (str_arg,'(I10)') nq
  CALL GETARG(2, str_arg)
  read (str_arg,'(I10)') npx
  npx = npx+1
  CALL GETARG(3, str_arg)
  read (str_arg,'(I10)') npy
  npy = npy+1
  CALL GETARG(4, str_arg)
  read (str_arg,'(I10)') npz
  ntiles = 6
  CALL GETARG(5, str_arg)
  read (str_arg,'(I10)') itmp
  hydrostatic = .true.
  if (itmp == 0) hydrostatic = .false.
  Make_NH = .false.
  if (.not. hydrostatic) Make_NH = .true.

  dt = 1800
  call fv_init(Atm, dt)
  call print_memuse_stats('interp_restarts: fv_init')

! Determine Total Number of Tracers (MOIST, GOCART, PCHEM, ANA)
! -------------------------------------------------------------
  nmoist  = 0
  ngocart = 0
  npchem  = 0
  nagcm   = 0
  if( file_exist("moist_internal_restart_in") ) then
  call rs_count( "moist_internal_restart_in",nmoist )
  endif
  if( file_exist("gocart_internal_restart_in") ) then
  call rs_count( "gocart_internal_restart_in",ngocart )
  endif
  if( file_exist("pchem_internal_restart_in") ) then
  call rs_count( "pchem_internal_restart_in",npchem )
  endif
  if( file_exist("agcm_import_restart_in") ) then
  call rs_count( "agcm_import_restart_in",nagcm )
  endif

  if (npx > 2880) use_mpiio = .false.

  if (use_mpiio) then
  if (gid==0) print*, 'Using MPIIO for fv/moist restarts, BE CAREFULL, Decomposition must be evenly divisble in X and Y dimensions'
  xmod = mod(npx-1,npes_x)
  write(fname1, "(i4.4,' not evenly divisible by ',i4.4)") npx-1, npes_x
  if (xmod /= 0) call mpp_error(FATAL, fname1)
  ymod = mod(npy-1,npes_y)
  write(fname1, "(i4.4,' not evenly divisible by ',i4.4)") npy-1, npes_y
  if (ymod /= 0) call mpp_error(FATAL, fname1)
  endif

  do_grads = .false.
  if (IARGC() == 7) then
     CALL GETARG(6, str_arg)
     read (str_arg,'(I10)') nlon
     CALL GETARG(7, str_arg)
     read (str_arg,'(I10)') nlat
     do_grads = .true.
  endif

! Need to get ak/bk
   if( file_exist("fvcore_internal_restart_in") ) then
    open(IUNIT,file='fvcore_internal_restart_in' ,access='sequential',form='unformatted',status='old')
  ! Headers
    read (IUNIT, IOSTAT=status) header
    read (IUNIT, IOSTAT=status) header(1:5)
    im=header(1)
    jm=header(2)
    km=header(3)
    call set_eta(npz,ks,ptop,pint,Atm(1)%ak,Atm(1)%bk)
    ntracers(1) = nmoist
    ntracers(2) = ngocart
    ntracers(3) = npchem
    ntracers(4) = nagcm
    nmoist  = npz*nmoist/km
    ngocart = npz*ngocart/km
    npchem  = npz*npchem/km
    nagcm   = npz*nagcm/km
    nq = nmoist + ngocart + npchem + nagcm + (npz-1) ! Add (npz-1) to ensure sufficient memory allocation
    nq = nq/npz
    Atm(1)%ncnst = nq
    if( gid==0 ) then
        print *
        write(6,100)
100     format(2x,' k ','      A(k)    ',2x,' B(k)   ',2x,'  Pref    ',2x,'  DelP',/, &
               1x,'----',3x,'----------',2x,'--------',2x,'----------',2x,'---------' )
           k=1
        write(6,101) k,Atm(1)%ak(k)*0.01, Atm(1)%bk(k), Atm(1)%ak(k)*0.01 + 1000.0*Atm(1)%bk(k)
        do k=2,ubound(Atm(1)%ak,1)
        write(6,102) k,Atm(1)%ak(k)*0.01, Atm(1)%bk(k), Atm(1)%ak(k)*0.01 + 1000.0*Atm(1)%bk(k), &
                     (Atm(1)%ak(k)-Atm(1)%ak(k-1))*0.01 + 1000.0*(Atm(1)%bk(k)-Atm(1)%bk(k-1))
        enddo
        print *
101     format(2x,i3,2x,f10.6,2x,f8.4,2x,f10.4)
102     format(2x,i3,2x,f10.6,2x,f8.4,2x,f10.4,3x,f8.4)
103     format(2x,a,i6,3x,a,f7.2,a)
        write(6,103) 'Total Number of Tracers in  MOIST: ',nmoist ,'(/NPZ = ',float(nmoist) /float(npz),')'
        write(6,103) 'Total Number of Tracers in GOCART: ',ngocart,'(/NPZ = ',float(ngocart)/float(npz),')'
        write(6,103) 'Total Number of Tracers in  PCHEM: ',npchem ,'(/NPZ = ',float(npchem) /float(npz),')'
        write(6,103) 'Total Number of Tracers in   AGCM: ',nagcm  ,'(/NPZ = ',float(nagcm)  /float(npz),')'
        print *
    endif
    close(IUNIT)
   endif

  if (gid==0) print*, 'HYDROSTATIC : ', Atm(1)%hydrostatic
  if (gid==0) print*, 'Make_NH     : ', Atm(1)%Make_NH
  if (gid==0) print*, 'Tracers     : ', Atm(1)%ncnst

  call print_memuse_stats('interp_restarts: begining get_external_ic')

  if (jm == 6*im) then
     call get_external_ic( Atm, domain, use_geos_cubed_restart=.true., ntracers=ntracers )
  else
     call get_external_ic( Atm, domain, use_geos_latlon_restart=.true. )
  endif

  is = Atm(1)%isc
  ie = Atm(1)%iec
  js = Atm(1)%jsc
  je = Atm(1)%jec

  call print_memuse_stats('interp_restarts: going to write restarts')

! write fvcore_internal_rst
  if( file_exist("fvcore_internal_restart_in") ) then

 if (use_mpiio) then
  write(fname1, "('fvcore_internal_rst_c',i4.4,'_',i3.3,'L')") npx-1,npz
  if (gid==0) print*, 'Writing with MPIIO: ', TRIM(fname1)
  call MPI_FILE_OPEN(MPI_COMM_WORLD, fname1, MPI_MODE_WRONLY+MPI_MODE_CREATE, MPI_INFO_NULL, MUNIT, STATUS)
  gsizes(1) = (npx-1)
  gsizes(2) = (npy-1) * 6
  distribs(1) = MPI_DISTRIBUTE_BLOCK
  distribs(2) = MPI_DISTRIBUTE_BLOCK
  dargs(1) = MPI_DISTRIBUTE_DFLT_DARG
  dargs(2) = MPI_DISTRIBUTE_DFLT_DARG
  psizes(1) = npes_x
  psizes(2) = npes_y * 6
  call MPI_COMM_SIZE(MPI_COMM_WORLD, total_pes, STATUS)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, STATUS)
  mcol = npes_x
  mrow = npes_y*ntiles
  irow = rank/mcol       !! logical row number
  jcol = mod(rank, mcol) !! logical column number
  mpiio_rank = jcol*mrow + irow
  call MPI_TYPE_CREATE_DARRAY(total_pes, mpiio_rank, 2, gsizes, distribs, dargs, psizes, MPI_ORDER_FORTRAN, MPI_DOUBLE_PRECISION, filetype, STATUS)
  call MPI_TYPE_COMMIT(filetype, STATUS)
  lsize = (ie-is+1)*(je-js+1)
  if (gid==0) then
   ! Write Header Stuff
     open(IUNIT,file='fvcore_internal_restart_in' ,access='sequential',form='unformatted',status='old')
   ! Headers
     read (IUNIT, IOSTAT=status) header
     print*, header
     record = 6*4
     call MPI_FILE_WRITE(MUNIT, record, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
     call MPI_FILE_WRITE(MUNIT, header, 6, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
     call MPI_FILE_WRITE(MUNIT, record, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
     read (IUNIT, IOSTAT=status) header(1:5)
     print*, header(1:5)
     header(1) = (npx-1)
     header(2) = (npy-1)*ntiles
     header(3) = npz
     print*, header(1:5)
     record = 5*4
     call MPI_FILE_WRITE(MUNIT, record,      1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
     call MPI_FILE_WRITE(MUNIT, header(1:5), 5, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
     call MPI_FILE_WRITE(MUNIT, record,      1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
     close(IUNIT)
   ! AK and BK
     allocate ( r8_akbk(npz+1) )
     r8_akbk = Atm(1)%ak
     record = (npz+1)*8
     call MPI_FILE_WRITE(MUNIT, record,      1, MPI_INTEGER         , MPI_STATUS_IGNORE, STATUS)
     call MPI_FILE_WRITE(MUNIT, r8_akbk, npz+1, MPI_DOUBLE_PRECISION, MPI_STATUS_IGNORE, STATUS)
     call MPI_FILE_WRITE(MUNIT, record,      1, MPI_INTEGER         , MPI_STATUS_IGNORE, STATUS)
     r8_akbk = Atm(1)%bk
     record = (npz+1)*8
     call MPI_FILE_WRITE(MUNIT, record,      1, MPI_INTEGER         , MPI_STATUS_IGNORE, STATUS)
     call MPI_FILE_WRITE(MUNIT, r8_akbk, npz+1, MPI_DOUBLE_PRECISION, MPI_STATUS_IGNORE, STATUS)
     call MPI_FILE_WRITE(MUNIT, record,      1, MPI_INTEGER         , MPI_STATUS_IGNORE, STATUS)
     deallocate ( r8_akbk )
  endif
 !offset = 1248 ! sequential access: 4 + INT(6) + 8 + INT(5) + 8 + DBL(NPZ+1) + 8 + DBL(NPZ+1) + 8
 !              !                    4 + 24     + 8 + 20     + 8 + 584        + 8 + 584        + 8 = 1248
  offset =                           4 + 24     + 8 + 20     + 8 + (npz+1)*8  + 8 + (npz+1)*8  + 8
! Write Record Markers
  slice_2d = (npx-1) * (npy-1) * (ntiles)
  record = slice_2d
  if (gid==0) then
      offset=offset-4
      ! U
       do k=1,npz
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + slice_2d*8 + 4
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + 4
       enddo
      ! V
       do k=1,npz
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + slice_2d*8 + 4
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + 4
       enddo
      ! PT
       do k=1,npz
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + slice_2d*8 + 4
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + 4
       enddo
      ! PE
       do k=1,npz+1
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + slice_2d*8 + 4
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + 4
       enddo
      ! PKZ
       do k=1,npz
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + slice_2d*8 + 4
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + 4
       enddo
      ! DZ
       do k=1,npz
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + slice_2d*8 + 4
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + 4
       enddo
      ! W
       do k=1,npz
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + slice_2d*8 + 4
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*8, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + 4
       enddo
  endif
 !offset = 1248 ! sequential access: 4 + INT(6) + 8 + INT(5) + 8 + DBL(NPZ+1) + 8 + DBL(NPZ+1) + 8
 !              !                    4 + 24     + 8 + 20     + 8 + 584        + 8 + 584        + 8 = 1248
  offset =                           4 + 24     + 8 + 20     + 8 + (npz+1)*8  + 8 + (npz+1)*8  + 8
! Write Variables
  allocate ( varo_r8(is:ie,js:je) )
 ! U  
  if (gid==0) print*, offset
  do k=1,npz
     varo_r8 = Atm(1)%u(is:ie,js:je,k)
     call MPI_FILE_SET_VIEW(MUNIT, offset, MPI_DOUBLE_PRECISION, filetype, "native", MPI_INFO_NULL, STATUS)
     call MPI_FILE_WRITE_ALL(MUNIT, varo_r8, lsize, MPI_DOUBLE_PRECISION, mpistatus, STATUS)
     offset = offset + slice_2d*8 + 8
  enddo
 ! V
  if (gid==0) print*, offset, (slice_2d*8 + 8)*npz
  do k=1,npz
     varo_r8 = Atm(1)%v(is:ie,js:je,k)
     call MPI_FILE_SET_VIEW(MUNIT, offset, MPI_DOUBLE_PRECISION, filetype, "native", MPI_INFO_NULL, STATUS)
     call MPI_FILE_WRITE_ALL(MUNIT, varo_r8, lsize, MPI_DOUBLE_PRECISION, mpistatus, STATUS)
     offset = offset + slice_2d*8 + 8
  enddo
 ! PT
  if (gid==0) print*, offset, (slice_2d*8 + 8)*npz
  do k=1,npz
    ! Convert to Potential Temperature
     varo_r8 = Atm(1)%pt(is:ie,js:je,k)/Atm(1)%pkz(is:ie,js:je,k)
    ! Include virtual effect
     varo_r8 = varo_r8 * (1.0 + zvir*Atm(1)%q(is:ie,js:je,k,1))
     call MPI_FILE_SET_VIEW(MUNIT, offset, MPI_DOUBLE_PRECISION, filetype, "native", MPI_INFO_NULL, STATUS)
     call MPI_FILE_WRITE_ALL(MUNIT, varo_r8, lsize, MPI_DOUBLE_PRECISION, mpistatus, STATUS)
     offset = offset + slice_2d*8 + 8
  enddo
 ! PE
  if (gid==0) print*, offset, (slice_2d*8 + 8)*npz
  do k=1,npz+1
     varo_r8 = Atm(1)%pe(is:ie,k,js:je)
     call MPI_FILE_SET_VIEW(MUNIT, offset, MPI_DOUBLE_PRECISION, filetype, "native", MPI_INFO_NULL, STATUS)
     call MPI_FILE_WRITE_ALL(MUNIT, varo_r8, lsize, MPI_DOUBLE_PRECISION, mpistatus, STATUS)
     offset = offset + slice_2d*8 + 8
  enddo
 ! PKZ
  if (gid==0) print*, offset, (slice_2d*8 + 8)*(npz+1)
  do k=1,npz
     varo_r8 = Atm(1)%pkz(is:ie,js:je,k)
     call MPI_FILE_SET_VIEW(MUNIT, offset, MPI_DOUBLE_PRECISION, filetype, "native", MPI_INFO_NULL, STATUS)
     call MPI_FILE_WRITE_ALL(MUNIT, varo_r8, lsize, MPI_DOUBLE_PRECISION, mpistatus, STATUS)
     offset = offset + slice_2d*8 + 8
  enddo
 ! DZ
  if (gid==0) print*, offset, (slice_2d*8 + 8)*npz
  do k=1,npz
     varo_r8 = Atm(1)%delz(is:ie,js:je,k)
     call MPI_FILE_SET_VIEW(MUNIT, offset, MPI_DOUBLE_PRECISION, filetype, "native", MPI_INFO_NULL, STATUS)
     call MPI_FILE_WRITE_ALL(MUNIT, varo_r8, lsize, MPI_DOUBLE_PRECISION, mpistatus, STATUS)
     offset = offset + slice_2d*8 + 8
  enddo
 ! W
  if (gid==0) print*, offset, (slice_2d*8 + 8)*npz
  do k=1,npz
     varo_r8 = Atm(1)%w(is:ie,js:je,k)
     call MPI_FILE_SET_VIEW(MUNIT, offset, MPI_DOUBLE_PRECISION, filetype, "native", MPI_INFO_NULL, STATUS)
     call MPI_FILE_WRITE_ALL(MUNIT, varo_r8, lsize, MPI_DOUBLE_PRECISION, mpistatus, STATUS)
     offset = offset + slice_2d*8 + 8
  enddo
  deallocate ( varo_r8 )
  call MPI_FILE_CLOSE(MUNIT, STATUS)

  else

    open(IUNIT,file='fvcore_internal_restart_in' ,access='sequential',form='unformatted',status='old')
    write(fname1, "('fvcore_internal_rst_c',i4.4,'_',i3.3,'L')") npx-1,npz
    if (gid==0) print*, 'Writing : ', TRIM(fname1)
    if (gid==0) open(OUNIT,file=TRIM(fname1),access='sequential',form='unformatted')

  ! Headers
    read (IUNIT, IOSTAT=status) header 
    write(OUNIT) header 
    if (gid==0) print*, header
    read (IUNIT, IOSTAT=status) header(1:5)
    if (gid==0) print*, header(1:5)  
    header(1) = (npx-1)
    header(2) = (npy-1)*6
    header(3) = npz
    write(OUNIT) header(1:5)
    if (gid==0) print*, header(1:5) 
    close(IUNIT)

  ! AK and BK
    allocate ( r8_akbk(npz+1) )
    r8_akbk = Atm(1)%ak
    write(OUNIT) r8_akbk
    r8_akbk = Atm(1)%bk
    write(OUNIT) r8_akbk
    deallocate ( r8_akbk )

     allocate ( r8_global(npx-1, npy-1, 6) )
     allocate ( varo_r8(npx-1, (npy-1) * 6) )
  ! U
        do k=1,npz
           r8_global(is:ie,js:je,tile) = Atm(1)%u(is:ie,js:je,k)
           call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
           do l=1,6
              j1 = (npy-1)*(l-1) + 1
              j2 = (npy-1)*(l-1) + npy-1
              do j=j1,j2
                 do i=1,npx-1
                    varo_r8(i,j)=r8_global(i,j-j1+1,l)
                 enddo
              enddo
           enddo
           if (gid==0) write(OUNIT) varo_r8
        enddo
  ! V
        do k=1,npz
           r8_global(is:ie,js:je,tile) = Atm(1)%v(is:ie,js:je,k)
           call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
           do l=1,6
              j1 = (npy-1)*(l-1) + 1
              j2 = (npy-1)*(l-1) + npy-1
              do j=j1,j2
                 do i=1,npx-1
                    varo_r8(i,j)=r8_global(i,j-j1+1,l)
                 enddo
              enddo
           enddo
           if (gid==0) write(OUNIT) varo_r8
        enddo
  ! PT
        do k=1,npz
         ! Convert to Potential Temperature
           r8_global(is:ie,js:je,tile) = Atm(1)%pt(is:ie,js:je,k)/Atm(1)%pkz(is:ie,js:je,k)
         ! Include virtual effect
           r8_global(is:ie,js:je,tile) = r8_global(is:ie,js:je,tile)* (1.0 + zvir*Atm(1)%q(is:ie,js:je,k,1))
           call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
           do l=1,6
              j1 = (npy-1)*(l-1) + 1
              j2 = (npy-1)*(l-1) + npy-1
              do j=j1,j2
                 do i=1,npx-1
                    varo_r8(i,j)=r8_global(i,j-j1+1,l)
                 enddo
              enddo
           enddo
           if (gid==0) write(OUNIT) varo_r8
        enddo
  ! PE
        do k=1,npz+1
           r8_global(is:ie,js:je,tile) = Atm(1)%pe(is:ie,k,js:je)
           call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
           do l=1,6
              j1 = (npy-1)*(l-1) + 1
              j2 = (npy-1)*(l-1) + npy-1
              do j=j1,j2
                 do i=1,npx-1
                    varo_r8(i,j)=r8_global(i,j-j1+1,l)
                 enddo
              enddo
           enddo
           if (gid==0) write(OUNIT) varo_r8
        enddo
  ! PKZ
        do k=1,npz
           r8_global(is:ie,js:je,tile) = Atm(1)%pkz(is:ie,js:je,k)
           call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
           do l=1,6
              j1 = (npy-1)*(l-1) + 1
              j2 = (npy-1)*(l-1) + npy-1
              do j=j1,j2
                 do i=1,npx-1
                    varo_r8(i,j)=r8_global(i,j-j1+1,l)
                 enddo
              enddo
           enddo
           if (gid==0) write(OUNIT) varo_r8
        enddo

     if (.not. hydrostatic) then
  ! DZ
        do k=1,npz
           r8_global(is:ie,js:je,tile) = Atm(1)%delz(is:ie,js:je,k)
           call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
           do l=1,6
              j1 = (npy-1)*(l-1) + 1
              j2 = (npy-1)*(l-1) + npy-1
              do j=j1,j2
                 do i=1,npx-1
                    varo_r8(i,j)=r8_global(i,j-j1+1,l)
                 enddo
              enddo
           enddo
           if (gid==0) write(OUNIT) varo_r8
        enddo
  ! W
        do k=1,npz
           r8_global(is:ie,js:je,tile) = Atm(1)%w(is:ie,js:je,k)
           call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
           do l=1,6
              j1 = (npy-1)*(l-1) + 1
              j2 = (npy-1)*(l-1) + npy-1
              do j=j1,j2
                 do i=1,npx-1
                    varo_r8(i,j)=r8_global(i,j-j1+1,l)
                 enddo
              enddo
           enddo
           if (gid==0) write(OUNIT) varo_r8
        enddo
      endif
     if  (gid==0) close (OUNIT)

     deallocate (varo_r8)
     deallocate (r8_global)

  endif


  if (do_grads) then
    write(fname2, "('fvcore_grads_rst_c',i4.4,'_',i3.3,'L')") npx-1,npz
    if (gid==0) print*, 'Writing : ', TRIM(fname2)
    if (gid==0) open(GUNIT,file=TRIM(fname2),access='sequential',form='unformatted')
     allocate ( r8_global(npx-1, npy-1, 6) )
     allocate ( varo_r8(npx-1, (npy-1) * 6) )
     allocate ( r4_ll(nlon, nlat) )
     allocate ( varo_r4(npx-1, (npy-1) * 6) )
     call INTERP_DGRID_TO_AGRID(Atm(1)%u, Atm(1)%v, Atm(1)%ua, Atm(1)%va, rotate=.true.)
    !Atm(1)%ua = Atm(1)%u(is:ie,js:je,:)
    !Atm(1)%va = Atm(1)%v(is:ie,js:je,:)
     do k=1,npz
        r8_global(is:ie,js:je,tile) = Atm(1)%ua(is:ie,js:je,k)
        call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
        do l=1,6
           j1 = (npy-1)*(l-1) + 1
           j2 = (npy-1)*(l-1) + npy-1
           do j=j1,j2
              do i=1,npx-1
                 varo_r8(i,j)=r8_global(i,j-j1+1,l)
              enddo
           enddo
        enddo
        varo_r4 = varo_r8
        call cube2latlon(npx-1, (npy-1)*6, nlon, nlat, varo_r4, r4_ll)
        if (gid==0) write(GUNIT) r4_ll
     enddo
     do k=1,npz
        r8_global(is:ie,js:je,tile) = Atm(1)%va(is:ie,js:je,k)
        call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
        do l=1,6
           j1 = (npy-1)*(l-1) + 1
           j2 = (npy-1)*(l-1) + npy-1
           do j=j1,j2
              do i=1,npx-1
                 varo_r8(i,j)=r8_global(i,j-j1+1,l)
              enddo
           enddo
        enddo
        varo_r4 = varo_r8
        call cube2latlon(npx-1, (npy-1)*6, nlon, nlat, varo_r4, r4_ll)
        if (gid==0) write(GUNIT) r4_ll
     enddo
  ! PT
     do k=1,npz
    ! Convert to Potential Temperature
        r8_global(is:ie,js:je,tile) = Atm(1)%pt(is:ie,js:je,k)/Atm(1)%pkz(is:ie,js:je,k)
    ! Include virtual effect
        r8_global(is:ie,js:je,tile) = r8_global(is:ie,js:je,tile)* (1.0 + zvir*Atm(1)%q(is:ie,js:je,k,1))
        call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
        do l=1,6
           j1 = (npy-1)*(l-1) + 1
           j2 = (npy-1)*(l-1) + npy-1
           do j=j1,j2
              do i=1,npx-1
                 varo_r8(i,j)=r8_global(i,j-j1+1,l)
              enddo
           enddo
        enddo
        varo_r4 = varo_r8
        call cube2latlon(npx-1, (npy-1)*6, nlon, nlat, varo_r4, r4_ll)
        if (gid==0) write(GUNIT) r4_ll
     enddo
   ! SLP
        call get_sea_level_pressure(Atm(1)%pt, Atm(1)%q, Atm(1)%pe, Atm(1)%pk, Atm(1)%phis, r8_global(is:ie,js:je,tile))
        call mp_gather(r8_global, is, ie, js, je, npx-1, npy-1, 6)
        do l=1,6
           j1 = (npy-1)*(l-1) + 1
           j2 = (npy-1)*(l-1) + npy-1
           do j=j1,j2
              do i=1,npx-1
                 varo_r8(i,j)=r8_global(i,j-j1+1,l)
              enddo
           enddo
        enddo
        varo_r4 = varo_r8
        call cube2latlon(npx-1, (npy-1)*6, nlon, nlat, varo_r4, r4_ll)
        if (gid==0) write(GUNIT) r4_ll
     if (gid==0) close (GUNIT)
     deallocate (varo_r8)
     deallocate (r8_global)
     deallocate (varo_r4)
     deallocate (r4_ll)
   endif

  endif

 if (use_mpiio) then

! write moist_internal_rst
  if( file_exist("moist_internal_restart_in") ) then
  write(fname1, "('moist_internal_rst_c',i4.4,'_',i3.3,'L')") npx-1,npz
  if (gid==0) print*, 'Writing with MPIIO: ', TRIM(fname1), nmoist/npz, npz
  call MPI_FILE_OPEN(MPI_COMM_WORLD, fname1, MPI_MODE_WRONLY+MPI_MODE_CREATE, MPI_INFO_NULL, MUNIT, STATUS)
  gsizes(1) = (npx-1)
  gsizes(2) = (npy-1) * 6
  distribs(1) = MPI_DISTRIBUTE_BLOCK
  distribs(2) = MPI_DISTRIBUTE_BLOCK
  dargs(1) = MPI_DISTRIBUTE_DFLT_DARG
  dargs(2) = MPI_DISTRIBUTE_DFLT_DARG
  psizes(1) = npes_x
  psizes(2) = npes_y * 6
  call MPI_COMM_SIZE(MPI_COMM_WORLD, total_pes, STATUS)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, STATUS)
  mcol = npes_x
  mrow = npes_y*ntiles
  irow = rank/mcol       !! logical row number
  jcol = mod(rank, mcol) !! logical column number
  mpiio_rank = jcol*mrow + irow
  call MPI_TYPE_CREATE_DARRAY(total_pes, mpiio_rank, 2, gsizes, distribs, dargs, psizes, MPI_ORDER_FORTRAN, MPI_REAL, filetype, STATUS)
  call MPI_TYPE_COMMIT(filetype, STATUS)
  lsize = (ie-is+1)*(je-js+1)
! Write Record Markers
  slice_2d = (npx-1) * (npy-1) * ntiles
  record = slice_2d
  if (gid==0) then
      offset=0
      do iq=1,nmoist/npz
       do k=1,npz
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*4, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + slice_2d*4 + 4
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*4, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + 4
       enddo
      enddo
  endif
  offset = 4
! Write Variables
  allocate ( varo_r4(is:ie,js:je) )
  do iq=1,nmoist/npz
   do k=1,npz
     varo_r4 = Atm(1)%q(is:ie,js:je,k,iq)
     call MPI_FILE_SET_VIEW(MUNIT, offset, MPI_REAL, filetype, "native", MPI_INFO_NULL, STATUS)
     call MPI_FILE_WRITE_ALL(MUNIT, varo_r4, lsize, MPI_REAL, mpistatus, STATUS)
     offset = offset + slice_2d*4 + 8
   enddo
   if (gid==0) print*, offset
  enddo
  deallocate ( varo_r4 )
  call MPI_FILE_CLOSE(MUNIT, STATUS)
  endif

! write gocart_internal_rst
  if( file_exist("gocart_internal_restart_in") ) then
  write(fname1, "('gocart_internal_rst_c',i4.4,'_',i3.3,'L')") npx-1,npz
  if (gid==0) print*, 'Writing with MPIIO: ', TRIM(fname1), ngocart/npz, npz
  call MPI_FILE_OPEN(MPI_COMM_WORLD, fname1, MPI_MODE_WRONLY+MPI_MODE_CREATE, MPI_INFO_NULL, MUNIT, STATUS)
  gsizes(1) = (npx-1)
  gsizes(2) = (npy-1) * 6
  distribs(1) = MPI_DISTRIBUTE_BLOCK
  distribs(2) = MPI_DISTRIBUTE_BLOCK
  dargs(1) = MPI_DISTRIBUTE_DFLT_DARG
  dargs(2) = MPI_DISTRIBUTE_DFLT_DARG
  psizes(1) = npes_x
  psizes(2) = npes_y * 6
  call MPI_COMM_SIZE(MPI_COMM_WORLD, total_pes, STATUS)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, STATUS)
  mcol = npes_x
  mrow = npes_y*ntiles
  irow = rank/mcol       !! logical row number
  jcol = mod(rank, mcol) !! logical column number
  mpiio_rank = jcol*mrow + irow
  call MPI_TYPE_CREATE_DARRAY(total_pes, mpiio_rank, 2, gsizes, distribs, dargs, psizes, MPI_ORDER_FORTRAN, MPI_REAL, filetype, STATUS)
  call MPI_TYPE_COMMIT(filetype, STATUS)
  lsize = (ie-is+1)*(je-js+1)
! Write Record Markers
  slice_2d = (npx-1) * (npy-1) * ntiles
  record = slice_2d
  if (gid==0) then
      offset=0
      do iq=1,ngocart/npz
       do k=1,npz
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*4, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + slice_2d*4 + 4
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*4, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + 4
       enddo
      enddo
  endif
  offset = 4
! Write Variables
  allocate ( varo_r4(is:ie,js:je) )
  do iq=1,ngocart/npz
   do k=1,npz
     varo_r4 = Atm(1)%q(is:ie,js:je,k,iq+nmoist/npz)
     call MPI_FILE_SET_VIEW(MUNIT, offset, MPI_REAL, filetype, "native", MPI_INFO_NULL, STATUS)
     call MPI_FILE_WRITE_ALL(MUNIT, varo_r4, lsize, MPI_REAL, mpistatus, STATUS)
     offset = offset + slice_2d*4 + 8
   enddo
   if (gid==0) print*, offset
  enddo
  deallocate ( varo_r4 )
  call MPI_FILE_CLOSE(MUNIT, STATUS)
  endif

! write pchem_internal_rst
  if( file_exist("pchem_internal_restart_in") ) then
  write(fname1, "('pchem_internal_rst_c',i4.4,'_',i3.3,'L')") npx-1,npz
  if (gid==0) print*, 'Writing with MPIIO: ', TRIM(fname1)
  call MPI_FILE_OPEN(MPI_COMM_WORLD, fname1, MPI_MODE_WRONLY+MPI_MODE_CREATE, MPI_INFO_NULL, MUNIT, STATUS)
  gsizes(1) = (npx-1)
  gsizes(2) = (npy-1) * 6
  distribs(1) = MPI_DISTRIBUTE_BLOCK
  distribs(2) = MPI_DISTRIBUTE_BLOCK
  dargs(1) = MPI_DISTRIBUTE_DFLT_DARG
  dargs(2) = MPI_DISTRIBUTE_DFLT_DARG
  psizes(1) = npes_x
  psizes(2) = npes_y * 6
  call MPI_COMM_SIZE(MPI_COMM_WORLD, total_pes, STATUS)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, STATUS)
  mcol = npes_x
  mrow = npes_y*ntiles
  irow = rank/mcol       !! logical row number
  jcol = mod(rank, mcol) !! logical column number
  mpiio_rank = jcol*mrow + irow
  call MPI_TYPE_CREATE_DARRAY(total_pes, mpiio_rank, 2, gsizes, distribs, dargs, psizes, MPI_ORDER_FORTRAN, MPI_REAL, filetype, STATUS)
  call MPI_TYPE_COMMIT(filetype, STATUS)
  lsize = (ie-is+1)*(je-js+1)
! Write Record Markers
  slice_2d = (npx-1) * (npy-1) * ntiles
  record = slice_2d
  if (gid==0) then
      offset=0
      do iq=1,npchem/npz
       do k=1,npz
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*4, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + slice_2d*4 + 4
         call MPI_FILE_WRITE_AT(MUNIT, offset, record*4, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
         offset = offset + 4
       enddo
      enddo
  endif
  offset = 4
! Write Variables
  allocate ( varo_r4(is:ie,js:je) )
  do iq=1,npchem/npz
   do k=1,npz
     varo_r4 = Atm(1)%q(is:ie,js:je,k,iq+(nmoist+ngocart)/npz)
     call MPI_FILE_SET_VIEW(MUNIT, offset, MPI_REAL, filetype, "native", MPI_INFO_NULL, STATUS)
     call MPI_FILE_WRITE_ALL(MUNIT, varo_r4, lsize, MPI_REAL, mpistatus, STATUS)
     offset = offset + slice_2d*4 + 8
   enddo
   if (gid==0) print*, offset
  enddo
  deallocate ( varo_r4 )
  call MPI_FILE_CLOSE(MUNIT, STATUS)
  endif

 else

   allocate ( r4_global(npx-1, npy-1, 6) )
   allocate ( r8_global(npx-1, npy-1, 6) )
   allocate ( varo_r4(npx-1, (npy-1) * 6) )

!
! MOIST
!
   if( file_exist("moist_internal_restart_in") ) then
     write(fname1, "('moist_internal_rst_c',i4.4,'_',i3.3,'L')") npx-1,npz
     if (gid==0) print*, 'Writing : ', TRIM(fname1), nmoist/npz, npz
     if (gid==0) open(OUNIT,file=fname1,access='sequential',form='unformatted')
     do iq=1,nmoist
           r4_global(is:ie,js:je,tile) = Atm(1)%q(is:ie,js:je,iq,1)
           call mp_gather(r4_global, is, ie, js, je, npx-1, npy-1, 6)
           if (gid==0) then ! DSK global variable only valid on masterproc
              do l=1,6
                 j1 = (npy-1)*(l-1) + 1
                 j2 = (npy-1)*(l-1) + npy-1
                 do j=j1,j2
                    do i=1,npx-1
                       varo_r4(i,j)=r4_global(i,j-j1+1,l)
                    enddo
                 enddo
              enddo
              write(OUNIT) varo_r4
           endif
     enddo
     if (gid==0) close (OUNIT)
   endif
!
!  GOCART
!
   if( file_exist("gocart_internal_restart_in") ) then
     write(fname1, "('gocart_internal_rst_c',i4.4,'_',i3.3,'L')") npx-1,npz
     if (gid==0) print*, 'Writing : ', TRIM(fname1), ngocart/npz, npz
     if (gid==0) open(OUNIT,file=fname1,access='sequential',form='unformatted')
     do iq=1,ngocart
           r4_global(is:ie,js:je,tile) = Atm(1)%q(is:ie,js:je,iq+nmoist,1)
           call mp_gather(r4_global, is, ie, js, je, npx-1, npy-1, 6)
           if (gid==0) then ! DSK global variable only valid on masterproc
              do l=1,6
                 j1 = (npy-1)*(l-1) + 1
                 j2 = (npy-1)*(l-1) + npy-1
                 do j=j1,j2
                    do i=1,npx-1
                       varo_r4(i,j)=r4_global(i,j-j1+1,l)
                    enddo
                 enddo
              enddo
              write(OUNIT) varo_r4
           endif
     enddo
     if (gid==0) close (OUNIT)
   endif

!
!  PCHEM
!
   if( file_exist("pchem_internal_restart_in") ) then
     write(fname1, "('pchem_internal_rst_c',i4.4,'_',i3.3,'L')") npx-1,npz
     if (gid==0) print*, 'Writing : ', TRIM(fname1), npchem/npz, npz
     if (gid==0) open(OUNIT,file=fname1,access='sequential',form='unformatted')
     do iq=1,npchem
           r4_global(is:ie,js:je,tile) = Atm(1)%q(is:ie,js:je,iq+nmoist+ngocart,1)
           call mp_gather(r4_global, is, ie, js, je, npx-1, npy-1, 6)
           if (gid==0) then ! DSK global variable only valid on masterproc
              do l=1,6
                 j1 = (npy-1)*(l-1) + 1
                 j2 = (npy-1)*(l-1) + npy-1
                 do j=j1,j2
                    do i=1,npx-1
                       varo_r4(i,j)=r4_global(i,j-j1+1,l)
                    enddo
                 enddo
              enddo
              write(OUNIT) varo_r4
           endif
     enddo
     if (gid==0) close (OUNIT)
   endif

!
!  AGCM
!
   if( file_exist("agcm_import_restart_in") ) then
     write(fname1, "('agcm_import_rst_c',i4.4,'_',i3.3,'L')") npx-1,npz
     if (gid==0) print*, 'Writing : ', TRIM(fname1), nagcm/npz, npz
     if (gid==0) open(OUNIT,file=fname1,access='sequential',form='unformatted')
     do iq=1,nagcm
           r4_global(is:ie,js:je,tile) = Atm(1)%q(is:ie,js:je,iq+nmoist+ngocart+npchem,1)
           call mp_gather(r4_global, is, ie, js, je, npx-1, npy-1, 6)
           if (gid==0) then ! DSK global variable only valid on masterproc
              do l=1,6
                 j1 = (npy-1)*(l-1) + 1
                 j2 = (npy-1)*(l-1) + npy-1
                 do j=j1,j2
                    do i=1,npx-1
                       varo_r4(i,j)=r4_global(i,j-j1+1,l)
                    enddo
                 enddo
              enddo
              write(OUNIT) varo_r4
           endif
     enddo
     if (gid==0) close (OUNIT)
   endif

   deallocate (varo_r4)
   deallocate (r4_global)
   deallocate (r8_global)

 endif

  call fv_end(Atm)
  call fms_end()

  contains

subroutine INTERP_DGRID_TO_AGRID(u, v, ua, va, rotate)
! 
  use fv_mp_mod, only: is, ie, js, je, isd, ied, jsd, jed
  use fv_grid_tools_mod,  only: dx, dy, rdxa, rdya
  use fv_arrays_mod, only: FVPRC, REAL8
  use mpp_domains_mod, only: mpp_get_boundary, mpp_update_domains, DGRID_NE
  use sw_core_mod, only: d2a2c_vect
   use fv_grid_utils_mod,  only: cubed_to_latlon
! !INPUT PARAMETERS:
  real(REAL8), intent(INOUT) ::  u(isd:ied,jsd:jed+1,1:npz)
  real(REAL8), intent(INOUT) ::  v(isd:ied+1,jsd:jed,1:npz)
  logical, optional, intent(IN) :: rotate
! 
! !OUTPUT PARAMETERS:
  real(FVPRC), intent(OUT) :: ua(isd:ied,jsd:jed,1:npz)
  real(FVPRC), intent(OUT) :: va(isd:ied,jsd:jed,1:npz)
!
! !DESCRIPTION:
! 
! !LOCAL VARIABLES:
  real(REAL8) :: wbuffer(js:je,npz)
  real(REAL8) :: sbuffer(is:ie,npz)
  real(REAL8) :: ebuffer(js:je,npz)
  real(REAL8) :: nbuffer(is:ie,npz)

  integer i,j,k

  real(REAL8) :: p1(2), p2(2), p3(2), p4(2)

  real(FVPRC) :: ut(isd:ied, jsd:jed)
  real(FVPRC) :: vt(isd:ied, jsd:jed)
  real(FVPRC) :: uc(isd:ied+1,jsd:jed,1:npz)
  real(FVPRC) :: vc(isd:ied,jsd:jed+1,1:npz)

!EOP
!------------------------------------------------------------------------------
!BOC

  call mpp_get_boundary(u, v, domain, &
                        wbuffery=wbuffer, ebuffery=ebuffer, &
                        sbufferx=sbuffer, nbufferx=nbuffer, &
                        gridtype=DGRID_NE )
   do k=1,npz
      do i=is,ie
         u(i,je+1,k) = nbuffer(i,k)
      enddo
   enddo
   do k=1,npz
      do j=js,je
         v(ie+1,j,k) = ebuffer(j,k)
      enddo
  enddo
  call mpp_update_domains(u, v, domain, gridtype=DGRID_NE)

  do k=1,npz
     call d2a2c_vect(u(:,:,k),  v(:,:,k), &
                     ua(:,:,k), va(:,:,k), &
                     uc, vc, ut, vt, .false.)
  enddo
  if (present(rotate)) then
     if (rotate) call cubed_to_latlon(u, v, ua, va, &
                                      dx, dy, rdxa, rdya, npz)
  endif

  return
end subroutine INTERP_DGRID_TO_AGRID

 subroutine get_sea_level_pressure(pt, q, pe, pk, phis, slp)
  use fv_mp_mod, only: is, ie, js, je, ng
  use fv_arrays_mod, only: FVPRC, REAL8
 real(REAL8), intent(in):: pt(is-ng:ie+ng,js-ng:je+ng,npz)
 real(REAL8), intent(in)::  q(is-ng:ie+ng,js-ng:je+ng,npz,*) ! water vapor
 real(REAL8), intent(in):: pe(is-1 :ie+1 ,npz+1,js-1 :je+ 1)
 real(REAL8), intent(in):: pk(is:ie,js:je,npz+1)
 real(REAL8), intent(in):: phis(is-ng:ie+ng,js-ng:je+ng)
 real(REAL8), intent(out):: slp(is:ie,js:je)      ! pressure (pa)
! local:
 real tstar                 ! extrapolated temperature (K)
 real p_bot
 real tref                   ! Reference virtual temperature (K)
 real pref                   ! Reference pressure level (Pa)
 real pkref                  ! Reference pressure level (Pa) ** kappa
 real dp1, dp2

 real, parameter :: gamma    = 6.5e-3
 real, parameter :: p_offset = 15000.
 real, parameter :: gg       = gamma/grav

 real, parameter :: factor   = grav / ( rdgas * gamma )
 real, parameter :: yfactor  = rdgas * gg

 integer k_bot, k, k1, k2
             
 do j=js,je
    do i=is,ie
      p_bot = pe(i,npz+1,j) - p_offset
      k_bot = -1
      do k = npz, 2, -1
         if ( pe(i,k+1,j) .lt. p_bot ) then
              k_bot = k
              exit
         endif             
      enddo
      k1    = k_bot - 1    
      k2    = k_bot
      dp1   = pe(i,k_bot,j)   - pe(i,k_bot-1,j)
      dp2   = pe(i,k_bot+1,j) - pe(i,k_bot,j)
      pkref = ( pk(i,j,k1)*dp1 + pk(i,j,k2)*dp2 ) / (dp1+dp2)
       tref = ( pt(i,j,k1)*(1.+zvir*q(i,j,k1,1))*dp1 + pt(i,j,k2)*(1.+zvir*q(i,j,k2,1))*dp2 ) / (dp1+dp2)
       pref = 0.5 * ( pe(i,k_bot+1,j) + pe(i,k_bot-1,j) )
      tstar = tref*( pe(i,npz+1,j)/pref )**yfactor
      slp(i,j) = pe(i,npz+1,j)*( 1.0+gg*phis(i,j)/tstar )**factor
    enddo
 enddo  

 end subroutine get_sea_level_pressure

      subroutine rs_count( filename,nrecs )
      implicit none
      integer        nrecs,rc,n
      character(*)   filename

      open  (55,file=trim(filename),form='unformatted',access='sequential')
             nrecs =  0
                rc =  0
      do while (rc.eq.0)
         read (55,iostat=rc)
      if( rc.eq.0 ) nrecs = nrecs + 1
      enddo
      close (55)

      return
  end subroutine rs_count

end program interp_restarts
