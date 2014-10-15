subroutine GetWeightsC2C( npx_in, npy_in, npx_out, npy_out, index, weight)

 use fv_arrays_mod,  only : REAL4, REAL8, FVPRC
 use CUB2CUB_mod,    only :  get_c2c_weight
 use mpp_mod,        only : mpp_pe
 use mpp_domains_mod, only :  domain2d, mpp_define_layout, mpp_define_domains,  &
      mpp_get_compute_domain, mpp_get_data_domain

 implicit none
  integer, intent(in) :: npx_in, npy_in, npx_out, npy_out
  integer, intent(out) :: index(:,:,:,:)
  real(REAL8), intent(out) :: weight(:,:,:,:)


! local vars
  integer, parameter :: ntiles=6

  type(domain2D) :: domain_i
  integer :: layout(2)
  integer :: npes, mype
  integer :: npy_i, npy_o
  integer :: is, ie, js, je, ls, le
  integer :: is_i,  ie_i,  js_i,  je_i
  integer :: isd_i, ied_i, jsd_i, jed_i
  real(REAL8), pointer :: sph_in(:,:,:,:) => null()
  real(REAL8), pointer :: sph_out(:,:,:,:) => null()

  ! initialize cube_in and cube_out corners
  !----------------------------------------

  npy_i = npy_in/ntiles  ! should be the same as npx_in
  npy_o = npy_out/ntiles ! should be the same as npx_out
  call init_cube_corners(npx_in, npy_i, sph_in)
  call init_cube_corners(npx_out, npy_o, sph_out)

! define a domain (global, on 1 PE), needed for the next call
  npes = 1
  mype = mpp_pe()

  call mpp_define_layout( (/1,npx_in,1,npy_i/), npes, layout )
  call mpp_define_domains( (/1,npx_in,1,npy_i/), layout, domain_i, &
       pelist=(/ mype /), xhalo=1, yhalo=1 )
  call mpp_get_compute_domain( domain_i, is_i,  ie_i,  js_i,  je_i  )
  call mpp_get_data_domain   ( domain_i, isd_i, ied_i, jsd_i, jed_i )

  is = 1
  ie = npx_out
  js = 1
  je = npy_o
  ls = 1
  le = ntiles

  ! calculate weights for bilinear interpolation
  ! from cubed sphere to cubed sphere
  !---------------------------------------------

  call get_c2c_weight(ntiles, npx_in+1, npy_i+1, &
                      is_i, ie_i, js_i, je_i, isd_i, ied_i, jsd_i, jed_i, &
                      sph_in(:,is_i:ie_i+1,js_i:je_i+1,:), &
                      npx_out+1, npy_o+1, is, ie, js, je, ls,le, &
                      sph_out(:,is:ie+1,js:je+1,:), &
                      index,  weight, domain_i)

  deallocate ( sph_in, sph_out )


  return

contains
  subroutine init_cube_corners(npx, npy, sph_corner)

    use fv_grid_utils_mod, only : gnomonic_grids !, cell_center2, mid_pt_sphere
    use fv_grid_tools_mod, only : mirror_grid
    use GHOST_CUBSPH_mod,  only : B_grid, A_grid, ghost_cubsph_update


    integer,  intent(in   ) :: npx,  npy
    real(REAL8), pointer       :: sph_corner (:,:,:,:)

    ! Locals
    !-------

    integer :: npts, n, l, j, j1

    integer, parameter :: ntiles=6
    integer, parameter :: ndims=2


    real, parameter :: PI=3.14159265358979323846

    ! Real*8 are needed to make fv calls.
    !-----------------------------------

    real(REAL8), allocatable :: grid_global(:,:,:,:)

    npts = npx + 1
    
    if (associated(sph_corner)) deallocate(sph_corner)
    allocate( sph_corner(ndims,0:npts+1,0:npts+1,ntiles))
    
    allocate( grid_global(npts,npts,ndims,ntiles))
    
    call gnomonic_grids(A_grid, npx, grid_global(:,:,1,1), grid_global(:,:,2,1))
    
    ! mirror_grid assumes that the tile=1 is centered 
    !   on equator and greenwich meridian Lon[-pi,pi]
    !------------------------------------------------
    
    call mirror_grid(grid_global, 0, npts, npts, ndims, ntiles)
    
    ! Shift the corner away from Japan.
    !  This will result in the corner 
    !  close to the east coast of China.
    !-----------------------------------
    
    grid_global(:,:,1,:) = grid_global(:,:,1,:) - PI/18.
    
    where(grid_global(:,:,1,:) < 0.) &
         grid_global(:,:,1,:) = grid_global(:,:,1,:) + 2.* PI
    
    ! Keep Equator and Greenwich exact
    !---------------------------------
    
    where(abs(grid_global(:,:,:,1)) < 1.e-10) grid_global(:,:,:,1) = 0.0
    
    
    ! Clean Up Corners
    !---------------------------------
    
    grid_global(1   ,   :,:,2)=grid_global(npts     ,:        ,:,1)
    grid_global(1   ,   :,:,3)=grid_global(npts:1:-1,npts     ,:,1)
    grid_global(:   ,npts,:,5)=grid_global(1        ,npts:1:-1,:,1)
    grid_global(:   ,npts,:,6)=grid_global(:        ,1        ,:,1)
    grid_global(:   ,   1,:,3)=grid_global(:        ,npts     ,:,2)
    grid_global(:   ,   1,:,4)=grid_global(npts     ,npts:1:-1,:,2)
    grid_global(npts,   :,:,6)=grid_global(npts:1:-1,1        ,:,2)
    grid_global(1   ,   :,:,4)=grid_global(npts     ,:        ,:,3)
    grid_global(1   ,   :,:,5)=grid_global(npts:1:-1,npts     ,:,3)
    grid_global(npts,   :,:,3)=grid_global(1        ,:        ,:,4)
    grid_global(:   ,   1,:,5)=grid_global(:        ,npts     ,:,4)
    grid_global(:   ,   1,:,6)=grid_global(npts     ,npts:1:-1,:,4)
    grid_global(1   ,   :,:,6)=grid_global(npts     ,:        ,:,5)


    sph_corner(1,1:npts,1:npts,:) = grid_global(:,:,1,:)
    sph_corner(2,1:npts,1:npts,:) = grid_global(:,:,2,:)

    deallocate ( grid_global )

  ! do halo update                                                   !
  !------------------------------------------------------------------!

    do n=1,ntiles
       sph_corner(1:2,0     ,0     ,n)=0.
       sph_corner(1:2,npts+1,0     ,n)=0.
       sph_corner(1:2,0     ,npts+1,n)=0.
       sph_corner(1:2,npts+1,npts+1,n)=0.
       do L=1,2
          call ghost_cubsph_update(sph_corner(L,0:npts+1,0:npts+1,:), &
               0, npts+1, 0, npts+1,           &
               1, ntiles, 1, n, B_grid         )
       end do
    enddo

    return
  end subroutine init_cube_corners


end subroutine GetWeightsC2C
