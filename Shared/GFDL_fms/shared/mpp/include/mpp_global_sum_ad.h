  subroutine MPP_GLOBAL_SUM_AD_( domain, field, gsum, flags, position, tile_count )
    MPP_TYPE_ :: MPP_GLOBAL_SUM_
    type(domain2D), intent(in) :: domain
    MPP_TYPE_, intent(inout) :: field(:,: MPP_EXTRA_INDICES_ )
    MPP_TYPE_, intent(in) :: gsum
    integer, intent(in), optional :: flags
    integer, intent(in), optional :: position
    integer, intent(in), optional :: tile_count

    integer :: i,j, ioff,joff, isc, iec, jsc, jec, is, ie, js, je, ishift, jshift, ioffset, joffset
    integer :: global_flag, tile, ntile, nlist, n, list, m

    if( domain%max_ntile_pe > MAX_TILES ) call mpp_error(FATAL, "MPP_GLOBAL_SUM: number of tiles is exceed MAX_TILES")
    ntile     = size(domain%x(:))
    nlist     = size(domain%list(:))
    tile = 1
    if(present(tile_count)) tile = tile_count
    global_flag = NON_BITWISE_EXACT_SUM
    if(present(flags)) global_flag = flags

    call mpp_get_domain_shift(domain, ishift, jshift, position)

    if( size(field,1).EQ.domain%x(tile)%compute%size+ishift .AND. size(field,2).EQ.domain%y(tile)%compute%size+jshift )then
!field is on compute domain
        ioff = -domain%x(tile)%compute%begin + 1 
        joff = -domain%y(tile)%compute%begin + 1
    else if( size(field,1).EQ.domain%x(tile)%memory%size+ishift .AND. size(field,2).EQ.domain%y(tile)%memory%size+jshift )then
!field is on data domain
        ioff = -domain%x(tile)%data%begin + 1
        joff = -domain%y(tile)%data%begin + 1
    else
        call mpp_error( FATAL, 'MPP_GLOBAL_SUM_AD_: incoming field array must match either compute domain or data domain.' )
    end if

    if(domain%ntiles > MAX_TILES)  call mpp_error( FATAL,  &
         'MPP_GLOBAL_SUM_AD_: number of tiles on this mosaic is greater than MAXTILES')

    call mpp_get_compute_domain( domain, is,  ie,  js,  je,  tile_count = tile_count )
    isc = is; iec = ie + ishift; jsc = js; jec = je + jshift

    if( global_flag == BITWISE_EXACT_SUM )then
       do j = jsc, jec
          do i = isc, iec
             field(i+ioff:i+ioff,j+joff:j+joff MPP_EXTRA_INDICES_) = gsum
          end do
       end do
    else  !this is not bitwise-exact across different PE counts
       ioffset = domain%x(tile)%loffset*ishift; joffset = domain%y(tile)%loffset*jshift
       field(is+ioff:ie+ioff+ioffset, js+joff:je+joff+joffset MPP_EXTRA_INDICES_) = gsum
    end if

    return
  end subroutine MPP_GLOBAL_SUM_AD_
