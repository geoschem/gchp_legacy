      program  main

      implicit none
      include 'alias.com'

! **********************************************************************
! **********************************************************************
! ****                                                              ****
! ****         Program to fix UNDEF values below P_Surface          ****
! ****                                                              ****
! **********************************************************************
! **********************************************************************

      character*256 :: filename,filename1,filename2
      character*256 :: arg(2)
      integer       :: n,nargs,iargc,id,rc

! First File
! ----------
      real     undef
      real,    allocatable ::    lat(:)
      real,    allocatable ::    lon(:)
      real,    allocatable ::    lev(:)
      real,    allocatable :: vrange(:,:)
      real,    allocatable :: prange(:,:)
      integer, allocatable :: yymmdd(:)
      integer, allocatable :: hhmmss(:)
      integer, allocatable ::  kmvar(:)
      integer id1, im,  jm,  lm,  nvars,  ngatts,  ntimes, timinc

      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)

! Second File
! -----------
      real     undef2
      real,    allocatable ::    lat2(:)
      real,    allocatable ::    lon2(:)
      real,    allocatable ::    lev2(:)
      real,    allocatable :: vrange2(:,:)
      real,    allocatable :: prange2(:,:)
      integer, allocatable :: yymmdd2(:)
      integer, allocatable :: hhmmss2(:)
      integer, allocatable ::  kmvar2(:)
      integer id2, im2, jm2, lm2, nvars2, ngatts2, ntimes2, timinc2

      character*256  title2
      character*256  source2
      character*256  contact2
      character*256  levunits2
      character*256, allocatable ::  vname2(:)
      character*256, allocatable :: vtitle2(:)
      character*256, allocatable :: vunits2(:)

! -----------

      real, allocatable :: ps (:,:)
      real, allocatable :: ps2(:,:)
      real, allocatable ::   q(:,:)
      logical found_ps
      logical found_ps2
      character*256 :: PSNAME
      character*256 ::   NAME
  
      integer, allocatable :: checkps(:)
      integer i,j,L,k
      integer nymd,nhms
      integer precision
      integer itype,icount
      real    plev

      precision = 1  ! 64-bit
      precision = 0  ! 32-bit

! **********************************************************************

! READ INPUT
! ----------
  nargs = iargc()
  if( nargs<1 .or. nargs>2 ) then
     call usage()
  end if

  do n=1,nargs
  call getarg(n,arg(n))
  enddo

  read(arg(1),'(a)') filename1

  if( nargs.eq.2 ) then
      read(arg(2),'(a)') filename2
  endif


! READ first file
! ---------------
      call gfio_open      ( trim(filename1),0,id1,rc )
      if( rc.ne.0 ) then
          print *, 'File: ',trim(filename1),' not found!'
          call exit(1)
      endif
      call gfio_diminquire ( id1,im,jm,lm,ntimes,nvars,ngatts,rc )
      if( rc.ne.0 ) then
          print *, 'Failed GFIO_DIMINQUIRE on File: ',trim(filename1)
          call exit(1)
      endif

      allocate ( lon(im) )
      allocate ( lat(jm) )
      allocate ( lev(lm) )
      allocate ( yymmdd(ntimes)  )
      allocate ( hhmmss(ntimes)  )
      allocate (  vname(nvars)   )
      allocate ( vtitle(nvars)   )
      allocate ( vunits(nvars)   )
      allocate (  kmvar(nvars)   )
      allocate ( vrange(2,nvars) )
      allocate ( prange(2,nvars) )

      call gfio_inquire ( id1,im,jm,lm,ntimes,nvars,    &
                          title,source,contact,undef,  &
                          lon,lat,lev,levunits,        &
                          yymmdd,hhmmss,timinc,        &
                          vname,vtitle,vunits,kmvar,   &
                          vrange,prange,rc )
      if( rc.ne.0 ) then
          print *, 'Failed GFIO_INQUIRE on File: ',trim(filename1)
          call exit(1)
      endif

      if( lev(lm).gt.lev(1) ) then
          print *, 'Error!'
          print *, 'File: ',trim(filename1)
          print *, 'contains Levels ordered top -> bottom (Eta?)'
          print *, 'LEVS: ',lev
          print *
	  call exit(1)
      endif

      found_ps = .FALSE.
      do n=1,nvars
         if( match( c_ps,vname(n) ) ) then
                PSNAME = vname(n)
              found_ps = .TRUE.
         endif
      enddo

! READ second file
! ----------------
      if( nargs.eq.2 ) then
      call gfio_open      ( trim(filename2),2,id2,rc )
      if( rc.ne.0 ) then
          print *, 'File: ',trim(filename2),' not found!'
          call exit(1)
      endif
      call gfio_diminquire ( id2,im2,jm2,lm2,ntimes2,nvars2,ngatts2,rc )
      if( rc.ne.0 ) then
          print *, 'Failed GFIO_DIMINQUIRE on File: ',trim(filename2)
          call exit(1)
      endif
      if( im2.ne.im .or. jm2.ne.jm ) then
          print *, 'File Horizontal Dimensions do not match!'
          print *, 'File: ',trim(filename1),' IM: ',im, ' JM: ',jm
          print *, 'File: ',trim(filename2),' IM: ',im2,' JM: ',jm2
          call exit(1)
      endif

      allocate ( lon2(im2) )
      allocate ( lat2(jm2) )
      allocate ( lev2(lm2) )
      allocate ( yymmdd2(ntimes2)  )
      allocate ( hhmmss2(ntimes2)  )
      allocate (  vname2(nvars2)   )
      allocate ( vtitle2(nvars2)   )
      allocate ( vunits2(nvars2)   )
      allocate (  kmvar2(nvars2)   )
      allocate ( vrange2(2,nvars2) )
      allocate ( prange2(2,nvars2) )

      call gfio_inquire ( id2,im2,jm2,lm2,ntimes2,nvars2,    &
                          title2,source2,contact2,undef2,   &
                          lon2,lat2,lev2,levunits2,         &
                          yymmdd2,hhmmss2,timinc2,          &
                          vname2,vtitle2,vunits2,kmvar2,    &
                          vrange2,prange2,rc )

      if( timinc2.ne.timinc ) then
          print *, 'File Time Frequencies do not match!'
          print *, 'File: ',trim(filename1),' TIMINC: ',timinc
          print *, 'File: ',trim(filename2),' TIMINC: ',timinc2
          call exit(1)
      endif

      found_ps2 = .FALSE.
      do n=1,nvars2
         if( match( c_ps,vname2(n) ) ) then
                PSNAME = vname2(n)
             found_ps2 = .TRUE.
         endif
      enddo
      endif

! *************************************************************************

     if( nargs.eq.1 .and. .not.found_ps ) then
          print *, 'Cannot find PS in File: ',trim(filename1)
          call exit(1)
     endif
     if( nargs.eq.2 .and. .not.found_ps .and. .not.found_ps2 ) then
          print *, 'Cannot find PS in File: ',trim(filename1),' or ',trim(filename2)
          call exit(1)
     endif

! *************************************************************************

! Fix UNDEF data
! --------------

     allocate( ps (im,jm) )
     allocate( ps2(im,jm) )
     allocate(   q(im,jm) )

     allocate( checkps(lm) )

     print *
     do n=1,ntimes
        nymd = yymmdd(n)
        nhms = hhmmss(n)
        write(6,100) trim(filename1),nymd,nhms
 100    format(1x,'Processing:  ',a,'  for: ',i8.8,2x,i6.6)

     ! Find PS
     ! -------
        if( found_ps ) then
            call gfio_getvar ( id1,trim(PSNAME),nymd,nhms,im,jm,0, 1,ps,rc )
            if( rc.ne.0 ) then
                print *, 'Failed to get PS from ',trim(filename1),' for: ',nymd,nhms
                call exit(1)
            endif
        endif

        if( nargs.eq.2 ) then
            if( found_ps2 ) then
                call gfio_getvar ( id2,trim(PSNAME),nymd,nhms,im,jm,0, 1,ps2,rc )
                if( rc.ne.0 ) then
                    print *, 'Failed to get PS from ',trim(filename1),' for: ',nymd,nhms
                    call exit(1)
                endif
                if( found_ps ) then
                    checkps(1) = count( ps.ne.ps2 ) 
                    if( checkps(1).ne.0 ) then
                        print *, 'PS from ',trim(filename1),' and ',trim(filename2),' do not match!'
                        call exit(1)
                    endif
                else
                    ps = ps2
                endif
            endif
        endif

     ! Loop over 3D-Variables
     ! ----------------------
       do L=1,lm
             plev    = lev(L)*100
          checkps(L) = count( ps.lt.plev ) 
       enddo

       do k=1,nvars
          if( kmvar(k).eq.lm ) then
              do L=1,lm
              if( checkps(L).ne.0 ) then
                  call gfio_getvar ( id1,trim(vname(k)),nymd,nhms,im,jm,L,1,q,rc )
                  if( rc.ne.0 ) then
                      print *, 'Failed to get ',trim(vname(k)),' for: ',nymd,nhms,' at Level: ',L
                      call exit(1)
                  endif
                  plev = lev(L)*100
                  where( ps.lt.plev ) q = undef
                  call gfio_putvar ( id1,trim(vname(k)),nymd,nhms,im,jm,L,1,q,rc )
                  if( rc.ne.0 ) then
                      print *, 'Failed to write ',trim(vname(k)),' for: ',nymd,nhms,' at Level: ',L
                      call exit(1)
                  endif
              endif
              enddo
          endif
       enddo

     enddo
     print *
                      call gfio_close ( id1,rc )
     if( nargs.eq.2 ) call gfio_close ( id2,rc )
     
     stop
     end

      subroutine usage()
      write(6,100)
 100  format(  "Usage:  "                                                                       ,/  &
                                                                                                ,/  &
               " fix_undef.x   PRS_filename  [PS_filename]"                                     ,/  &
                                                                                                ,/  &
               "where:"                                                                         ,/  &
                                                                                                ,/  &
               "      PRS_filename (required)  is the name of the PRS file which is to be fixed"  ,/  &
               "       PS_filename (optional)  is the name of the file containing PS (if not present in PRS_filename)" ,/  )
      call exit(1)
      end subroutine usage

