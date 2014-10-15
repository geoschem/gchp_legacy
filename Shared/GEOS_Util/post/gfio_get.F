      subroutine gfio_get ( fname,id,im,jm,lm,ntime,nvars,nymdb,nhmsb,nymde,nhmse,ndt,rc )
c     interface
c        subroutine gfio_get ( fname,id,im,jm,lm,ntime,nvars,nymdb,nhmsb,nymde,nhmse,ndt,rc )
c        character (len=*),           intent(IN)  :: fname
c        integer          , optional, intent(OUT) :: id
c        integer          , optional, intent(OUT) :: im
c        integer          , optional, intent(OUT) :: jm
c        integer          , optional, intent(OUT) :: lm
c        integer          , optional, intent(OUT) :: ntime
c        integer          , optional, intent(OUT) :: nvars
c        integer          , optional, intent(OUT) :: nymdb
c        integer          , optional, intent(OUT) :: nhmsb
c        integer          , optional, intent(OUT) :: nymde
c        integer          , optional, intent(OUT) :: nhmse
c        integer          , optional, intent(OUT) :: ndt
c        integer          , optional, intent(OUT) :: rc
c        end subroutine gfio_get
c     end interface
      character (len=*),           intent(IN)  :: fname
      integer          , optional, intent(OUT) :: id
      integer          , optional, intent(OUT) :: im
      integer          , optional, intent(OUT) :: jm
      integer          , optional, intent(OUT) :: lm
      integer          , optional, intent(OUT) :: ntime
      integer          , optional, intent(OUT) :: nvars
      integer          , optional, intent(OUT) :: nymdb
      integer          , optional, intent(OUT) :: nhmsb
      integer          , optional, intent(OUT) :: nymde
      integer          , optional, intent(OUT) :: nhmse
      integer          , optional, intent(OUT) :: ndt
      integer          , optional, intent(OUT) :: rc

      integer idx
      integer imx
      integer jmx
      integer lmx
      integer ntimex
      integer nvarsx
      integer ngatts
      integer ndtx
      integer rcx

      character*256  title
      character*256  source
      character*256  contact
      character*256  levunits
      character*256, allocatable ::  vname(:)
      character*256, allocatable :: vtitle(:)
      character*256, allocatable :: vunits(:)
      real         , allocatable :: lat(:)
      real         , allocatable :: lon(:)
      real         , allocatable :: lev(:)
      real         , allocatable :: vrange(:,:)
      real         , allocatable :: prange(:,:)
      integer      , allocatable ::  kmvar(:)
      integer      , allocatable :: yymmdd(:)
      integer      , allocatable :: hhmmss(:)
      real                       :: undef

      print *, 'Checking file: ',trim(fname)
      call gfio_open       ( trim(fname),1,idx,rcx )
      call gfio_diminquire ( idx,imx,jmx,lmx,ntimex,nvarsx,ngatts,rcx )
      print *, '           im: ',imx
      print *, '           jm: ',jmx
      print *, '           lm: ',lmx
      print *, '        ntime: ',ntimex
      print *, '        nvars: ',nvarsx

      allocate ( lon(imx) )
      allocate ( lat(jmx) )
      allocate ( lev(lmx) )
      allocate ( yymmdd(  ntimex) )
      allocate ( hhmmss(  ntimex) )
      allocate (  vname(  nvarsx) )
      allocate ( vtitle(  nvarsx) )
      allocate ( vunits(  nvarsx) )
      allocate (  kmvar(  nvarsx) )
      allocate ( vrange(2,nvarsx) )
      allocate ( prange(2,nvarsx) )

      call gfio_inquire ( idx,imx,jmx,lmx,ntimex,nvarsx,
     .                    title,source,contact,undef,
     .                    lon,lat,lev,levunits,
     .                    yymmdd,hhmmss,ndtx,
     .                    vname,vtitle,vunits,kmvar,
     .                    vrange,prange,rcx )
      ndtx = nsecf (ndtx)
      print *, '          ndt: ',ndtx

      if(present(rc)   ) rc    = rcx
      if(present(id)   ) id    = idx
      if(present(im)   ) im    = imx
      if(present(jm)   ) jm    = jmx
      if(present(lm)   ) lm    = lmx
      if(present(ndt)  ) ndt   = ndtx
      if(present(ntime)) ntime = ntimex
      if(present(nvars)) nvars = nvarsx
      if(present(nymdb)) nymdb = yymmdd(1)
      if(present(nhmsb)) nhmsb = hhmmss(1)
      if(present(nymde)) nymde = yymmdd(ntime)
      if(present(nhmse)) nhmse = hhmmss(ntime)

      call gfio_close ( idx,rcx )
      return
      end
