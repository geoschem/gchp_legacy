      implicit none
      include "netcdf.inc"

      integer timeId, timInc
      integer fid, rc
      integer  :: in_fmode = 1         ! non-zero for READ-ONLY
      integer  iarg, iargc, argc
      character*255 fname, argv

      argc =  iargc()
      if ( argc .lt. 1 ) call exit(1)
      iarg = 1
      call GetArg ( iArg, argv )
      fname = trim(argv)
      fid = ncopn (fname, NCNOWRIT, rc)
      timeId = ncvid (fid, 'time', rc)
      call ncagt     (fid, timeId, 'time_increment', timInc, rc)
      call ncpopt(NCVERBOS)
      call ncclos (fid, rc)
      write(6,'(i6.6)') timInc
      end
