      implicit none
      include "netcdf.inc"

      integer timeId, timInc
      integer fid, rc
      integer  iarg, argc
      character*255 fname, argv

      argc =  command_argument_count()
      if ( argc .lt. 1 ) stop 1
      iarg = 1
      call get_command_argument ( iArg, argv )
      fname = trim(argv)
      fid = ncopn (fname, NCNOWRIT, rc)
      timeId = ncvid (fid, 'time', rc)
      call ncagt     (fid, timeId, 'time_increment', timInc, rc)
      call ncpopt(NCVERBOS)
      call ncclos (fid, rc)
      write(6,'(i6.6)') timInc
      end
