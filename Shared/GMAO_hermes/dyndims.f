      implicit none
      include "netcdf.inc"

      integer im, jm, km
      integer fid, rc
      integer  iarg, iargc, argc
      character*255 fname, argv

      argc =  command_argument_count()
      if ( argc .lt. 1 ) stop 1
      iarg = 1
      call get_command_argument ( iArg, argv )
      fname = trim(argv)
      fid = ncopn (fname, NCNOWRIT, rc)
      call ncdinq (fid, 1, 'lon', im, rc)
      call ncdinq (fid, 2, 'lat', jm, rc)
      call ncdinq (fid, 3, 'lev', km, rc)
      call ncclos (fid, rc)
      print *, im, jm, km
      end
