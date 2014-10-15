      implicit none
      include "netcdf.inc"

      integer im, jm, km
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
      call ncdinq (fid, 1, 'lon', im, rc)
      call ncdinq (fid, 2, 'lat', jm, rc)
      call ncdinq (fid, 3, 'lev', km, rc)
      call ncclos (fid, rc)
      print *, im, jm, km
      end
