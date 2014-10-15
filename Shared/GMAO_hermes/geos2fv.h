      integer, parameter :: READ_ONLY = 1

      integer RESTART, GRADS, GFIO
      parameter ( RESTART =  1)
      parameter ( GRADS = 2 )
      parameter ( GFIO = 3 )

      character*255 ifname, ofname, tfname, sfname ! file names from command line

      common / file_names / ifname, ofname, tfname, sfname
      common / file_info /  ftype, prec 

      integer prec, ftype                  ! precision: 0=32 bits or 1=64 bits

     
