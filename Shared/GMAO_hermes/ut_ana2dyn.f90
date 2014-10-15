!
! Simple unit tester for m_ana2dyn.
!

    program ut_ana2dyn

    use m_dyn
    use m_insitu
    use m_ana2dyn

    implicit NONE

    character(len=*), parameter :: myname = 'ut_ana2dyn'
    

    type(dyn_vect) :: w_f, w_a 
    type(sim_vect) :: w_s

    real, dimension(:,:,:), allocatable :: du, dv, dh, dw, work, temp, conf

    real, dimension(:),     allocatable :: lat, lon
    real, dimension(:,:,:), allocatable :: leve, levm, lat3, lon3

!   Analysis increment coarsening indices. Analysis increments are
!    produced with PSAS only on a subset of the gridpoints given by
!    (glat,glon,glvm/glve): glat' = glat(ilat), etc.
!   ---------------------------------------------------------------
    integer              :: imc, jmc, kmc
    integer, allocatable :: ilon(:), ilat(:), ilvm(:), ilve(:) 

    character(len=255) :: ifname1, ifname2, outfile, gfname, ofname

    integer i, j, k, ios, im, jm, km, lm, n, rc, iu, nymd, nhms, opt, nobs
    real ampl ! amplitude to scale incremements
      integer freq

!   ........................................................

    call init_ ( ifname1, ifname2, outfile, ampl ) ! parse cmd line
    gfname = trim(outfile) // '.grd'
    ofname = trim(outfile) // '.hdf'


!   process first file
!   ------------------
    call dyn_get ( ifname1, nymd, nhms, w_f, rc, freq=freq )
    if ( rc .ne. 0 ) call die ( myname, 'could not read first file' ) 
    print *, 'read ', trim(ifname1)

    call insitu_init ( w_f, w_s, rc )
    if ( rc .ne. 0 ) call die ( myname, 'could create sim vect (1)' )

    print *, 'initialized insitu (1)'

!   Allocate memory
!   ---------------
    call dyn_getdim ( w_f, im, jm, km, lm )
    print *, 'allocating mem with im, jm, km = ', im, jm, km
    allocate ( lon(im), lat(jm), levm(im,jm,km), leve(im,jm,km+1), &
               lon3(im,jm,km+1), lat3(im,jm,km+1), &
               work(im,jm,km+1), temp(im,jm,km+1), &
               du(im,jm,km), dv(im,jm,km), dw(im,jm,km), dh(im,jm,km+1), &
               conf(im,jm,km+1), stat=ios )
    if ( ios .ne. 0 ) call die ( myname, 'could not allocate mem' )
    print *, 'allocated mem '

!   generate coords
!   ---------------
    call Dyn_GetCoords ( w_f, im, jm, km, lon, lat, levm, leve, rc )
    if ( rc .ne. 0 ) call die ( myname, 'could not create coords' )
    do k = 1, km + 1
       do j = 1, jm
          do i = 1, im
             lon3(i,j,k) = lon(i)
             lat3(i,j,k) = lat(j)
          end do
       end do
    end do

!   To aleviate round-offs during interpolation
!   -------------------------------------------
    leve(1:im,1:jm,1)  = 1.0001  *  leve(1:im,1:jm,1) 
    levm(1:im,1:jm,1)  = 1.0001  *  levm(1:im,1:jm,1) 
    levm(1:im,1:jm,km) = 0.9999  *  levm(1:im,1:jm,km) 

    print *, 'levm = ', minval(levm), maxval(levm)
    print *, 'leve = ', minval(leve), maxval(leve)
    print *
    print *, 'lat  = ', minval(lat), maxval(lat)
    print *, 'lon  = ', minval(lon), maxval(lon)
    print *
    print *, 'lat3  = ', minval(lat3), maxval(lat3)
    print *, 'lon3  = ', minval(lon3), maxval(lon3)
    print *
   

!   Use insitu to sample w_f as analysis variables
!   ----------------------------------------------
    print *, 'Simulating fields 1...'
    opt = 0
    nobs = im*jm*km
    conf = 1.0
    print *, '   heights (1)'
    call Insitu_Mass ( w_s, lon3, lat3, leve, nobs+im*jm, 'hght', opt, &  
                       dh, conf, rc )
    n = count(conf.eq.0.0) 
    if ( rc.ne.0 .or. n.ne.0 ) then
         print *, 'n, rc = ', n, rc
         call die ( myname, 'could not simulate hght (1)' )
    end if

    print *, '   mixr (1)'
    call Insitu_Mass ( w_s, lon3(1:im,1:jm,1:km), lat3(1:im,1:jm,1:km), &
                       levm, nobs, 'mixr', opt, &  
                       dw, conf, rc )
    n = count(conf.eq.0.0) 
    if ( rc.ne.0 .or. n.ne.0 ) then
         print *, 'n, rc = ', n, rc
         call die ( myname, 'could not simulate mixr (1)' )
    end if


    print *, '   winds (1)'
    call Insitu_Wind ( w_s, lon3, lat3, levm, nobs, 'wind', opt, &  
                       du, dv, conf, rc )
    n = count(conf.eq.0.0) 
    if ( rc.ne.0 .or. n.ne.0 ) then
         print *, 'n, rc = ', n, rc
         call die ( myname, 'could not simulate mixr (1)' )
    end if

    call insitu_clean ( w_s )


!   Now process second file and compute analysis increments
!   -------------------------------------------------------
    print *, 'reading ' // trim(ifname2)
    call dyn_get ( ifname2, nymd, nhms, w_a, rc, freq=freq )
    if ( rc .ne. 0 ) call die ( myname, 'could not read second file' ) 


    call insitu_init ( w_a, w_s, rc )
    if ( rc .ne. 0 ) call die ( myname, 'could create sim vect (2)' )


!   Use insitu to sample w_f as analysis variables and comput ainc
!   This uses the vertical levels from file (1)
!   --------------------------------------------------------------
    print *, 'Simulating fields 2...'
    conf = 1.0
    print *, '   heights (2)'
    call Insitu_Mass ( w_s, lon3, lat3, leve, nobs+im*jm, 'hght', opt, &  
                       work, conf, rc )
    if ( rc.ne.0 .or. count(conf.eq.0.0) .ne. 0 ) &
         call die ( myname, 'could not simulate hght (2)' )
    dh = ampl * ( work - dh )

!   Generate coords for file (2). We do this because the insitu_*()
!    package cannot extrapolate winds and mixing ratio
!   ---------------------------------------------------------------
    call Dyn_GetCoords ( w_a, im, jm, km, lon, lat, levm, leve, rc )
    if ( rc .ne. 0 ) call die ( myname, 'could not create coords' )

!   To aleviate round-offs during interpolation
!   -------------------------------------------
    leve(1:im,1:jm,1)  = 1.0001  *  leve(1:im,1:jm,1) 
    levm(1:im,1:jm,1)  = 1.0001  *  levm(1:im,1:jm,1) 
    levm(1:im,1:jm,km) = 0.9999  *  levm(1:im,1:jm,km) 

    conf = 1.0
    print *, '   mixr (2)'
    call Insitu_Mass ( w_s, lon3, lat3, levm, nobs,  'mixr', opt, &  
                       work, conf, rc )
    n = count(conf.eq.0.0) 
    if ( rc.ne.0 .or. n.ne.0 ) then
         print *, 'n, rc = ', n, rc
         call die ( myname, 'could not simulate mixr (2)' )
    end if
    dw = ampl * ( work(1:im,1:jm,1:km) - dw )

    print *, '   wind (2)'
    call Insitu_Wind ( w_s, lon3, lat3, levm, nobs, 'wind', opt, &  
                       work, temp, conf, rc )
    n = count(conf.eq.0.0) 
    if ( rc.ne.0 .or. n.ne.0 ) then
         print *, 'n, rc = ', n, rc
         call die ( myname, 'could not simulate winds (2)' )
    end if
    du = ampl * ( work(1:im,1:jm,1:km) - du )
    dv = ampl * ( temp(1:im,1:jm,1:km) - dv )

    call insitu_clean ( w_s )


!   Write out analysis increment
!   ----------------------------
    print *, 'writing grads file with anal increments...'
    iu = 10
    open(iu,file=gfname,form='unformatted' )
    call write_grads ( iu, du, im*jm, km )
    call write_grads ( iu, dv, im*jm, km )
    call write_grads ( iu, dw, im*jm, km )
    call write_grads ( iu, dh, im*jm, km+1 )
    close ( iu )


!   Coarsening indices
!   ------------------
    allocate ( ilon(im), ilat(jm), ilvm(km), ilve(km+1), stat=ios )
    if ( ios .ne. 0 ) &
         call die ( myname, 'cannot allocate mem for grid indices' )
    ilon = (/ (i, i=1,im)    /)
    ilat = (/ (i, i=1,jm)    /)
    ilvm = (/ (i, i=1,km)    /)
    ilve = (/ (i, i=1,km+1)  /)

!   Finally, call m_ana2dyn to update first guess
!   ---------------------------------------------
    print *, 'calling Ana2Dyn...'
    call Ana2Dyn ( w_f, im, jm, km, du, dv, dh, dw,         &
                   lat, lon, levm, leve,                    &
                   im, jm, km, ilat, ilon, ilvm, ilve,   &
                   w_a, rc )

    if ( rc .ne. 0) then
         print *, 'rc = ', rc
         call die ( myname, 'could not create analyzed dynamics vector' )
    end if

!   Write out w_a
!   -------------
    print *, 'writing out '//trim(ofname)
    call dyn_put ( ofname, nymd, nhms, 0, w_a, rc, freq=freq )
    if ( rc .ne. 0) &
         call die ( myname, 'could not write w_a' )


    print *
    print *, 'ps_f = ', minval(w_f%ps), maxval(w_f%ps)
    print *, 'ps_a = ', minval(w_a%ps), maxval(w_a%ps)

    print *
    print *, 'u_f = ', minval(w_f%u), maxval(w_f%u)
    print *, 'u_a = ', minval(w_a%u), maxval(w_a%u)

    print *
    print *, 'v_f = ', minval(w_f%v), maxval(w_f%v)
    print *, 'v_a = ', minval(w_a%v), maxval(w_a%v)

    print *
    print *, 'pt_f = ', minval(w_f%pt), maxval(w_f%pt)
    print *, 'pt_a = ', minval(w_a%pt), maxval(w_a%pt)

    print *
    print *, 'q_f = ', minval(w_f%q), maxval(w_f%q)
    print *, 'q_a = ', minval(w_a%q), maxval(w_a%q)



!   clean up mess
!   -------------
    call dyn_clean ( w_a )
    deallocate ( lon, lat, levm, leve, &
               lon3, lat3, &
               work, temp, &
               du, dv, dw, dh, &
               conf )
    deallocate ( ilon, ilat, ilvm, ilve )
    

    stop

CONTAINS

     subroutine die ( myname, msg )
     character(len=*) :: myname, msg
     write(*,'(a)') trim(myname) // ': ' // trim(msg)
     stop
     end subroutine die
  
      end program ut_ana2dyn

!................................................................

      subroutine write_grads ( iu, x, m, n )

      real   x(m,n)
      real*4 x4(m)

      do j = 1, n
         x4 = x(1:m,j)
         write(iu) ( x4(i), i = 1, m )
      end do

      end


!................................................................

      subroutine Init_ ( dynfile1, dynfile2, outfile, ampl )

      implicit NONE

      character*255, intent(out) :: dynfile1 ! dynamics state file name
      character*255, intent(out) :: dynfile2 !  input ODS file name
      character*255, intent(out) :: outfile  ! output ODS file name
      real,          intent(out) :: ampl     ! scale factor for ainc

      character*4, parameter :: myname = 'init'

      integer iret, i, iarg, argc, iargc
      character*255 argv

!     Parse command line
!     ------------------
      ampl = 0.1
      outfile = 'ut_ana2dyn'

      argc =  iargc()
      if ( .not. (argc.eq.2 .or. argc.eq.4)  ) call usage()

      iarg = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iArg, argv )
         if (index(argv,'-o' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iArg, outfile )
         else if (index(argv,'-s' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iArg, argv )
            read(argv,*) ampl
         else
            dynfile1 = trim(argv)
            iarg = iarg + 1
            call GetArg ( iArg, dynfile2 )
         end if
      end do

!     Echo the parameters
!     -------------------
      print *
      print *, 'Dynamics state file 1: ', trim(dynfile1)
      print *, 'Dynamics state file 2: ', trim(dynfile2)
      print *, '         Output  file: ', trim(outfile)
      print *, '  Ainc scaling factor: ', ampl
      print *

      end subroutine Init_

!.................................................................

      subroutine usage()
      print *
      print *, 'Usage:  ut_ana2dyn.x [-o fname]  dynfile1  dynfile2'
      print *
      print *, 'where'
      print *
      print *,   '-o fname    basename for output file name'
      print *,   '           (default: ut_ana2dyn)'
      print *,   '-s factor  analysis increment scaling factor'
      print *,   '           (default = 0.1)'
      print *,   ' dynfile1   dynamics state file name 1'
      print *,   ' dynfile2   dynamics state file name 2'
      print *
      stop
      end

