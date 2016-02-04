!
! Simple command line application for resolving file names.
!

    use m_StrTemplate
    use m_FileResolv

    implicit NONE


    character(len=*), parameter :: myname = 'fresolv'


    integer nymd, nhms
    logical caching, dryrun
    integer i, iarg, argc, iargc
    character(len=255) expid, argv, template, fname

    caching = .true.
    dryrun = .false.
    nymd = 0000
    nhms = 0000
    expid = 'unknown'


!   Parse command line
!   ------------------
    argc =  iargc()
    if ( argc .lt. 1 ) call usage()
    
    iarg = 0
    do i = 1, 32767
       iarg = iarg + 1
       if ( iarg .gt. argc ) exit
       call GetArg ( iArg, argv )
       if (index(argv,'-nocache' ) .gt. 0 ) then
          caching = .false.
       else if (index(argv,'-d' ) .gt. 0 ) then
          dryrun = .true.
       else if (index(argv,'-t' ) .gt. 0 ) then
          if ( iarg+3 .gt. argc ) call usage()
          call GetArg ( iarg+1, expid )
          call GetArg ( iarg+2, argv  ); read(argv,*) nymd
          call GetArg ( iarg+3, argv  ); read(argv,*) nhms
          iarg = iarg + 3
       else
          if ( dryrun ) then
             call StrTemplate ( fname, argv, &
                                xid=trim(expid), nymd=nymd, nhms=nhms )
             write(*,'(a)') trim(fname)
          else
             call FileResolv ( expid, nymd, nhms, argv, fname, cache=caching )
             write(*,'(a)') myname//': resolved ' // trim(fname)
          endif
       end if
    end do

    end

    subroutine usage()

    print *
    print *, 'NAME'
    print *
    print *, '      fresolv - Resolve local or remote file names'
    print *
    print *, 'SYNOPSIS'
    print *
    print *, '      fresolv  [-nocache] [-t expid nymd  nhms]  template(s)'
    print *
    print *, 'DESCRIPTION'
    print *
    print *, '      Given one or more templates of the form:'
    print *
    print *, '              [host:][dirname]fname[.gz]'
    print *, '      e.g.,'
    print *
    print *, '      dasilva@dixon0:/data/ods/%s.ods.t%y4%m2%d2.gz'
    print *
    print *, '      this utility uses nymd/nhms to expand the GrADS-like'
    print *, '      templates %y4, %m2, %d2, replacing %s with "expid".'
    print *, '      In case the file is compressed with gzip(1) (name ending'
    print *, '      with ".gz") an uncompressed copy of the file is created'
    print *, '      in the local directory. In case the file resides on a '
    print *, '      remote machine a local copy is made using "rcp".'
    print *, '      Local files are not overwritten unless the option'
    print *, '      "-nocache" is specified.'
    print *
    print *, 'OPTIONS'
    print *, '-t expid nymd  nhms'
    print *, '      Tokens for file name expansion:'
    print *, '      expid   experiment id '
    print *, '      nymd    year-month-day, e.g., 19990921'
    print *, '      nhms    hour-min-sec, e.g., 120000'
    print *
    print *, ' -d'
    print *, '      Dry run; just prints out the file name it would try'
    print *, '      to resolve'
    print *
    print *, ' -nocache  '
    print *, '      Turns cache OFF. When the template specifies a file'
    print *, '      residing on a remote machine, or the file is compressed'
    print *, '      with gzip(1), a local copy is created only if the file'
    print *, '      does not exist in the current directory. However, '
    print *, '      if "-nocache" is specified, any existing local copy '
    print *, '      of the file will be overwritten.'
    print *
    print *, 'AUTHOR'
    print *, '      Arlindo da Silva (dasilva@gsfc.nasa.gov)'
    print *

    call exit(1)
 
    stop
    end


