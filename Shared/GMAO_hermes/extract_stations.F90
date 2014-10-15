!
! extract_stations.F90
! Peter Colarco, Dec. 2010
!
! Procedure looks for a resource file (fixed format for now) that
! specifies station locations.  Then reads from model horizontal
! tau fields and interpolates to station points, extracting
! one gfio file per station.  Very specific to my needs for AERONET
! comparisons.
!
! Requires my special hacked in version of gfio_putvarT
! For brevity, no error checking
!
! Add optional switch to stop running after some NYMD
! Add optional switch to output files one day at time (-daily)
!

   Program extract_stations

   use m_interp 
   use m_StrTemplate

   implicit NONE

!  Command line inputs
   integer iarg, iargc, argc
   character(len=256) :: argv
   character(len=256) :: rcfile, inpTemplate, fname, outTemplate, &
                         expid, forcefirstname
   logical :: inpexist

!  Station location information
   integer, parameter :: mvar = 1000   ! max aeronet sites
   character(len=256) :: site(mvar)
   integer :: londeg(mvar), lonmin(mvar), lonsec(mvar), &
              latdeg(mvar), latmin(mvar), latsec(mvar), elevation(mvar)
   integer :: cdfid
   real, allocatable :: lonsite(:), latsite(:), elvsite(:)
   integer :: iflip, ilev  ! iflip is correctly orient lon/lat minutes relative to degrees


!  Variables dimension on input (read once per time)
   integer :: im, jm, km
   real, allocatable :: var(:,:,:)


!  interpolation points (reduced from station location information)
!  -------------
   integer :: n, icnt
   real, allocatable :: lat(:), lon(:), lev(:), conf(:), val(:,:,:,:) 
   type(int_grid) :: grid



!  Local information
   integer, parameter :: READ_ONLY = 1
   integer i, rc, fid, lm, nvars, nvars_e, ngatts, nymd, nhms, ivar, ivar_e, it, itime, inc, nt
   character(len=256) :: str
   character(len=256) :: title            ! meta data title
   character(len=256) :: source           ! data source
   character(len=256) :: contact          ! contact org.   
   character(len=256) :: levunits         ! Vertical levels
   real :: amiss
   character(len=256), allocatable :: vName(:), vName_e(:)       ! output variable names (nVars)
   character(len=256), allocatable :: vtitle(:), vtitle_e(:)     ! output title
   character(len=256), allocatable :: vunits(:), vunits_e(:)     ! output vertical level unit
   real, allocatable :: valid_range(:,:), valid_range_e(:,:)
   real, allocatable :: packing_range(:,:), packing_range_e(:,:)
   integer, allocatable  :: kmVar(:), kmVar_e(:)          ! Number of vertical levels for variables
   integer, allocatable  :: yyyymmdd(:), hhmmss(:)
   integer :: timinc, nsecs, timestep, hour_step, minute_step, second_step, ntimesteps
   integer :: ndays, yyyy
   character(len=256) :: vars(mvar)
   integer :: iday, it0, nt0, lastnymd
   logical :: do_daily, create_files


! Parse the command line (see usage() below)
  argc = iargc()
  if(argc .lt. 1) call usage()
  iarg = 0
  nymd = 20000101
  nhms = 0
  expid = 'u000_b32_b'
  inpTemplate = '/output7/colarco/' // trim(expid) // &
                '/tau/Y%y4/M%m2/' // trim(expid) // '.tau2d.%y4%m2%d2.hdf'
  outTemplate = '/output7/colarco/' // trim(expid) // &
                '/tau/Y%y4/' // trim(expid) // '.tau2d.%s.%y4.nc'
  rcfile  = 'aeronet_locs.dat'
  forcefirstname = ''
  lastnymd = 99999999
  nvars = 0
  do_daily = .false.
  do i = 0, 32767
   iarg = iarg+1
   if(iarg .gt. argc) exit
   call GetArg(iarg, argv)
   select case(argv)
    case ("-i")
     if(iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg, inptemplate)
    case ("-o")
     if(iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg, outTemplate)
    case ("-expid")
     if(iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg, expid)
    case ("-nymd")
     if(iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg, argv)
     read(argv,*) nymd
    case ("-lastnymd")
     if(iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg, argv)
     read(argv,*) lastnymd
    case ("-nhms")
     if(iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg, argv)
     read(argv,*) nhms
    case ("-rc")
     if(iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg, rcfile)
    case ("-daily")
     do_daily = .true.
    case ("-vars")
     if ( iarg+1 .gt. argc ) call usage()
     iarg = iarg + 1
     call GetArg ( iArg, argv )
     call split_ ( ',', argv, mVar, Vars, nVars )
     !CAR
     !print *, 'Vars in: ', Vars(1:15)
     !CAR
    case ("-force")
     if(iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg, forcefirstname)
    case ("-timestep")
     if (iarg+1 .gt. argc) call usage()
     iarg = iarg+1
     call GetArg(iarg,argv)
     read(argv,*) timestep
     hour_step   = timestep/10000
     minute_step = (timestep/100)-hour_step*100
     second_step = timestep-hour_step*10000-minute_step*100
     !print *, hour_step, minute_step, second_step
   end select
  end do
  rcfile = trim(rcfile)
  inpTemplate = trim(inpTemplate)
  outTemplate = trim(outTemplate)

! Get the AERONET sites
  open(unit=10, file=rcfile)
  read(10,'(a30)') str
  read(10,'(a30)') str
  n = 1
  do
   read(10,'(a30,6(1x,i4),1x,i5)', end=100) &
            site(n), latdeg(n), latmin(n), latsec(n), &
            londeg(n), lonmin(n), lonsec(n), elevation(n)
   n = n+1
  enddo
100 continue
  n = n-1
  allocate(lonsite(n), latsite(n), elvsite(n))
  do icnt = 1, n
     iflip = 1
     if(londeg(icnt) .lt. 0) iflip = -1
     lonsite(icnt) = londeg(icnt) + iflip*lonmin(icnt)/60. + iflip*lonsec(icnt)/3600.
     iflip = 1
     if(latdeg(icnt) .lt. 0) iflip = -1
     latsite(icnt) = latdeg(icnt) + iflip*latmin(icnt)/60. + iflip*latsec(icnt)/3600.
     elvsite(icnt) = elevation(icnt)
  enddo

!   For now we assume you want to do a whole year at a time
    yyyy = nymd/10000
    ndays = 365
    if(yyyy/4 .eq. yyyy/4.) ndays = 366

!   Check the first file to structure the reads
    if(forcefirstname .eq. '') then
     call strTemplate(fname,inpTemplate,xid=expid,nymd=nymd,nhms=nhms)
     call GFIO_Open ( fname, READ_ONLY, fid, rc )
    else
     call GFIO_Open ( trim(forcefirstname), READ_ONLY, fid, rc)
    endif
    call GFIO_DimInquire ( fid, im, jm, km, lm, nvars_e, ngatts, rc)

!CAR
print *, 'Value im: ', im
print *, 'Value jm: ', jm
print *, 'Value km: ', km
print *, 'Value lm: ', lm
print *, 'Nvars_e: ', nvars_e
print *, 'Ngatts: ', ngatts
print *, 'Number of vars: ', nvars
!CAR

    ! ntimesteps is either (a) based on hour_step if you have one file for each time step of (b) based on the
    ! number of timesteps (lm) from the file if you have one file per day with maybe more than one time step in
    ! it; if you don't tell it your timestep between files, it assumes 1 file per day or based on lm from file
    if (lm .eq. 1) then
       if (hour_step .gt. 0) then
       	  ntimesteps = 24./hour_step
       else
          ! basically assumes 1 time per day
	  ntimesteps = lm
       endif
    else
       ! assumes lm times per day
       ntimesteps = lm
    endif

    ! nt is the total number of timesteps in the year   
    nt = ndays*ntimesteps


    allocate (lon(im), lat(jm), lev(km))

    allocate (yyyymmdd(nt), hhmmss(nt), kmvar_e(nvars_e), vname_e(nvars_e), &
              vtitle_e(nvars_e), vunits_e(nvars_e), valid_range_e(2,nvars_e), packing_range_e(2,nvars_e))
    call GFIO_Inquire ( fid, im, jm, km, lm, nvars_e, &
                        title, source, contact, amiss, &
                        lon, lat, lev, levunits, &
                        yyyymmdd(1:lm), hhmmss(1:lm), timinc, &
                        vname_e, vtitle_e, vunits_e, kmvar_e, &
                        valid_range_e, packing_range_e, rc)
    call GFIO_Close ( fid, rc )


    ! nsecs is what we use to update counter; equal to number of seconds in the hour_step
    ! if no timestep is given, assume timinc from the file is HHMMSS and turn into seconds
    if (hour_step .gt. 0) then 
       nsecs = hour_step*60.*60.
    else
       nsecs = 60*60*(timinc/10000)+60*((timinc/100)-(timinc/10000)*100)+timinc-(timinc/10000)*10000-((timinc/100)-100*(timinc/10000))*100
    endif

!   PRC -- here is where we associate the requested variables with the actuals
    if(nvars .le. 0) nvars=nvars_e
    allocate (kmvar(nvars), vname(nvars), &
              vtitle(nvars), vunits(nvars), valid_range(2,nvars), packing_range(2,nvars))


    if(nvars .ne. nvars_e) then
     do ivar = 1, nvars
      do ivar_e = 1, nvars_e
       if(vars(ivar) .eq. vname_e(ivar_e)) then
        kmvar(ivar) = kmvar_e(ivar_e)
	!print *, 'The value of vname_e is: ', vname_e(ivar_e), ivar_e
        vname(ivar) = vname_e(ivar_e)
        vtitle(ivar) = vtitle_e(ivar_e)
        vunits(ivar) = vunits_e(ivar_e)
        valid_range(:,ivar) = valid_range_e(:,ivar_e)
        packing_range(:,ivar) = packing_range_e(:,ivar_e)
       endif
      enddo
     enddo
    else
     do ivar = 1, nvars
        kmvar(ivar) = kmvar_e(ivar)
        vname(ivar) = vname_e(ivar)
        vtitle(ivar) = vtitle_e(ivar)
        vunits(ivar) = vunits_e(ivar)
        valid_range(:,ivar) = valid_range_e(:,ivar)
        packing_range(:,ivar) = packing_range_e(:,ivar)
     enddo
    endif

    if(km > 0) then
     allocate (var(im,jm,km))
     allocate(val(km,nt,nvars,n),conf(n))
    else
     allocate (var(im,jm,1))
     allocate(val(1,nt,nvars,n),conf(n))
    endif



!   Initialize interpolation package
!   --------------------------------
    call Interp_Init ( im, jm, 1, 1., grid, rc )

!   Loop over time, opening the input tau file once per day, extracting
!   variables once per time on file, interpolating to site locations,
!   and once per day writing to output files.



    !CAR: Loop over total number of times in the year 
    create_files = .true.
    do iday = 1, ndays

!    Create the station files if needed
!    Either at the beginning for a year long extract, or every day for daily
     if(create_files) then
      do icnt = 1, n
       call strTemplate(fname,outTemplate,xid=trim(site(icnt)),nymd=nymd,nhms=nhms)
       call GFIO_Create ( fname, title, source, contact, amiss, &
                                 1, 1, km, lonsite(icnt), latsite(icnt), lev, levunits, &
                                 nymd, nhms, timestep, &
                                 nvars, vname, vtitle, vunits, kmvar, &
                                 valid_range, packing_range, 0, &
                                 cdfid, rc )
       call GFIO_close(cdfid,rc)
       create_files = .false.
      enddo
     endif
     if(do_daily) create_files = .true.

!    Loop over timesteps per day
     do inc = 1, ntimesteps

      it = (iday-1)*ntimesteps+(inc-1)*lm

!     Open the input
      call strTemplate(fname,inpTemplate,nymd=nymd,nhms=nhms)
      call GFIO_Open ( fname, READ_ONLY, fid, rc )


!     If file not opened, we will just fill in the array with 
!     the missing values.
      inpexist = .true.
      if(rc .ne. 0) inpexist = .false.

      if(inpexist) then
       call GFIO_Inquire ( fid, im, jm, km, lm, nvars_e, &
                           title, source, contact, amiss, &
                           lon, lat, lev, levunits, &
                           yyyymmdd(it+1:it+lm), hhmmss(it+1:it+lm), timinc, &
                           vname_e, vtitle_e, vunits_e, kmvar_e, &
                           valid_range_e, packing_range_e, rc)
      else
       yyyymmdd(it+1) = nymd
       hhmmss(it+1)   = nhms
       if(lm .gt. 1) then
        do itime = 2,lm
         call getdate(yyyymmdd(it+itime-1), hhmmss(it+itime-1), 86400, &
                      yyyymmdd(it+itime), hhmmss(it+itime), rc)
        enddo
       endif
      endif
      if(inpexist) then
      do itime = 1, lm
       do ivar = 1, nvars
        if(kmvar(ivar) > 0) then
         call GFIO_GetVar ( fid, trim(vname(ivar)), yyyymmdd(it+itime), &
                            hhmmss(it+itime), im, jm, 1, kmvar(ivar), var, rc ) 

        else
         call GFIO_GetVar ( fid, trim(vname(ivar)), yyyymmdd(it+itime), &
                            hhmmss(it+itime), im, jm, 0, 1, var, rc ) 
        endif
! PRC -- we need to shift if the min value of longitude is lt 0
        if(minval(lon) .lt. 0) var = cshift(var,im/2)
        if(kmvar(ivar) > 0) then
         do ilev = 1, kmvar(ivar)
          call Interp_Field ( grid, lonsite, latsite, lev(ilev), n,  &
                              im, jm, 1, var(:,:,ilev), 0,       &
                              val(ilev,it+itime,ivar,:), conf(:), rc )
         enddo
        else
         call Interp_Field ( grid, lonsite, latsite, lev(1), n,  &
                             im, jm, 1, var(:,:,1), 0,       &
                             val(1,it+itime,ivar,:), conf(:), rc )
        endif
       enddo
      enddo 
      else
       val(:,it+1:it+lm,:,:) = amiss
      endif

!     Increment the date
      if(inpexist) call GFIO_Close ( fid, rc )
      print *, 'Finished reading: ', nymd, nhms
      call getdate(yyyymmdd(it+1), hhmmss(it+1), nsecs, nymd, nhms, rc)
      print *, 'Incrementing Date: ', nymd, nhms

     enddo ! ntimesteps

!    Write if needed
!    If doing daily, then write every day
!    If you've run out of days (iday = ndays) then write
!    If next date nymd > lastnymd then write
     if(do_daily .or. iday .eq. ndays .or. nymd .gt. lastnymd) then
      it0 = 1
      nt0 = it+1
      if(do_daily) it0 = (iday-1)*ntimesteps + 1
      if(do_daily) nt0 = ntimesteps
      do icnt = 1, n
       call strTemplate(fname,outTemplate,xid=trim(site(icnt)),nymd=yyyymmdd(it0),nhms=hhmmss(it0))
       call gfio_open(trim(fname), 0, cdfid, rc)
       print *, 'writing: ', trim(fname)
       do ivar = 1, nvars
          if(kmvar(ivar) .gt. 0) then 

            call gfio_putvarT(cdfid,trim(vname(ivar)),yyyymmdd(it0), hhmmss(it0), &
                              1, 1, 1, kmvar(ivar), 1, nt0, val(1:kmvar(ivar),it0:it0+nt0-1,ivar,icnt),rc)
          else
            call gfio_putvarT(cdfid,trim(vname(ivar)),yyyymmdd(it0), hhmmss(it0), &
                              1,1,0,1,1,nt0,val(1,it0:it0+nt0-1,ivar,icnt),rc)
          endif
       enddo
       call gfio_close(cdfid,rc)
      enddo
     endif

     if(nymd > lastnymd) goto 300

    enddo  ! ndays

300 continue

! Done with interpolation
! -----------------------
  call Interp_Clean ( grid )

  deallocate(var,lon,lat,lev,yyyymmdd,hhmmss,kmvar,vname,vtitle,vunits, &
             valid_range,packing_range,lonsite,latsite,elvsite,val,conf, &
             kmvar_e, vname_e, vtitle_e, vunits_e, valid_range_e, packing_range_e )


  contains

  subroutine usage()
  print *
  print *,'Usage: '
  print *,'  extract_stations.x [-rc rcfile -force forcefirstname]'
  print *,'                                   -i inpTemplate'
  print *,'                                   -o outTemplate'
  print *,'                                   -e expid'
  print *,'                                   -nymd nymd'
  print *,'                                   -nhms nhms'
  print *,'                                   -vars var1,var2'
  print *,'                                   -timestep timestep'
  print *,'                                   -lastnymd nymd'
  print *,'                                   -daily'
  print *
  print *, 'where'
  print *
  print *, 'rcfile         location database'
  print *, 'forcefirstname optional file that definitely exists'
  print *, 'inpTemplate    Grads like template specifying input path structure'
  print *, 'outTemplate    Grads like template specifying output path structure'
  print *, 'expid          string experiment id (needed for inpTemplate/outTemplate'
  print *, 'nymd           YYYYMMDD of starting time'
  print *, 'nhms           HHMMSS of starting time'
  print *, 'timestep       HHMMSS of timestep between each file'
  print *
  print *, 'lastnymd       option to close files and exit for nymd > lastnymd'
  print *, '-daily         if specified, only loop over single day'
  print *
  print *, 'Note: Assumes you have a full year of data with either one file per' 
  print *, '      timestep if specified OR'
  print *, '      one time per day if timestep is not specified'
  print *, '      Right now only deals with time steps in increments of hours'
  print *
  call exit(1)
  end subroutine usage



!............................................................................

    subroutine split_ ( tok, str, mList, List, nList )
    implicit NONE
    character(len=1), intent(in)  :: tok  ! delimitter
    character(len=*), intent(in)  :: str  ! string
    integer,          intent(in)  :: mList
    character(len=*), intent(out) :: List(mList)
    integer,          intent(out) :: nList

    integer i, l, n, j

    i = 1
    l = len_trim(str)
    nList = 0
    do n = 1, mList
       j = index(trim(str(i:)),tok) - 1
       if ( j < 1 ) then
            nList = nList + 1
            List(nList) = str(i:)
            exit
       end if
       nList = nList + 1
       List(nList) = str(i:i+j-1)
       i = i + j + 1
       if ( i > l ) exit
    end do

    end subroutine split_

end Program extract_stations
  
