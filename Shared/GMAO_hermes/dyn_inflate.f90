program dyn_inflate
use  m_dyn
use m_inpak90
use m_stdio, only : stdout,stderr
use m_ioutil, only : luavail
implicit none

integer, parameter :: MFILES=3
integer dyntype, freq
integer k,nv,iv,im,jm,km
integer nymd, nhms
integer rc,lu
type(dyn_vect) xm_b ! bkg fields
type(dyn_vect) xm_a ! ana fields
type(dyn_vect) xs_b ! bkg spreads
real, allocatable :: rmse(:,:) 

character(len=50),allocatable :: vars(:)
character(len=255) :: ofile
real, allocatable :: infl(:,:)
real, allocatable :: ainf(:,:)
real, allocatable :: coslat(:)
real :: ainf_ps,ainf_ts
real :: infl_ps,infl_ts
real :: sbye,rr
real :: pkthresh
logical :: ocean_only
integer :: nparam
integer :: m_star

character(len=255) files(MFILES) ! 1=mean background
                                 ! 2=mean verifying analysis
                                 ! 3=background spread

call init_(dyntype,files)

! Read in background spread
  call dyn_get ( files(1), nymd, nhms, xm_b, rc, timidx=1, freq=freq, vectype=dyntype )
  call dyn_get ( files(2), nymd, nhms, xm_a, rc, timidx=1, freq=freq, vectype=dyntype )
  call dyn_get ( files(3), nymd, nhms, xs_b, rc, timidx=1, freq=freq, vectype=dyntype )

  im=xm_b%grid%im; jm=xm_b%grid%jm; km=xm_b%grid%km
  allocate(rmse(im,jm))
  allocate(infl(km,size(vars)))

! estimate inflation parameters
  rmse = xm_b%ps - xm_a%ps
  rr   = gave_(rmse*rmse,mask=xm_b%frocean)
  sbye  =gave_(xs_b%ps*xs_b%ps,mask=xm_b%frocean)/rr
  infl_ps = inflation_(ainf_ps,sbye)

  rmse = xm_b%ts - xm_a%ts
  rr = gave_(rmse*rmse,mask=xm_b%frocean)
  sbye  =gave_(xs_b%ts*xs_b%ts,mask=xm_b%frocean)/rr
  infl_ts = inflation_(ainf_ts,sbye)

  if(.not.allocated(ainf)) then
    print *, 'ainf not defined, aborting ...'
    call exit(99)
  endif
  do iv=1,size(vars)
     if(trim(vars(iv))=='u')then
       do k=1,km
          rmse = abs(xm_b%u(:,:,k) - xm_a%u(:,:,k));
          sbye=gave_(xs_b%u(:,:,k)*xs_b%u(:,:,k))/gave_(rmse*rmse)
          infl(k,iv) = inflation_(ainf(k,iv),sbye)
       enddo
     endif
     if(trim(vars(iv))=='v')then
       do k=1,km
          rmse = abs(xm_b%v(:,:,k) - xm_a%v(:,:,k));
          sbye=gave_(xs_b%v(:,:,k)*xs_b%v(:,:,k))/gave_(rmse*rmse)
          infl(k,iv) = inflation_(ainf(k,iv),sbye)
       enddo
     endif
     if(trim(vars(iv))=='tv')then
       do k=1,km
          rmse = abs(xm_b%pt(:,:,k) - xm_a%pt(:,:,k));
          sbye=gave_(xs_b%pt(:,:,k)*xs_b%pt(:,:,k))/gave_(rmse*rmse)
          infl(k,iv) = inflation_(ainf(k,iv),sbye)
       enddo
     endif
     if(trim(vars(iv))=='sphu')then
       do k=1,km
          rmse = abs(xm_b%q(:,:,k,1) - xm_a%q(:,:,k,1));
          rr=gave_(rmse*rmse)
          if(rr>1.e-10) then
             sbye=gave_(xs_b%q(:,:,k,1)*xs_b%q(:,:,k,1))/rr
             infl(k,iv) = inflation_(ainf(k,iv),sbye)
          else
             infl(k,iv)=0.0
          endif
       enddo
     endif
     if(trim(vars(iv))=='ozone')then
       do k=1,km
          rmse = abs(xm_b%q(:,:,k,2) - xm_a%q(:,:,k,2));
          rr=gave_(rmse*rmse)
          if(rr>1.e-10) then
             sbye=gave_(xs_b%q(:,:,k,2)*xs_b%q(:,:,k,2))/rr
             infl(k,iv) = inflation_(ainf(k,iv),sbye)
          else
             infl(k,iv)=0.0
          endif
       enddo
     endif
     if(trim(vars(iv))=='qitot')then
       do k=1,km
          rmse = abs(xm_b%q(:,:,k,3) - xm_a%q(:,:,k,3));
          rr=gave_(rmse*rmse)
          if(rr>1.e-10) then
             sbye=gave_(xs_b%q(:,:,k,3)*xs_b%q(:,:,k,3))/rr
             infl(k,iv) = inflation_(ainf(k,iv),sbye)
          else
             infl(k,iv)=0.0
          endif
       enddo
     endif
     if(trim(vars(iv))=='qltot')then
       do k=1,km
          rmse = abs(xm_b%q(:,:,k,4) - xm_a%q(:,:,k,4));
          rr=gave_(rmse*rmse)
          if(rr>1.e-10) then
             sbye=gave_(xs_b%q(:,:,k,4)*xs_b%q(:,:,k,4))/rr
             infl(k,iv) = inflation_(ainf(k,iv),sbye)
          else
             infl(k,iv)=0.0
          endif
       enddo
     endif
     if(trim(vars(iv))=='qrtot')then
       do k=1,km
          rmse = abs(xm_b%q(:,:,k,5) - xm_a%q(:,:,k,5));
          rr=gave_(rmse*rmse)
          if(rr>1.e-10) then
             sbye=gave_(xs_b%q(:,:,k,5)*xs_b%q(:,:,k,5))/rr
             infl(k,iv) = inflation_(ainf(k,iv),sbye)
          else
             infl(k,iv)=0.0
          endif
       enddo
     endif
     if(trim(vars(iv))=='qstot')then
       do k=1,km
          rmse = abs(xm_b%q(:,:,k,6) - xm_a%q(:,:,k,6));
          rr=gave_(rmse*rmse)
          if(rr>1.e-10) then
             sbye=gave_(xs_b%q(:,:,k,6)*xs_b%q(:,:,k,6))/rr
             infl(k,iv) = inflation_(ainf(k,iv),sbye)
          else
             infl(k,iv)=0.0
          endif
       enddo
     endif
  enddo

! Output inflation factor to file
  if(trim(ofile)/='NONE') then
    lu=luavail()
    open(lu,file=trim(ofile),form='formatted')
    write(lu,'(a,2x,i5)')   'dyn_recenter_adaptive_smooth_factor: ', m_star
    if(ocean_only) then
      write(lu,'(a,2x,a)')  'dyn_recenter_2d_ocean_only: ', 'yes'
    else
      write(lu,'(a,2x,a)')  'dyn_recenter_2d_ocean_only: ', 'no'
    endif
    write(lu,'(a,2x,f7.3)') 'dyn_addinf_pthreshold_pa:', pkthresh
    write(lu,'(a,2x,i5)')   'dyn_recenter_nlevs: ', km
    write(lu,'(a,2x,f7.3)') 'dyn_recenter_add_inf_ps: ', infl_ps
    write(lu,'(a,2x,f7.3)') 'dyn_recenter_add_inf_ts: ', infl_ts
    write(lu,'(a,2x,12(a,1x))') 'dyn_recenter_add_inf_vars: ', (trim(vars(iv)),iv=1,size(vars))
    do iv=1,size(vars)
       write(lu,'(3a,i3.3,a)') 'dyn_recenter_add_inf_', trim(vars(iv)), '_', km, '::'
       do k=1,km
          write(lu,'(2x,i4,2x,f7.3)')  k, infl(k,iv)
       enddo
       write(lu,'(a)') '::'
    enddo
    close(lu)
  endif
! clean up
  deallocate(vars)
  deallocate(ainf)
  deallocate(rmse)
contains

  subroutine init_ (dyntype,files)
      integer, intent(out) :: dyntype
      character(len=*),intent(out) :: files(:)

      character(len=*), parameter :: myname='init_'
      integer iret, i, iarg, argc, iargc
      integer irow,nlevs, ier, nfiles, iv, nivars
      character(len=255) :: argv
      character(len=255) :: token
      character(len=255) :: RCfile
      character(len=255) :: tablename

      m_star = 0
      nparam = 1 ! only a single parameter estimated at a time
      dyntype = 5
      ofile = 'NONE'
      RCfile = 'dyn_recenter.rc'

!     Parse command line
!     ------------------
      argc =  iargc()
      if ( argc .lt. 1 ) call usage()
      nfiles=0
      iarg=0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iarg, argv )

         select case (argv)
           case ("-g4")
             dyntype = 4
           case ("-o")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, ofile )
           case ("-rc")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iArg, RCfile )
           case default
             nfiles = nfiles + 1
             if ( nfiles .gt. mfiles ) call usage()
             files(nfiles) = argv
         end select
      enddo

!     Load resources: some of these will overwrite command line settings
!     --------------
      if ( trim(RCfile) .ne. 'dyn_recenter.rc' ) then
        call i90_loadf (trim(RCfile), iret)
        if( iret .ne. 0) then
           write(stdout,'(3a)') trim(myname),': Warning, I90_loadf cannot find file', trim(RCfile)
           rc = 1
           return
        end if
        write(stdout,'( a  )') '---------------------------------------------------------'
        write(stdout,'(3a  )') trim(myname), ': Reading resource file: ', trim(RCfile)
        write(stdout,'( a,/)') '---------------------------------------------------------'


!       Read in number of levels in background field
!       --------------------------------------------
        nlevs = -1
        call I90_label('dyn_recenter_nlevs:', iret)
        if (iret==0) then
           nlevs = I90_GInt(iret)
        else
           write(stderr,'(2a,i5)') trim(myname),': cannot determine no. of levels, aborting ... '
           call exit(1)
        end if
        write(stdout,'(2a,i5)') trim(myname),': number of inflation vertical levels = ', nlevs 

!       Read in number of levels in background field
!       --------------------------------------------
        nivars=0
        call I90_label('dyn_recenter_add_inf_vars:', iret)
        do while (iret==0)  ! figure out how many variables ...
           call I90_GToken(token, iret )
           if (iret==0) then
               nivars = nivars + 1
           endif
        end do
        allocate(vars(nivars))
        nivars= 0 
        call I90_label('dyn_recenter_add_inf_vars:', iret)
        do while (iret==0)  ! read again to get variables ...
           call I90_GToken(token, iret )
           if (iret==0) then
               nivars = nivars + 1
               vars(nivars) = trim(token)
           endif
        end do

!       Read table with variable types
!       ------------------------------
        allocate(ainf(nlevs,nivars))
        do iv=1,nivars ! for each variable ...
           write(tablename,'(3a,i3.3,a)') 'dyn_recenter_add_inf_', trim(vars(iv)), '_', nlevs, '::'
           call I90_label(trim(tablename), iret)
           if (iret/=0) then
              write(stderr,'(2a,i5,2a)') myname, ': I90_label error, iret=', iret, &
                                                 ': trying to read ', trim(tablename)
              call exit(2)
           end if
           irow = 0
           write(stdout,'(3a)') ' Reading vertically varying inflation ', trim(vars(iv)), '...'
           do while (iret==0)                   ! read table entries
              call I90_GLine ( iret )           ! iret=-1: end of file; +1: end of table
              if (iret==0.and.irow<=nlevs) then ! OK, we have next row of table
                  irow = irow + 1
    
                  call I90_GToken(token, ier )
                  if(ier/=0) then
                    write(stderr,'(2a,i5)') trim(myname),': cannot read 1st entry in table, aborting ...'
                    call exit(3)
                  endif
                  call I90_GToken(token, ier )
                  if(ier/=0) then
                    write(stderr,'(2a,i5)') trim(myname),': cannot read 2nd entry in table, aborting ...'
                    call exit(4)
                  endif
                  read(token,*) ainf(irow,iv) 
              end if
           end do
           if(irow/=nlevs) then
             write(stderr,'(2a,i5)') trim(myname),': inconsistent number of levels in table, aborting ...'
             call exit(4)
           endif
        end do ! iv

!       Read in additive inflation for PS
!       ---------------------------------
        call I90_label('dyn_recenter_add_inf_ps:', iret)
        if (iret==0) then
           ainf_ps = I90_GFloat(ier)
           if(ier/=0) then
              write(stderr,'(2a,i5)') trim(myname),': cannot addinf_coeff(ps), aborting ...'
              call exit(5)
           endif
        else
           write(stderr,'(2a)') trim(myname),': cannot get addinf_coeff(ps) from RC, using default ... '
        end if
        write(stdout,'(2a,f7.3)') trim(myname),': add-inf coeff (ps) = ', ainf_ps 

!       Read in additive inflation for TS
!       ---------------------------------
        call I90_label('dyn_recenter_add_inf_ts:', iret)
        if (iret==0) then
           ainf_ts = I90_GFloat(ier)
           if(ier/=0) then
              write(stderr,'(2a,i5)') trim(myname),': cannot addinf_coeff(ts), aborting ...'
              call exit(5)
           endif
        else
           write(stderr,'(2a)') trim(myname),': cannot get addinf_coeff(ts) from RC, using default ... '
        end if
        write(stdout,'(2a,f7.3)') trim(myname),': add-inf coeff (ts)= ', ainf_ts 

!       Read in additive inflation pressure threshold (Pa)
!       -------------------------------------------------
        call I90_label('dyn_addinf_pthreshold_pa:', iret)
        if (iret==0) then
           pkthresh = I90_GFloat(ier)
           if(ier/=0) then
              write(stderr,'(2a,i5)') trim(myname),': cannot pkthresh, aborting ...'
              call exit(5)
           endif
        else
           write(stderr,'(2a)') trim(myname),': cannot get pthreshold from RC, using default ... '
        end if
        write(stdout,'(2a,f7.3)') trim(myname),': add-inf pthreshold = ', pkthresh

!       Read in ocean-only option
!       -------------------------
        call I90_label('dyn_recenter_2d_ocean_only:', iret)
        if (iret==0) then
           call I90_GToken(token, iret)
           if (iret==0) then
              if(trim(token) == 'yes' .or. trim(token) == 'YES' ) then
                 ocean_only = .true.
              endif
           endif
           write(stderr,'(3a)') trim(myname),': 2d-ocean-only: ', trim(token)
        end if

!       Read in adaptive smoothing coefficient
!       --------------------------------------
        call I90_label('dyn_recenter_adaptive_smooth_factor:', iret)
        if (iret==0) then
           m_star = I90_GInt(ier)
           if(ier/=0) then
              write(stderr,'(2a,i5)') trim(myname),': cannot adaptive_smooth_factor(), aborting ...'
              call exit(5)
           endif
        else
           write(stderr,'(2a)') trim(myname),': cannot get adaptive_smooth_factor() from RC, no smoothing ... '
        end if
        write(stdout,'(2a,i7)') trim(myname),': adaptive-smoothing factor = ', m_star 


!       release resource file:
!       ---------------------
        call I90_release()

      endif ! RCfile

      if (nfiles==MFILES) then
         print *
         print *, 'Mean background   file: ', trim(files(1))
         print *, 'Mean analysis     file: ', trim(files(2))
         print *, 'Background spread file: ', trim(files(3))
         print *
      else
         print *, 'not enough input files, aborting ...'
         call exit(1)
      endif
  end subroutine init_

  real function gave_(fld,mask)
  real,intent(in) :: fld(:,:)
  real,optional,intent(in) :: mask(:,:)
  integer :: ii,jj
  real :: pi

  if(.not.allocated(coslat)) then
    pi=4.0*atan(1.0)
    allocate(coslat(size(fld,2)))
    coslat=cos(xm_b%grid%lat*pi/180.)
  endif
  gave_ = 0.0
  if ( present(mask) ) then
     do jj=1,size(fld,2)
     do ii=1,size(fld,1)
        if(mask(ii,jj)>0.99) then
           gave_ = gave_ + coslat(jj)*fld(ii,jj)
        endif
     enddo
     enddo
  else
     do jj=1,size(fld,2)
        gave_ = gave_ + coslat(jj)*sum(fld(:,jj))
     enddo
  endif
  end function gave_

  real function inflation_(factor,sprdBYerr)
  real,intent(in) :: factor
  real,intent(in) :: sprdBYerr
  if(sprdBYerr>1.e-10) then
    inflation_ = factor/sqrt(sprdBYerr)
  else
    inflation_ = factor
  endif
  inflation_ = smooth_(inflation_,factor,nparam,m_star)
  end function inflation_

  real function smooth_(alpha_upd,alpha,mdim,m_star)
  real,intent(inout) :: alpha_upd
  real,intent(in) :: alpha
  integer,intent(in) :: mdim   ! no. parameters estimated
  integer,intent(in) :: m_star ! smoothing parameter
  ! note: here I take m_star := m_star * mdim
  smooth_ = (m_star * alpha + alpha_upd) / (m_star + 1.0)
  end function smooth_

end program dyn_inflate
subroutine usage()
   print *
   print *,'Usage: '
   print *
   print *,'  dyn_inflate.x [options] xm_b xm_a xs_b'
   print *
   print *, 'where [options]'
   print *
   print *, '-h              Help (optional)'
   print *, '-mmem           Number of ensemble members'
   print *, '                (default: 32)'
   print *, '-g4             Treats files as GEOS-4 files'
   print *, '-rc      RCfile '
   print *, '                (default: dyn_recenter.rc)'
   print *
   print *, ' Required inputs:'  
   print *, '  xm_b    - ensemble mean background file'
   print *, '  xm_a    - ensemble mean analysis file'
   print *, '  xs_b    - ensemble background spread file'
   print *
   call exit(1)
end subroutine usage
      


