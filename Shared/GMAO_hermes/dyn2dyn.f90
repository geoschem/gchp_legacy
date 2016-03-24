      program dyn2dyn

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: dyn2dyn: reads dynamics vector and writes out as 32-bit/interpolated
!
! !USAGE: see the routine usage() below
!
! !USES:
!
      use m_dyn
      use m_set_eta, only : set_ncep72,unset_ncep72
      use m_dyn2dyn, only : dyn2dyn_do

      use m_StrTemplate        ! grads style templates

      implicit NONE

! !DESCRIPTION: Uses the {\em Insitu Package} {\tt m\_insitu} to convert
!               the dynamics state vector from hybrid eta coordinates 
!               written in 64-bits to 32-bits.
!
! !REMARKS: When performing horizontal interpolation this program needs
!    a GCM physics restart at the output resolution. This is to extract
!    the land-water-ice (LWI) mask array since currently we do not have a
!    smart way of interpolating LWI yet. 
!
! !TO DO: Implement a voting interpolation procedure to handle LWI.
!
!
! !REVISION HISTORY:
!
!  04Oct2001  da Silva/Lucchesi   Initial code.  Adapted from dyn2prs.
!  10Dec2002  S. Cheung/EMarvis   Minor fix for halem
!  23Sep2004  Todling             Extended to allow for vert & horiz interp.
!  28Sep2004  Todling             Placed core part of routine into m_dyn2dyn
!  04Oct2004  Todling             Modified handling of RCfile vs command line
!  08Oct2004  Todling             Added filename template handler
!  17Dec2004  Todling/Ravi        Added -oldana for backward compatibility
!  08Jul2005  Todling             Added "e" resolution
!  19Aug2005  Todling             Fixed def of "d" resolution for G5GCM
!  14dec2005  da Silva            Changed default LWI to NONE; needs recent
!                                 version of m_maph for simple LWI interp.
!  29dec2005  da Silva            Added option to force remapping even when ptop
!                                 and nlevs are the same. This is useful when
!                                 converting from lcv to eta coordinates. 
!  05Jan2006  Todling             Added fakedate option  
!  19Nov2007  Todling             A-grid handle for GEOS-5 support
!  20Feb2014  Todling             Knob for non-complaint file
!
!-------------------------------------------------------------------------
!EOP

      character(len=*), parameter :: myname = 'dyn2dyn'

!     File names
!     ----------
      integer, parameter :: MFILES = 512 ! max.   number of input files
      integer, parameter :: mynstep = 15760
      character(len=255) :: etafiles(MFILES), etafile
      character(len=255) :: expid, RCfile
      character(len=255) :: dynfile, dynftmpl
      character(len=255) :: lwifile
      integer            :: nfiles       ! actual no. of input files

!     Dynamics/simulator vectors
!     --------------------------
      type(dyn_vect) w_e  ! dynamics vector in eta (input)
      type(dyn_vect) w_o  ! fully interpolated dynamics vector in eta (output)
      type(dyn_vect) w_v  ! auxiliar dynamics vector


!     Locals
!     ------
      character(len=255) msg
      integer, parameter :: READ_ONLY = 1
      integer fid, nvars, ngatts
      integer ios, iopt, ier, ifile
      integer ntimes, n, freq, myfreq, nstep, nymd, nhms, prec, nymdf, nhmsf
      integer im, jm, km
      integer in, jn, kn
      integer vectype
      logical verbose, pick, dophys, oldana, force, fakedate, dgrid, ncep72
      logical ncf,pncf
 

!                                 *******


!  Initialize
!  ----------     
   call Init_ ( mfiles, etafiles, nfiles, dynfile, lwifile, &
                                prec, in, jn, kn, pick, nymd, nhms, myfreq,  &
                                fakedate, nymdf, nhmsf, &
                                dophys, expid, RCfile, verbose, oldana, force, &
                                vectype, dgrid, ncep72, ncf, pncf )


!  Loop over input eta files
!  -------------------------
   do ifile = 1, nfiles

      etafile = etafiles(ifile)

!     Determine how many time levels on file
!     --------------------------------------
      call GFIO_Open ( etafile, READ_ONLY, fid, ier )
      if ( ier .ne. 0 ) then
          call die(myname,'cannot open GFIO file '//trim(etafile))
      end if
      call GFIO_DimInquire ( fid, im, jm, km, ntimes, nvars, ngatts, ier)
      if ( ier .ne. 0 ) then
         call die(myname,'problems getting dimensions' )
      end if
      call GFIO_Close ( fid, ier )

      if ( pick ) ntimes = 1
      
!     For each time on file...
!     ------------------------
      do n = 1, ntimes

!        Get ETA data for this time
!        --------------------------
         if ( oldana ) then
              if ( pick ) then
                 call dyn_get ( etafile, nymd, nhms, w_e, ier, timidx=0, freq=freq, vectype=vectype, ncf=ncf, pncf=pncf )
                 if ( myfreq/=0 ) freq = myfreq  ! reset frequency to whatever user's want
              else
                 call dyn_get ( etafile, nymd, nhms, w_e, ier, timidx=n, freq=freq, vectype=vectype, ncf=ncf, pncf=pncf )
              endif
              nstep = mynstep
         else
              if ( pick ) then
                 call dyn_get ( etafile, nymd, nhms, w_e, ier, timidx=0, freq=freq, nstep=nstep, vectype=vectype, ncf=ncf, pncf=pncf )
                 if ( myfreq/=0 ) freq = myfreq  ! reset frequency to whatever user's want
              else
                 call dyn_get ( etafile, nymd, nhms, w_e, ier, timidx=n, freq=freq, nstep=nstep, vectype=vectype, ncf=ncf, pncf=pncf )
              endif
         endif
         if ( ier .ne. 0 ) then
            write(msg,'(a,i4)') 'cannot read dynamics vector file, ier= ',ier
            call die(myname,msg)
         end if

!        Overwrite input date/time with user-prescribed date/time
!        --------------------------------------------------------
         if ( fakedate ) then
              nymd = nymdf
              nhms = nhmsf
         end if
         if(ncep72) call set_ncep72

!        Perform interpolation
!        ---------------------
         if ( trim(RCfile)=='NONE' ) then

              call dyn2dyn_do ( w_e,  &
                                in, jn, kn, verbose, ier, &
                                dynfile=dynfile, lwifile=lwifile, &
                                nymd=nymd, nhms=nhms, prec=prec, freq=freq, nstep=nstep,   &
                                dophys=dophys, force=force, dgrid=dgrid, vectype=vectype )

         else

!             Construct file name
!             -------------------
              dynftmpl = trim(dynfile)
              call strTemplate ( dynfile, dynftmpl, 'GRADS', xid=expid, &
                                 nymd=nymd, nhms=nhms, stat=ier )
                  if (ier/=0) call die(myname,'cannot determine file via template')

              call dyn2dyn_do ( dynfile, w_e, nymd, nhms, freq, nstep, ier, &
                                dophys=dophys, expid=expid, RCfile=RCfile, force=force, &
                                dgrid=dgrid, vectype=vectype )
         endif
            if( ier/=0 ) call die(myname,'failed to complete interpolation')

!        Clean up mess
!        -------------
         call dyn_clean ( w_e )
         if(ncep72) call unset_ncep72

      end do

   end do ! loop over files

!  All done
!  --------
   call exit(0)

CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: Init_ --- Initialize dyn2dyn
!
! !DESCRIPTION: parses command line.
!
! !INTERFACE:
!
      subroutine Init_ ( mfiles, etafiles, nfiles, dynfile, lwifile, &
                         prec, in, jn, kn, pick, nymd, nhms, myfreq, &
                         fakedate, nymdf, nhmsf,                     &
                         dophys, expid, RCfile, verbose, oldana, force, &
                         vectype, dgrid, ncep72, ncf, pncf )

      implicit NONE

      integer,       intent(in)  :: mfiles  ! max. number of eta files
                                            ! dynamics file names (eta)
      character*255, intent(out) :: etafiles(mfiles) 
      character*255, intent(out) :: dynfile ! dynamics file name (32 bit)
      character*255, intent(out) :: lwifile ! file containing LWI field
      integer,       intent(out) :: nfiles  ! actual no. of eta files
      integer,       intent(out) :: prec    ! precision of output dyn-vect
      integer,       intent(out) :: in      ! number of longitude points
      integer,       intent(out) :: jn      ! number of latitude  points
      integer,       intent(out) :: kn      ! number of vertical levels for output
      integer,       intent(out) :: nymd    ! date in YYYYMMDD
      integer,       intent(out) :: nhms    ! time in HHMMSS
      integer,       intent(out) :: myfreq  ! user specified freq in file (only used w/ -pick)
      logical,       intent(out) :: pick    ! allows pick specific date/time in file
      logical,       intent(out) :: fakedate! allows output file to have user-prescribed date/time
      integer,       intent(out) :: nymdf   ! user prescribed date in YYYYMMDD
      integer,       intent(out) :: nhmsf   ! user prescribed time in HHMMSS
      logical,       intent(out) :: verbose ! set verbose mode
      logical,       intent(out) :: dophys  ! controls vert.diff.coefs. interpolation
      logical,       intent(out) :: oldana  ! allows reading from eta files w/ nstep parameter
      character*255, intent(out) :: expid   ! experiment name
      character*255, intent(out) :: RCfile  ! resource file
      logical,       intent(out) :: force   ! force zmapping
      integer,       intent(out) :: vectype ! GEOS-4 or GEOS-5 dyn vect
      logical,       intent(out) :: dgrid   ! GEOS-4 or GEOS-5 switch for winds grid
      logical,       intent(out) :: ncep72  ! Set NCEP-like-levels, but 72 of them
      logical,       intent(out) :: ncf     ! non-complaint dyn-file knob
      logical,       intent(out) :: pncf    ! non-complaint dyn-perturbation file knob
      
!
! !REVISION HISTORY:
!       04Oct2001  da Silva/Lucchesi  Initial code.
!       23Sep2004  Todling            Enhanced w/ interpolation features.
!       11Feb2005  Todling            Added -freq opt
!       08Jul2005  Todling            Added logics to handle geos4/5 hor res diffs
!       05Jan2006  Todling            Added fakedate option
!       21Apr2009  Todling            Updated default hor/ver resolutions of GEOS-5
!       20Feb2014  Todling            Knob for non-complaint file
!       27Jan2015  Todling            Add 137-level option
!
!EOP
!BOC

      character*4, parameter :: myname = 'init'

      integer iret, i, iarg, argc, iargc
      integer uprec, iprec, ires
      logical verb, setres, geos4res
      character(len=255) :: etafile, argv, res
      character*10 str

      integer, dimension(6), parameter :: IMS4 = (/ 72, 144, 288, 576, 1152, 2304 /)
      integer, dimension(6), parameter :: IMS5 = (/ 72, 144, 288, 576, 1152, 2304 /)
      integer, dimension(6), parameter :: JMS  = (/ 46,  91, 181, 361,  721, 1441 /)
      integer, dimension(7), parameter :: KMS  = (/ 18,  32,  55,  64,   72,   91, 137 /)

!     Defaults
!     --------
      prec = 0             ! output dyn vect written in 32 bits
      in   = 0             ! default is to return original resolution
      jn   = 0             ! default is to return original resolution
      kn   = 0             ! default is to return original resolution
      verbose=.false.      ! default is no verbose
      force = .false.      ! do not remap if nlevs are the same
      oldana=.false.       ! default read nstep as in latest eta files
      lwifile='NONE'       ! default filename of file containing LWI field
      pick = .false.       ! default is do all data in file
      fakedate = .false.   ! default is to use input file date/time
      nymd = 0             ! default unknown
      nhms = 0             ! default unknown
      nymdf= 0             ! default unknown
      nhmsf= 0             ! default unknown
      myfreq = 0           ! default freq of output dyn as from input dyn
      dophys=.false.       ! default is to not interpolate phys vert. diff. coefs
      expid='NONE'         ! default name of experiment
      RCfile='NONE'        ! default not to use an RCfile
      setres=.false.       ! default no horizontal resolution change
      geos4res=.false.     ! default use geos-5 horizontal resolution defs
      vectype = 4          ! default: assume vector is GEOS-4-type
      dgrid   = .true.     ! default: in GEOS-4 dyn-vector winds are on D-grid
      ncep72  = .false.    ! default: use usual GMAO-72 level
      ncf     = .false.    ! default: handle usual dyn-complaint file
      pncf    = .false.    ! default: handle usual dyn-complaint file

!     Parse command line
!     ------------------
      dynfile = 'DEFAULT'
      argc =  iargc()
      if ( argc .lt. 1 ) call usage()

      iarg = 0
      nfiles = 0

      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) exit
         call GetArg ( iarg, argv )
         select case (argv)
           case ("-vdc")
             dophys = .true.
           case ("-o")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, dynfile )
           case ("-lwi")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, lwifile )
           case ("-rc")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, RCfile )
           case ("-expid")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, expid )
           case ("-res")
             if ( iarg+1 .gt. argc ) call usage()
             iarg = iarg + 1
             call GetArg ( iarg, res )
             select case (res)
               case ("a")
                     ires=1
               case ("b")
                     ires=2
               case ("c")
                     ires=3
               case ("d")
                     ires=4
               case ("e")
                     ires=5
               case ("f")
                     ires=6
               case default
                     print *, 'Sorry this resolution not supported'
                     call exit(1)
             end select
             setres = .true.
           case ('-verb')
            verbose = .true.
           case ('-force')
            force = .true.
           case ('-geos4')
            geos4res = .true.
           case ('-oldana')
            oldana = .true.
           case ('-ncep72')
            ncep72 = .true.
           case ('-nlevs')
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iarg, str )
            read(str,*) kn
            if (count(kn==KMS).eq.0) then
                print *, 'Cannot handle this vertical number of levels'
                call exit(1)
            endif
           case ('-pick')
               if ( iarg+2 .gt. argc ) call usage()
               iarg = iarg + 1
               call GetArg ( iarg, argv )
               read(argv,*) nymd
               iarg = iarg + 1
               call GetArg ( iarg, argv )
               read(argv,*) nhms
               pick = .true.
           case ('-fakedate')
               if ( iarg+2 .gt. argc ) call usage()
               iarg = iarg + 1
               call GetArg ( iarg, argv )
               read(argv,*) nymdf
               iarg = iarg + 1
               call GetArg ( iarg, argv )
               read(argv,*) nhmsf
               fakedate = .true.
           case ('-freq')
               if ( iarg+1 .gt. argc ) call usage()
               iarg = iarg + 1
               call GetArg ( iarg, argv )
               read(argv,*) myfreq
           case ('-g5')
               vectype = 5
               dgrid   = .false.
           case ('-ncf')
               ncf = .true.
           case ('-pncf')
               pncf = .true.
           case ('-prec')
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iarg, str )
            read(str,*) iprec
            if(iprec==32)prec=0
            if(iprec==64)prec=1
           case default
             nfiles = nfiles + 1
             if ( nfiles .gt. mfiles ) call die(myname,'too many eta files')
             etafiles(nfiles) = argv
         end select
      end do

      if ( nfiles .lt. 1 ) call usage()

!     If not specified, come up with a good name for dyn file
!     -------------------------------------------------------
      etafile = etafiles(1)
      if ( trim(dynfile) .eq. 'DEFAULT' ) then 
         i = index ( etafile, '.eta.' )
         if ( i .gt. 1 ) then
            if(prec==0) dynfile = etafile(1:i-1) // '.eta32.' // etafile(i+5:)
            if(prec==1) dynfile = etafile(1:i-1) // '.etait.' // etafile(i+5:)
         end if
      end if

      if ( setres ) then
           if ( geos4res ) then
             in = ims4(ires)
             jn = jms (ires)
           else
             in = ims5(ires)
             jn = jms (ires)
           endif
      endif

      if (verbose) then
      print *
      print *, '  -----------------------------------------------------------'
      print *, '  dyn2dyn - converts (64/32bits) and interpolates dyn file(s)'
      print *, '  -----------------------------------------------------------'
      print *
      endif

!     Always echoes the parameters (verbose or not)
!     ---------------------------------------------
      print *
      print *, '------------------------------------------------------------------'
      print *, '  Eta     Dynamics state files: ', nfiles
      do i = 1, nfiles
      print *, '                                ', trim(etafiles(i))
      end do

      end subroutine Init_

!.................................................................

      subroutine usage()
      print *
      print *, '  -----------------------------------------------------------'
      print *, '  dyn2dyn - converts (64/32bits) and interpolates dyn file(s)'
      print *, '  -----------------------------------------------------------'
      print *
      print *
      print *,'Usage: '
      print *,'  dyn2dyn.x [-verb] [-o dynfile ] [-prec PREC] [-pick nymd nhms] [-oldana]'
      print *,'                    [-vdc] [-lwi fname] [-res RES] [-nlevs NLEVS] etafile(s)'
      print *,'  Alternatively: '
      print *,'  dyn2dyn.x [-verb] [-o dynfile ] [-rc RCfile] [-expid XID] [-vdc] etafile(s)'
      print *
      print *, 'where'
      print *
      print *, '-verb         specify to set verbose on'
      print *, '-force        force z-mapping, even if nlevs is the same'
      print *, '-oldana       to read dyn vector from old files not having nstep'
      print *, '-o dynfile    output dynamics vector file'
      print *, '              Default: if 32bit conversion only, '
      print *, '              if 32-bits conversion only, same as first "etafile"'
      print *, '              with substring ".eta." replaced with ".eta32."; other'
      print *, '              substring replaced with ".etait.".'
      print *, '-prec  PREC   where PREC=32 or 64 (default 32bits)'
      print *, '-pick  date/time  allows selection of scecific time from input file'
      print *, '-fakedate  date/time  allows user to prescribe output file date/time'
      print *, '-freq  HHMMSS frequency of output in file (only to be used w/ -pick opt)'
      print *, '-res   RES    where RES= a, b, c, d, and e(*) '
      print *, '              (*NOTE: the e resolution is diff for GEOS-4 and GEOS-5 gcm' 
      print *, '                      see geos4 flag below)'
      print *, '-geos4        specify when trying to convert dyn-vect to geos4 e resolution'
      print *, '              (Default: GEOS-5 e resolution)'
      print *, '-nlevs NLEVS  where NLEVS is the number of vertical levs out'
      print *, '-vdc          interpolates vert. diff. coeffs also'
      print *, '              (default: dont do it)'
      print *, '-lwi   FNAME  name of file containing LWI field at final resolution'
      print *, '              (only required when performing horizontal interpolation)'
      print *, '-expid XID    Alternatinely could set expid and use an RCfile '
      print *, '              (default: NONE)'
      print *, '-rc    RCfile Alternatinely could set expid and use an RCfile '
      print *, '              (default: NONE)'
      print *, '-etafile(s)   input dynamics vector file in'
      print *, '              hybrid (eta) coordinates'
      print *, '-g5           specify when using GEOS-5 vector (not all transforms work)'
      print *, '              (default: GEOS-4 vector-type)'
      print *, '-ncf          non-compliant dyn-vector (see NOTES)'
      print *, '-pncf         non-compliant dyn-vector perturbation (see NOTES)'
      print *
      print *, ' NOTES:'
      print *, '   1)  For the time being, in order to do horizontal'
      print *, '       interpolation the user can supply the '
      print *, '       land-water-ice (LWI) mask array via a binary'
      print *, '       unformatted sequential file at the resolution'
      print *, '       of the desired output; as it turns out a physics'
      print *, '       restart (p_rst) file from FVGCM will do it.'
      print *, '       If this file is not specified, a crude nearest neighbor'
      print *, '       interpolation is implemented which is good enough'
      print *, '       if you do not need LWI, say for replay.' 
      print *
      print *, '   2)  ncf converts files containing a sub-set of variables'
      print *, '       used in the dyn-vector to a full dyn-vector; these files '
      print *, '       typically come from ncep2gmao.pl (and ec2gmao.pl).'
      print *
      print *, '   3)  pncf converts files containing a sub-set of perturbation'
      print *, '       fields typically generated by GSI when putting out '
      print *, '       increments from a minimization (4dvar/4densvar).'
      print *
      print *, ' Last updated: 05 Jan 2006; Todling '
      print *
      call exit(1)
      end subroutine usage
      
!.................................................................

      subroutine die ( myname, msg )
      character(len=*) :: myname, msg
      write(*,'(a)') trim(myname) // ': ' // trim(msg)
      call exit(1)
      end subroutine die

!.................................................................

  end program dyn2dyn
