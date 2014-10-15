      program regrid
      parameter(nimax=4608,njmax=2881,n1max=nimax*2,
     $     niopt=4,nvopt=8)

      real vals(20),ovals(20),args(8)

      dimension xin(n1max),yin(n1max),xout(n1max),yout(n1max)
      dimension gxout(n1max),gyout(n1max)
      double precision glat(n1max),glatb(n1max)

      dimension dumx(n1max)

      dimension incard(80)
      character*10 carg(niopt)
      character*2 copt(nvopt)

      real, allocatable :: dum1   (:,:)
      real, allocatable :: fld_in (:,:), area_in (:,:)
      real, allocatable :: fld_out(:,:), area_out(:,:)

      character card*80,qtitle*24,regrid_method*40
      character incard*1

      logical cyclicxi,wrapxi,gauss_out,
     $     diag_out,vote,bessel,
     $     npole_point,spole_point,
     $     cray

      equivalence (incard(1),card)

      data copt/'ig','ba','bl','vt','bs','p1','gg','ma'/
C         
C         gfi - input file to the program with the GrADS data object
C         
C         gfo - output file to be read by GrADS
C
      iunit_in=8
      iunit_out=10
      iunit_diag=12
      iunit_dat=14

      cray=.true.
      cray=.false.

      if(cray) then

        card='assign -s unblocked -a udf.regrid.gfi fort.8'
cc        call assign(card)
        card='assign -s unblocked -a udf.regrid.gfo fort.10'
cc        call assign(card)
        open (iunit_diag,file='udf.regrid.out',
     $       form='formatted',status='unknown')

      else

C         
C         950202:   set the location of the udf files to /tmp.......
C         950427:   changed back to avoid conflicts between MULTIPLE regrid processes
C
        open (iunit_in,file='udf.regrid.gfi',status='unknown',
     $       form='unformatted',err=810)
        open (iunit_out,file='udf.regrid.gfo',status='unknown',
     $       form='unformatted')
        open (iunit_diag,file='udf.regrid.out',
     $       form='formatted',status='unknown')

      endif

C         
C         constants
C
      pi=3.141592654
      deg2rad=pi/180.0
      rad2deg=180.0/pi
C         
C         defaults
C
      diag_out=.false.
ccc      diag_out=.true.
C         
C         user defined beginning lat/lon
C
      rlatbeg_set=-999.0
      rlonbeg_set=-999.0
C         
C         pole points
C         
      spole_point=.false.
      spole_point=.false.
C         
C         initialize flag for using calculated gauss lat weights
C
      jsglat=-888
C         
C         the default method is box averaging without voting on
C         a unform grid
C         
      iregrid_type=1
      iregrid_method=1
      ilinearo=1
      jlinearo=1
      bessel=.false.
      vote=.false.
      gauss_out=.false.
C         
C         minimum fractional area covered by data in box averaging
C         to have a defined point
C         
      area_min=0.5
C         
C         voting defaults --
C         50% of grid box must be covered regardless of the number
C         of candidates
C         
          rmin_vote_max=0.5
          rmin_vote_min=0.5
C         
C         the first record contains number of args
C         and other parameters for future implementations
C

      read (iunit_in) vals
      nargs=nint(vals(1))
ccc      write(*,*) 'number of arguments = ',nargs
C         
C         stop if only one argument
C         
      if(nargs.le.1) then
        write(*,12) 
 12     format(' ',/
     $       /' ','regrid requires 2 or more arguements:',
     $       /' ',' ')
        go to 999
      endif
C         
C         the second record is the field ("expr" in the user defined
C         function table) and has is input using four reads
C         
C         grid record 1
C
      read(iunit_in,err=820) vals
C         
C         grid parameters
C
      undef=vals(1)
      itype=nint(vals(2))
      jtype=nint(vals(3))
      nii=nint(vals(4))
      nji=nint(vals(5))
      ilineari=nint(vals(6))
      jlineari=nint(vals(7))
      xbeg=vals(8)
C         
C         check the input grid type
C         
C         abort if not 2-d x,y
C         
      if(itype.ne.0.or.jtype.ne.1) go to 830
C         
C         get grid increments if uniform
C         
      if(ilineari.eq.1) then
        dx=vals(9)
      else
        dx=-999
      endif
      ybeg=vals(10)

      if(jlineari.eq.1) then
        dy=vals(11)
      else
        dy=-999
      endif
C         
C         only get time values if a one of the dimensions is time
C     
      if(itype.eq.3.or.jtype.eq.3) then
        yrbeg=vals(12)
        mobeg=vals(13)
        dabeg=vals(14)
        hrbeg=vals(15)
        mnbeg=vals(16)
        dmn=vals(17)
        dmo=vals(18)
      endif
C         
      write(iunit_diag,102) undef,itype,jtype,nii,nji,
     $     ilineari,jlineari,xbeg,dx,ybeg,dy
 102  format(' ',
     $     /,10x,'          undefined value = ',e13.5,
     $     /,10x,'         x dimension type = ',i3,
     $     /,10x,'         y dimension type = ',i3,
     $     /,10x,'         x dimension size = ',i5,
     $     /,10x,'         y dimension size = ',i5,
     $     /,10x,' uniform grid spacing in x ? ',i2,
     $     /,10x,' uniform grid spacing in y ? ',i2,
     $     /,10x,'   x world coor beginning  = ',e12.4,
     $     /,10x,'                        dx = ',e12.4,
     $     /,10x,'   y world coor beginning  = ',e12.4,
     $     /,10x,'                        dy = ',e12.4)


      if(itype.eq.3.or.jtype.eq.3) then
        write(iunit_diag,104) 
     $       yrbeg,mobeg,dabeg,hrbeg,mnbeg,dmn,dmo
 104    format(' ',
     $       /,10x,'                year start = ',e12.4,
     $       /,10x,'               month start = ',e12.4,
     $       /,10x,'                 day start = ',e12.4,
     $       /,10x,'                hour start = ',e12.4,
     $       /,10x,'              minute start = ',e12.4,
     $       /,10x,'           minute increment = ',e12.4,
     $       /,10x,'            month increment = ',e12.4)
      endif

C
C         grid record #2 the data
C         
      allocate(   dum1(nii,nji) )
      allocate( fld_in(nii,nji) )
      allocate(area_in(nii,nji) )

      call read_grid(iunit_in,fld_in,nii,nji,istat)
      if(istat.ne.1) go to 820

      qtitle='input field from GrADS '
      call qprntu(fld_in,qtitle,undef,1,1,nii,nji,4,iunit_diag)
C         
C         grid record #3 and 4 -- grid-to-world coordinate maps
C         of the input grid
C         
      read(iunit_in,err=820) (xin(i),i=1,nii)
      write(iunit_diag,*) (xin(i),i=1,nii)
      read(iunit_in,err=820) (yin(j),j=1,nji)
      write(iunit_diag,*) (yin(j),j=1,nji)

C         
C------------------------------------------------------------
C
C         read arguments after the grid -- two floats
C         
C         #1 - dx or # gaussian lats
C         #2 - dy or # gaussian lons
C         
C         if only 1 then dx=dy
C         if only 2 then method = box averaging
C         
C------------------------------------------------------------
C         
C         decrement ngarg by one for the grid
C
      nargs=nargs-1
      if(nargs.ge.2) then
        nargrd=2
      else
        nargrd=1
      endif

      do i=1,nargrd
        read(iunit_in,err=832) args(i)
      end do
C         
C         if one argument, then assume uniform lat lon with equal dx
C         
      if(nargrd.eq.1) then
        iout_grid_type=1
        iregrid_method=1
        dxout=args(1)
        dyout=args(1)

      else 
C         
C         if two arguments, dx & dy are specified the
C         we run box ave by default
C         
        iout_grid_type=1
        iregrid_method=1
        dxout=args(1)
        dyout=args(2)
C
C         method
C         
        iregrid_type=1
        iregrid_method=1
      endif
C         
C------------------------------------------------------------
C         
C         read the third argument -- the char option string
C         
C         this specifies:
C
C         1)  we are telling regrid that the input grid
C         is gaussian and the resolution (number of gaussian latitudes)
C         
C         2)  interp method
C         
C         3)  starting lon,lat of the (1,1) of the output grid
C
C------------------------------------------------------------
C         
C         decrement nargs based on the last reads
C         

      nargs=nargs-nargrd
C         
C         number of char options
C
      nca=0
C         
C         we have char args...
C
      if(nargs.ge.1) then

        read(iunit_in,err=832) incard
        lenic=ichar_len(incard,80)
C         
C         parse it and check for proper lengths
C         
        nca=1
        ib=1
        do ii=1,lenic

          carg(nca)='          '
          if(incard(ii).eq.'_') then
            ie=ii-1
            ilen=ie-ib+1
            carg(nca)(1:ilen)=card(ib:ie)
            if(carg(nca)(1:2).eq.'ig'.or.carg(nca)(1:2).eq.'ma') then
              if(ilen.eq.2) go to 812
            else
              if(ilen.gt.2) go to 812
            endif

            ib=ie+2
            nca=nca+1

          endif

          if(ii.eq.lenic) then
            ie=lenic
            ilen=ie-ib+1
            carg(nca)(1:ilen)=card(ib:ie)
            if(carg(nca)(1:2).eq.'ig'.or.carg(nca)(1:2).eq.'ma') then
              if(ilen.eq.2) go to 812
            else
              if(ilen.gt.2) go to 812
            endif

          endif

        enddo
C         
C         too many args
C
        if(nca.gt.niopt) go to 814 
C         
C***      process the args
C         
        do ii=1,nca
          iok=0
ccc          print*,'ii = ',ii,carg(ii)
          do jj=1,nvopt
            if(carg(ii)(1:2).eq.copt(jj)) then
              iok=1
            endif
          enddo
C         
C***      invalid carg
C         
          if(iok.ne.1) then
            nca=ii
            go to 812
          endif
C         
C***      min area for defined data
C
          if(carg(ii)(1:2).eq.'ma') then
            read(carg(ii)(3:5),'(i3)') i1
            area_min=i1*0.01
C         
C         decrease area_min slightly for 100%
C         because of numerical differences between
C         sum of intersections and the actual sfc area
C         of the output grid boxes
C
            if(area_min.ge.1.0) area_min=0.96
          endif
C         
C***      number of gaussian lats in input grid
C***      to invoke the more precise calculation 
C***      of the box boundaries
C
          if(carg(ii)(1:2).eq.'ig') then

            eps_glat=0.01
            read(carg(ii)(3:5),'(i3)') njglat
C         
C         make sure this IS a valid gaussian grid 
C         and can be calculated by the routine
C         
            if(mod(njglat,2).ne.0.or.njglat.lt.8) then
              write(*,'(/,a,/,a,/)') 
     $             'number of input gaussian lats invalid',
     $             '(not a factor of 1) bypass precise weighting'
            else
C         
C         get the latitudes and boundaries using the pcmdi routine
C         
              call gauss_lat_pcmdi(glat,glatb,njglat)

              do j=1,njglat
                write(*,'(i3,3(2x,f7.3))') 
     $               j,glat(j),glatb(j),glatb(j+1)
              end do
              
              do j=1,nji
                write(*,'(i3,2x,f7.3)') j,yin(j)
              end do
C         
C         now see if we can match the input grid to the assumed
C         gaussian grid
C         
              jstest=1
              if(yin(jstest).lt.-90.0) jstest=2

              do j=1,njglat
                if(abs(yin(jstest)-glat(j)).le.eps_glat) then
                  if(diag_out) then
                    write(iunit_diag,*) 
     $                   ' We have a near match use the calcuated grid'
                    write(iunit_diag,*) 
     $                   ' jsglat = ',j,' jstest = ',jstest
                    write(iunit_diag,*) 
     $                   ' abs(yin(jstest) =  ',yin(jstest)
                    write(iunit_diag,*) 
     $                   ' glat(j) = ',glat(j)
                  endif
                  jsglat=j
C         
C         quick exit
C         
                  go to 9001
                endif
              end do

 9001         continue

            endif

          endif


        enddo

      endif

C         
C         
C------------------------------------------------------------
C         
C         input grid parameters
C         check for cyclic continuity in x 
C         
C------------------------------------------------------------
C
      cyclicxi=.false.
      wrapxi=.false.
C
C         make sure the x dimension is longitude and is uniform
C         
      if(itype.eq.0.and.ilineari.eq.1) then
        xlen=(nii-1)*dx
        if(xlen.ge.360.0) then
          wrapxi=.true.
          cyclicxi=.true.
          niifix=360.0/dx
          xlen=360.0
        endif

        xlen=nii*dx
        if(xlen.eq.360.0) then
          wrapxi=.false.
          cyclicxi=.true.
        endif

      endif

      if(diag_out) 
     $     write(iunit_diag,*) 'cyclicxi: ',cyclicxi,wrapxi
C         
C------------------------------------------------------------
C
C         if wrapped in x, then trim the grid
C
C         dump the input data into a dummy array
C         and then load the trimmed grid into the original field array
C
C------------------------------------------------------------
C         
      if(wrapxi) then
        call vload2(dum1,fld_in,nii,nji)
        call trim_grid(dum1,nii,niifix,nji,fld_in)
C         
C         set the input size to the new trimmed x dimension
C         
        nii=niifix
      endif
C         
C------------------------------------------------------------
C         
C         set up the input grid
C         
C------------------------------------------------------------
C
C         boundaries of input grid boxes
C         
      rlonbegi=xin(1)
      rlonendi=xin(nii)
      if(cyclicxi) rlonedi=xin(1)+360.0

      rlatbegi=yin(1)
      rlatendi=yin(nji)

      niip1=nii+1
      njip1=nji+1
C         
C         x input grid box boundaries
C         
      do i=2,nii
        dumx(i)=0.5*(xin(i-1)+xin(i))
      end do

      dumx(1)=xin(1)-0.5*(xin(2)-xin(1))
      dumx(niip1)=xin(nii)+0.5*(xin(nii)-xin(nii-1))

      do i=1,niip1
        xin(i)=dumx(i)
      end do
C         
C         y input grid box boundaries
C         
      if(jsglat.ne.-888) then
C         
C         use the calculated gaussian boundaries
C         
        dumx(1)=yin(1)-0.5*(yin(2)-yin(1))
        dumx(njip1)=yin(nji)+0.5*(yin(nji)-yin(nji-1))

        jend=njip1
        if(jend.gt.njglat+1) jend=njglat+1

        if(jstest.eq.1) then

          do j=1,jend
            jj=jsglat+(j-1)
            if(j.ne.1.and.j.ne.njip1) 
     $           dumx(j)=0.5*(yin(j-1)+yin(j))
            if(diag_out) 
     $           write(iunit_diag,*) 'jstest 1 ',j,jj,dumx(j),glatb(jj)
            dumx(j)=glatb(jj)
          end do

        else if(jstest.eq.2) then

          write(iunit_diag,*) 'jstest 2 1 = ',dumx(1)
          do j=2,jend
            jj=jsglat+(j-jstest)
            if(j.ne.1.and.j.ne.njip1) 
     $           dumx(j)=0.5*(yin(j-1)+yin(j))
            if(diag_out) 
     $           write(iunit_diag,*) 'jstest 2 ',j,jj,dumx(j),glatb(jj)
            dumx(j)=glatb(jj)
          end do

        end if


      else
C         
C         use the input lat -> j map for the boundaries
C
        do j=2,nji
          dumx(j)=0.5*(yin(j-1)+yin(j))
        end do

        dumx(1)=yin(1)-0.5*(yin(2)-yin(1))
        dumx(njip1)=yin(nji)+0.5*(yin(nji)-yin(nji-1))

      endif
C         
C         make sure the input grid does not extend beyond the poles
C
      do j=1,njip1
        yin(j)=dumx(j)
        if(yin(j).lt.-90.0) yin(j)=-90.0
        if(yin(j).gt.90.0) yin(j)=90.0
      end do

      if(diag_out) then
        write(iunit_diag,*) 'input grid x boundaries'
        write(iunit_diag,*) (xin(i),i=1,niip1)
        write(iunit_diag,*) 'input grid y boundaries'
        write(iunit_diag,*) (yin(j),j=1,njip1)
      endif

C         
C------------------------------------------------------------
C
C         read arguments after the char options
C         
C         the first is the grid and the second in the char options
C
C------------------------------------------------------------
C         
C         decrement nargs if there were char opts
C
      if(nca.gt.0) nargs=nargs-1

      if(nargs.ne.0) then
        do ii=1,nargs
          read(iunit_in,err=832) args(ii+nargrd)
        enddo
      endif

      istrvt=999
      istrp1=999

      if(nca.ne.0) then

        do ii=1,nca

          if(carg(ii)(1:2).eq.'gg') then

C         
C         Gaussian Output Grid
C         
            niog=nint(args(1))
            njog=nint(args(2))
            nwaves=niog/3-1
            gauss_out=.true.
            
C         
C         calculate the gaussian latitudes and 
C         the grid spacing for the gaussian longitudes
C         
            call gauss_lat_nmc(dumx,njog)
            
            ilinearo=1
            jlinearo=0
            dxout=360.0/float(niog)
            dyout=180.0/float(njog)
            dyoutg=dyout
            iout_grid_type=2
            
          endif
C         
C*****         method
C         
          if(carg(ii)(1:2).eq.'vt') then
            iregrid_method=1
            vote=.true.
            if(nargs.gt.0) istrvt=ii
          else if(carg(ii)(1:2).eq.'ba') then
            dxout=args(1)
            dyout=args(2)
            iregrid_method=1
C         
C         change the type of map if gauss out grid
C
            if(gauss_out) then
              ilinearo=1
              jlinearo=0
            endif
            vote=.false.
          else if(carg(ii)(1:2).eq.'bl') then
            iregrid_method=2
            bessel=.false.
          else if(carg(ii)(1:2).eq.'bs') then
            iregrid_method=2
            bessel=.true.
          end if
C         
C         starting lon/lat of the (1,1) point
C         
          if(carg(ii)(1:2).eq.'p1') then
            istrp1=ii
          endif

        end do
C         
C         SPECIAL OPTIONS
C         
C         case 1 - voting options
C         case 2 - user-defined beginning lat/lon
C         
        if(nargs.eq.4) then
          
          if(istrvt.eq.999.or.istrp1.eq.999) go to 838 

          if(istrvt.lt.istrp1) then

            rmin_vote_max=args(3)
            rmin_vote_min=args(4)
            rlonbeg_set=args(5)
            rlatbeg_set=args(6)

          else

            rlonbeg_set=args(3)
            rlatbeg_set=args(4)
            rmin_vote_max=args(5)
            rmin_vote_min=args(6)

          endif


        else if(nargs.eq.2) then
          if(istrvt.ne.999.and.istrp1.ne.999) then
C         
C         both vt and p1 have been set, then use defaults for
C         vt and set the start lon,loat
C
            rlonbeg_set=args(3)
            rlatbeg_set=args(4)
          else if(istrvt.ne.999) then
            rmin_vote_max=args(3)
            rmin_vote_min=args(4)
          else if(istrp1.ne.999) then
            rlonbeg_set=args(3)
            rlatbeg_set=args(4)
          else 
            go to 839
          endif

        else if(nargs.ne.0) then
          
          go to 839

        endif


      endif
C         
C         QC the input
C         
C         #1 - setting the (1,1) point for a gaussian output grid NOT!
C 
      if(iout_grid_type.eq.2.and.rlonbeg_set.ne.-999.0) then
        rlonbeg_set=-999.0
        rlatbeg_set=-999.0
      endif

      if(diag_out) then
      write(iunit_diag,*) 'iout_grid_type = ',iout_grid_type
      write(iunit_diag,*) 'iregrid_method = ',iregrid_method
      write(iunit_diag,*) '         dxout = ',dxout
      write(iunit_diag,*) '         dyout = ',dyout
      write(iunit_diag,*) '          vote = ',vote
      write(iunit_diag,*) '        bessel = ',bessel
      write(iunit_diag,*) ' rmin_vote_max = ',rmin_vote_max
      write(iunit_diag,*) ' rmin_vote_min = ',rmin_vote_min
      write(iunit_diag,*) '   rlonbeg_set = ',rlonbeg_set
      write(iunit_diag,*) '   rlatbeg_set = ',rlatbeg_set
      endif

C
C------------------------------------------------------------
C         
C         set up the output grid based on the arguments
C
C------------------------------------------------------------
C
C         iout_grid_type 1 - uniform lat/lon in both directions
C
      if(iout_grid_type.eq.1) then

        rlonbego=ifix(rlonbegi/dxout)*dxout
        rlonendo=ifix(rlonendi/dxout)*dxout
        if(cyclicxi) rlonendo=rlonbego+(360.0-dxout)
        rlatbego=ifix(rlatbegi/dyout)*dyout
        rlatendo=ifix(rlatendi/dyout)*dyout
C         
C         check for a global input gaussian grid
C         
        yleni=yin(njip1)-yin(1)
        if(yleni+dyout.ge.180.0) then
          rlatbego=-90.0
          rlatendo=90.0
        endif

        nio=nint((rlonendo-rlonbego)/dxout)+1
        njo=nint((rlatendo-rlatbego)/dyout)+1

C         
C         make sure we have at least a 2x2 output grid
C 
        if(nio.lt.2) then
          nio=2
          rlonendo=rlonbego+2.0*dxout
        endif
        if(njo.lt.2) then
          njo=2
          rlatendo=rlonbego+2.0*dyout
        endif
C         
C         user defined beginning longitude
C         
        if(rlonbeg_set.ne.-999.0) then
          rlonbego=rlonbeg_set
C         
C         cylic continuity check
C
          if(cyclicxi) then
            nio=nint(360.0/dxout)
          else
            nio=nint((rlonendi-rlonbego)/dxout)+1
          endif
          if(nio.lt.2) nio=2
          rlonendo=rlonbego+(nio-1)*dxout
        endif

        if(rlatbeg_set.ne.-999.0) then
          rlatbego=rlatbeg_set
          njo=nint((rlatendi-rlatbego)/dyout)+1
          if(njo.lt.2) njo=2
          rlatendo=rlatbego+(njo-1)*dyout
C         
C         bounds check
C         
          if(rlatendo.gt.90.0) then
            rlatendo=rlatendo-dyout
            njo=njo-1
          endif

        endif

        if(diag_out) then
          write(iunit_diag,*) ' '
          write(iunit_diag,*) 'iout_grid_type=1'
          write(iunit_diag,*) 
     $         'i= ',rlonbegi,rlonendi,rlatbegi,rlatendi
          write(iunit_diag,*) 
     $         'o= ',rlonbego,rlonendo,rlatbego,rlatendo
          write(iunit_diag,*) 'nio,njo = ',nio,njo
        endif

C         
C         type 2 - gaussian in latitude
C
      else if(iout_grid_type.eq.2) then

        rlonbego=ifix(rlonbegi/dxout)*dxout
        rlonendo=ifix(rlonendi/dxout)*dxout
        if(cyclicxi) rlonendo=rlonbego+(360.0-dxout)
        nio=nint((rlonendo-rlonbego)/dxout)+1
C         
C         limits of the input grid (handles gaussian --> gaussian)
C         
        xbegi=yin(1)
        xendi=yin(njip1)

        jj=0
        do j=1,njog
          if(dumx(j).ge.xbegi.and.dumx(j).le.xendi) then
            jj=jj+1
            yout(jj)=dumx(j)
          endif
        end do

        njo=jj
        rlatbego=yout(1)
        rlatendo=yout(njo)

        if(diag_out) then
          write(iunit_diag,*) ' '
          write(iunit_diag,*) 'iout_grid_type=2'
          write(iunit_diag,*) 'dxout = ',dxout
          write(iunit_diag,*)
     $         'i= ',rlonbegi,rlonendi,rlatbegi,rlatendi
          write(iunit_diag,*) 
     $         'o= ',rlonbego,rlonendo,rlatbego,rlatendo
          write(iunit_diag,*) 'nio, njo = ',nio,njo
        endif
 
      else

C         
C         invalid grid type; abort
C
        go to 834

      endif
C         
C         output grid characteristics to GrADS
C         

      if(iregrid_method.eq.1.and..not.vote) 
     $     regrid_method='box averaging                  '
      if(iregrid_method.eq.1.and.vote)
     $     regrid_method='box averaging with VOTING      '
      if(iregrid_method.eq.2.and..not.bessel)
     $     regrid_method='bilinear interpolation         '
      if(iregrid_method.eq.2.and.bessel)
     $     regrid_method='bessel interpolation           '

      if(iout_grid_type.eq.1) then
        write(*,'(a)')  ' '
        write(*,'(a)')
     $       'the output grid is UNIFORM lat/lon:'
        write(*,'(a,f5.2,a,f5.2,a)')
     $       'dx = ',dxout,' deg and dy = ',dyout,' deg'
        write(*,'(a,i4,a,i4)')
     $       '# points in i(lon) = ',nio,
     $       '  # points j(lat) = ',njo
        write(*,'(a,f7.2,a,f7.2,a,f6.2,a,f6.2)') 
     $       'lon extent = ',rlonbego,' to ',rlonendo,
     $       ' lat extend = ',rlatbego,' to ',rlatendo
      endif

      if(iout_grid_type.eq.2) then
        write(*,'(a)')  ' '
        write(*,'(a,i4,a)')
     $       'the output grid is ~ T',nwaves,' GAUSSIAN:'
        write(*,'(a,f7.2,a,f5.2,a)')
     $       'dx = ',dxout,' deg and dy ~ ',dyoutg,' deg'
        write(*,'(a,i4,a,i4)')
     $       '# points in i(lon) = ',nio,'  # points j(lat) = ',njo
        write(*,'(a,f7.2,a,f7.2,a,f6.2,a,f6.2)') 
     $       'lon extent = ',rlonbego,' to ',rlonendo,
     $       '   lat extend = ',rlatbego,' to ',rlatendo
      endif
      write(*,'(a,a)') 
     $     'regrid method is: ',regrid_method
      if(vote) then
        write(*,'(a,f4.2,a,f4.2)')
     $       'vote parameters:  max fract area = ',rmin_vote_max,
     $       '  min frac area = ',rmin_vote_min
      endif
      write(*,'(a)')  ' '

C
C         boundaries of the output boxes
C         
      niop1=nio+1
      njop1=njo+1

C
C         X
C
      do i=1,niop1
        xout(i)=rlonbego+(i-1)*dxout-0.5*dxout
      end do
C         
C         y
C         
C         make sure the output grid does not extend beyond the poles
C         
      if(iout_grid_type.eq.1) then

        do j=1,njop1
          yout(j)=rlatbego+(j-1)*dyout-0.5*dyout
          if(yout(j).lt.-90.0) yout(j)=-90.0
          if(yout(j).gt.90.0) yout(j)=90.0
        end do
C         
C         check for pole points
C         
        if(rlatendo.eq.90.0) npole_point=.true.
        if(rlatbego.eq.-90.0) spole_point=.true.

      else if(iout_grid_type.eq.2) then

        dumx(1)=yout(1)-0.5*(yout(2)-yout(1))
        dumx(njop1)=yout(njo)+0.5*(yout(njo)-yout(njo-1))
        do j=2,njo
          dumx(j)=0.5*(yout(j-1)+yout(j))
        end do

        do j=1,njop1
          yout(j)=dumx(j)
        end do

      endif

C         
C------------------------------------------------------------
C
C         input-output grid box relationship 
C
C------------------------------------------------------------
C         
C         make sure longitudes of the input/output grids
C         are in positive deg
C         
      if(xin(1).lt.0.0) then
        do i=1,niip1
          xin(i)=xin(i)+360.0
        enddo
      endif
         
      if(xout(1).lt.0.0) then
        do i=1,niop1
          xout(i)=xout(i)+360.0
        enddo
      endif
C         
C         calculate the location of the output grid box boundaries 
C         w.r.t. the input grid box boundaries
C
      call in_out_boundaries(xin,yin,nii,nji,
     $     xout,yout,nio,njo,cyclicxi,
     $     niip1,njip1,niop1,njop1,
     $     iunit_diag,
     $     gxout,gyout)
      
      write(iunit_diag,*) 'output grid x boundaries'
      do i=1,niop1
        write(iunit_diag,*) ' i = ',i,' xout = ',xout(i),
     $       ' gxout = ',gxout(i)
      end do

      write(iunit_diag,*) 'output grid y boundaries'
      do j=1,njop1
        write(iunit_diag,*) ' j = ',j,' yout = ',yout(j),
     $       ' gyout = ',gyout(j)
      end do
C         
C         calculate sfc area of each grid box of input grid
C         
      call sfc_area(fld_in,xin,yin,undef,nii,nji,
     $     area_in,iunit_diag)

      allocate(  fld_out(nio,njo) )
      allocate( area_out(nio,njo) )
      call sfc_area(fld_out,xout,yout,undef,nio,njo,
     $     area_out,iunit_diag)

cc      qtitle='grid box area on unit s'
cc      call qprntn(area_in,qtitle,1,1,nii,nji,4,iunit_diag)
C         
C------------------------------------------------------------
C         
C         do the regrid
C
C------------------------------------------------------------
C
C         box averaging or "clumping" with a "voting" option
C         where the output grid equals the value of the
C         input grid which accounts for the most area.  voting
C         is used for discontinuos data such as soil type
C

      if(iregrid_method.eq.1) then

        call box_ave(fld_in,area_in,area_out,area_min,
     $       undef,gxout,gyout,
     $       nii,nji,nio,njo,fld_out,iunit_diag,vote,istat,
     $       rmin_vote_max,rmin_vote_min)
C         
C         FNOC bilinear/bessel interpolation
C         
      else if(iregrid_method.eq.2) then

        call bssl_interp(fld_in,undef,gxout,gyout,
     $       nii,nji,nio,njo,fld_out,iunit_diag,
     $       cyclicxi,spole_point,npole_point,bessel,istat)

      endif
C         
C         check for pole points
C         
      if(spole_point.or.npole_point) then
        call fix_poles(fld_out,nio,njo,undef,
     $       spole_point,npole_point)
      endif

      qtitle='fld_out                '
      call qprntu(fld_out,qtitle,undef,1,1,nio,njo,4,iunit_diag)
C
C------------------------------------------------------------
C
C         write out return info for GrADS
C
C------------------------------------------------------------
C
      ovals(1) = 0.0
      write(iunit_out) ovals
C         
C         modify the appropriate input grid parameters for 
C         defining the output grid for GrADS
C

      vals(4)=float(nio)
      vals(5)=float(njo)

      vals(6)=float(ilinearo)
      vals(7)=float(jlinearo)

      vals(8)=rlonbego
      vals(9)=dxout
      if(iout_grid_type.eq.1) then
        vals(10)=rlatbego
        vals(11)=dyout
      else if(iout_grid_type.eq.2) then
        vals(10)=0.0
        vals(11)=0.0
      endif

C         

      write(iunit_out) vals
C         
C         write out the grid to the return file
C
      call write_grid(iunit_out,fld_out,nio,njo)
C         
C         write out the grid to a back door dump file
C         
      call grdump(iunit_dat,fld_out,nio,njo)
C         
C         define the lat/lon of the output grid points
C         
      if(ilinearo.eq.0) then
        do i=1,nio
          xout(i)=rlonbego+(i-1)*dxout
        end do
        write(iunit_out) (xout(i),i=1,nio)
      endif

      if(iout_grid_type.eq.1) then

        do j=1,njo
          dumx(j)=(yout(j)+yout(j+1))*0.5
          yout(j)=rlatbego+(j-1)*dyout
        end do

      else if(iout_grid_type.eq.2) then

        do j=1,njo
          dumx(j)=(yout(j)+yout(j+1))*0.5
        end do
        do j=1,njo
          yout(j)=dumx(j)
        end do

      endif

      write(iunit_out) (yout(j),j=1,njo)
c
      go to 999
C         
C         error conditions 
C
 810  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 810'
      write(*,*) 'unable to open the GrADS input file to regrid'
      write(*,*) ' '
      go to 999

 812  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 812'
      write(*,'(a,a,a)') 
     $     'invalid character option in arg #4 = (',
     $     carg(nca)(1:2),') try again'
      write(*,*) ' '
      go to 999

 814  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 814'
      write(*,'(a,i1,a)') 
     $     'too many options in character arg #4 = (',
     $     nca,') try again'
      write(*,*) ' '
      go to 999

 820  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 820'
      write(*,*) 'the first arguement to regrid is not a grid'
      write(*,*) ' '
      go to 999
      
 830  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 830'
      write(*,*) 'the grid must be 2-d and x-y (lon-lat)' 
      write(*,*) ' '
      go to 999

 832  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 832'
      write(*,*) 'arguments must be numbers' 
      write(*,*) ' '
      go to 999

 834  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 834'
      write(*,*) 'invalid output grid iout_grid_type  = ',
     $     iout_grid_type 
      write(*,*) ' '
      go to 999

 836  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 836'
      write(*,*) 'invalid interpolation option',
     $     ' iregrid_type = ',iregrid_type 
      write(*,*) ' '
      go to 999

 838  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 838'
      write(*,*) '4 or more special case arguments, not a special case'
      write(*,*) ' '
      go to 999

 839  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 839'
      write(*,*) 'too few or wrong number of options for special cases'
      write(*,*) ' '
      go to 999

 840  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 840'
      write(*,*) 'rmin_vote outside the interval [1,0]'
      write(*,*) ' '
      go to 999

 842  continue
      write(*,*) ' '
      write(*,*) 'regrid ERROR 842'
      write(*,*) 'dlon/dlat greater than zero when attempting'
      write(*,*) 'to set beginning lat/lon of the output grid'
      write(*,*) ' '
      go to 999


 999  continue

      close(iunit_in)
      close(iunit_out)
      close(iunit_diag)
      close(iunit_dat)

      deallocate( dum1   )
      deallocate( fld_in )
      deallocate( fld_out)
      deallocate(area_in )
      deallocate(area_out)

      stop
      end

      subroutine grdump(iunit,a,ni,nj)
      dimension a(ni,nj)
      open (iunit,file='udf.regrid.dat',status='unknown',
     $     access='direct',recl=ni*nj*4,form='unformatted')
      write(iunit,rec=1) a
      close (iunit)
      return
      end

      subroutine box_ave(fld_in,area_in,area_out,area_min,
     $     undef,gxout,gyout,
     $     nii,nji,nio,njo,fld_out,iunit_diag,vote,istat,
     $     rmin_vote_max,rmin_vote_min)

      parameter (na=300)
      dimension fld_in(nii,nji),area_in(nii,nji),
     $     fld_out(nio,njo),area_out(nio,njo),
     $     gxout(nio+1),gyout(njo+1)

      dimension area_box(na),fld_box(na),dxdy_box(na),ifld_rank(na),
     $     fld_cand(na),area_cand(na),dxdy_cand(na),rmin_dxdy_vote(3)

      logical cyclicx,diag_out,vote
      
      diag_out=.false.
ccc      diag_out=.true.
      istat=1
C         
C         tolerance for checking whether grid is undefined (zero sfc area)
C
      eps=1.0e-12
C         
C         minimum fractional area of the output grid box
C         in order to have a winner in the voting
C
      rmin_dxdy_vote(1)=rmin_vote_max
      rmin_dxdy_vote(2)=(rmin_vote_max+rmin_vote_min)*0.5
      rmin_dxdy_vote(3)=rmin_vote_min

      ibb=1
      iee=nio
      jbb=1
      jee=njo

      do j=jbb,jee
        do i=ibb,iee
          
          ib=int(gxout(i))+1
          ie=int(gxout(i+1))+1
          jb=int(gyout(j))+1
          je=int(gyout(j+1))+1
C         
C         check for exceeding n pole
C         
          if(je.gt.nji) je=nji

C         
C         cyclic continuity in x
C         
          cyclicx=.false.
          if(ie.lt.ib) then
            ie=ie+nii
            cyclicx=.true.
          endif
          
          if(diag_out) then
            write(iunit_diag,*) ' '
            write(iunit_diag,*)
     $           'i,j,ib,ie,jb,je = ',i,j,ib,ie,jb,je
          endif
C         
C         initialize the counter for intersecting grid boxes
C         
          icnt=0
C         
C         CASE 1:  only one input grid box in output grid box
C         
          if(ib.eq.ie.and.jb.eq.je) then
            
            icnt=1
            dxdy_box(icnt)=1.0
            area_box(icnt)=area_in(ib,jb)
            if(area_box(icnt).eq.0.0) dxdy_box(icnt)=0.0
            fld_box(icnt)=fld_in(ib,jb)
            
            if(diag_out) then
              write(iunit_diag,*)
     $             'ib=ie and jb=je ',area_box(icnt),dxdy_box(icnt),
     $             fld_box(icnt)
            endif
            
          else if(ib.eq.ie) then
C         
C         CASE 2:  intersecting boxes in y only
C         
            ii=ib
            dx=gxout(i+1)-gxout(i)
            dxout=1.0

            do jj=jb,je
              
              icnt=icnt+1
              
              if(icnt.gt.na) then
                istat=0
                go to 999
              endif
              
              if(jj.eq.jb) then
                dy=float(jj)-gyout(j)
              else if(jj.eq.je) then
                dy=gyout(j+1)-float(jj-1)
              else
                dy=1.0
              endif
              
              if(jj.eq.jb.or.jj.eq.je) then
                dyout=dy/(gyout(j+1)-gyout(j))
              else
                dyout=1.0
              endif

              dxdy_box(icnt)=dxout*dyout
              area_box(icnt)=dx*dy*area_in(ii,jj)
              if(area_in(ii,jj).eq.0.0) dxdy_box(icnt)=0.0
              fld_box(icnt)=fld_in(ii,jj)
              
              if(diag_out) then
                write(iunit_diag,*)
     $               'ib=ie ',ii,jj,dx,dy,area_in(ii,jj)
                write(iunit_diag,*)
     $               'area_box ... ',area_box(icnt),fld_box(icnt)
                write(iunit_diag,*)
     $               'dx dyout ... ',dxout,dyout,dxdy_box(icnt)
              endif
              
            end do
            
          else if(jb.eq.je) then
C         
C         CASE 3:  intersecting boxes in x only
C         
            jj=jb
            dy=gyout(j+1)-gyout(j)
            dyout=1.0

            do ii=ib,ie

              icnt=icnt+1
              if(icnt.gt.na) then
                istat=0
                go to 999
              endif

              ii0=ii
              if(cyclicx.and.ii0.gt.nii) ii0=ii-nii
              if(ii.eq.ib) then
                dx=float(ii)-gxout(i)
              else if(ii.eq.ie) then
                x0=float(ii-1)
                if(cyclicx) x0=float(ii0)-1.0
                dx=gxout(i+1)-x0
              else
                dx=1.0
              endif

              if(ii.eq.ib.or.ii.eq.ie) then
                dxout=dx/(gxout(i+1)-gxout(i))
              else
                dxout=1.0
              endif

              dxdy_box(icnt)=dxout*dyout

              area_box(icnt)=dx*dy*area_in(ii0,jj)
              if(area_in(ii0,jj).eq.0.0) dxdy_box(icnt)=0.0
              fld_box(icnt)=fld_in(ii0,jj)
              
              if(diag_out) then
                write(iunit_diag,*)
     $               'jb=je ',ii,ii0,jj,dx,dy,area_in(ii0,jj)
                write(iunit_diag,*)
     $               'area_box ... ',area_box(icnt),fld_box(icnt)
                write(iunit_diag,*)
     $               'dx dyout ... ',dxout,dyout,dxdy_box(icnt)
              endif
              
            end do
            
          else
C         
C         CASE 4:  intersecting boxes in both directions
C         
            do jj=jb,je

              if(jj.eq.jb) then
                dy=float(jj)-gyout(j)
              else if(jj.eq.je) then
                dy=gyout(j+1)-float(jj-1)
              else
                dy=1.0
              endif

              if(jj.eq.jb.or.jj.eq.je) then
                dyout=dy/(gyout(j+1)-gyout(j))
              else
                dyout=1.0
              endif

              do ii=ib,ie
                icnt=icnt+1
                if(icnt.gt.na) then
                  istat=0
                  go to 999
                endif
                
                ii0=ii
                if(cyclicx.and.ii0.gt.nii) ii0=ii-nii
                if(ii.eq.ib) then
                  dx=float(ii)-gxout(i)
                else if(ii.eq.ie) then
                  x0=float(ii-1)
                  if(cyclicx) x0=float(ii0)-1.0
                  dx=gxout(i+1)-x0
                else
                  dx=1.0
                endif

                if(ii.eq.ib.or.ii.eq.ie) then
                  dxout=dx/(gxout(i+1)-gxout(i))
                else
                  dxout=1.0
                endif

                dxdy_box(icnt)=dxout*dyout
                area_box(icnt)=dx*dy*area_in(ii0,jj)
                if(area_in(ii0,jj).eq.0.0) dxdy_box(icnt)=0.0
                fld_box(icnt)=fld_in(ii0,jj)
                if(diag_out) then
                  write(iunit_diag,*) 'ib.ne.ib.and.jb.ne.je ',
     $                 ii,ii0,jj,dx,dy,area_in(ii0,jj)
                  write(iunit_diag,*)
     $                 'area_box ... ',area_box(icnt),fld_box(icnt)
                  write(iunit_diag,*)
     $                 'dx dyout ... ',dxout,dyout,dxdy_box(icnt)
                endif
                
              end do
              
            enddo
            
          endif
C         
C         integrate or vote for the average value
C         
          if(vote) then
C         
C         voting routine; first get total area
C         
            
            tot_area=0.0
            do ii=1,icnt
              tot_area=tot_area+area_box(ii)
            end do
            
            if(diag_out) then
              write(iunit_diag,*) 'i,j,icnt,tot_area = ',
     $             i,j,icnt,tot_area
            endif
            
            
            if(tot_area.le.eps) then
              fld_out(i,j)=undef
              go to 100
            endif
            
            if(icnt.eq.1) then
C         
C         USSR election -- only one "candidate" to vote for
C         
C         check if the the total area is greater
C         than the minimum required to hold the election (e.g., 0.5)
C         
              if(diag_out) then
                write(iunit_diag,*)
     $               'USSR:  ',dxdy_box(1),rmin_dxdy_vote(1),fld_box(1)
              endif

              if(dxdy_box(1).lt.rmin_dxdy_vote(1)) then
                fld_out(i,j)=undef
              else
                fld_out(i,j)=fld_box(1)
              endif
              
              go to 100
              
            else if(icnt.eq.2) then
C         
C         USA election -- two-party, two-candidate race; area wins
C         
              if(diag_out) then
                write(iunit_diag,*)'USA: ',dxdy_box(1),dxdy_box(2),
     $               rmin_vote
              endif

              if(dxdy_box(1).eq.0.0.or.dxdy_box(2).eq.0.0) then
                rmin_vote=rmin_dxdy_vote(1)
              else if(fld_box(1).eq.fld_box(2)) then
                rmin_vote=rmin_dxdy_vote(1)
              else
                rmin_vote=rmin_dxdy_vote(2)
              endif

              if((dxdy_box(1).ge.dxdy_box(2)).and.
     $             (dxdy_box(1).gt.rmin_vote)) then
                fld_out(i,j)=fld_box(1)
              else if(dxdy_box(2).gt.rmin_vote) then
                fld_out(i,j)=fld_box(2)
C         
C         case where both candidates are the same in the two-person race
C
              else if(fld_box(1).eq.fld_box(2).and.
     $               ((dxdy_box(1)+dxdy_box(2)).gt.rmin_vote)) then
                fld_out(i,j)=fld_box(2)
              else
                fld_out(i,j)=undef
              endif
              
            else
C         
C         a wide open election - three or more candidates
C         
C         sort the data by surface area using 
C         the numercial recipes routine --  indexx
C         
              call indexx(icnt,fld_box,ifld_rank)
              
              if(diag_out) then
                write(iunit_diag,*) 
     $               'fld_box = ',(fld_box(ipp),ipp=1,icnt)
                write(iunit_diag,*) 
     $               'area_box = ',(area_box(ipp),ipp=1,icnt)
                write(iunit_diag,*) 
     $               'ifld_rank = ',(ifld_rank(ipp),ipp=1,icnt)
                write(iunit_diag,*)
              endif
C         
C         the indexes are in reverse order, with the
C         biggest fld element in the last element of the array
C         first check if the biggest is in the majority
C         
C         set up the candidates
C         
              ncand=1
              it1=ifld_rank(1)
              
              area_cand(ncand)=area_box(it1)
              fld_cand(ncand)=fld_box(it1)
              dxdy_cand(ncand)=dxdy_box(it1)
              
              do ii=2,icnt
                
                i1=ii-1
                i2=ii
                it1=ifld_rank(i1)
                it2=ifld_rank(i2)
                
                if(fld_box(it1).eq.fld_box(it2)) then
                  area_cand(ncand)=area_cand(ncand)+area_box(it2)
                  dxdy_cand(ncand)=dxdy_cand(ncand)+dxdy_box(it2)
                else
                  ncand=ncand+1
                  area_cand(ncand)=area_box(it2)
                  dxdy_cand(ncand)=dxdy_box(it2)
                  fld_cand(ncand)=fld_box(it2)
                endif
                
              end do
              if(diag_out) then
                write(iunit_diag,*) 'ncand = ',ncand
                write(iunit_diag,*) 
     $               'fld_cand = ',(fld_cand(ipp),ipp=1,ncand)
                write(iunit_diag,*) 
     $               'area_cand = ',(area_cand(ipp),ipp=1,ncand)
                write(iunit_diag,*) 
     $               'dxdy_cand = ',(dxdy_cand(ipp),ipp=1,ncand)
              endif
C         
C         if one candidate, all done
C         
              if(ncand.eq.1) then
                if(dxdy_cand(1).gt.rmin_dxdy_vote(3)) then
                  fld_out(i,j)=fld_cand(1)
                else
                  fld_out(i,j)=undef
                endif
                go to 100
              else
                
C         
C         the candidate with the most area is the winner
C         
                area_max=0.0
                do ii=1,ncand
                  if(area_cand(ii).gt.area_max) then
                    iamax=ii
                    area_max=area_cand(ii)
                  endif
                end do
                
                if(ncand.le.2) then
                  rmin_vote=rmin_dxdy_vote(ncand)
                else
                  rmin_vote=rmin_dxdy_vote(3)
                endif

                if(dxdy_cand(iamax).gt.rmin_vote) then
                  fld_out(i,j)=fld_cand(iamax)
                else
                  fld_out(i,j)=undef
                endif
                
                if(diag_out) then
                  write(iunit_diag,*) ' '
                  write(iunit_diag,*) 'the winner is...',
     $                 iamax,area_max,dxdy_cand(iamax),
     $                 rmin_vote,fld_out(i,j)
                endif
                
                go to 100
                
              endif
              
            endif
            
            
          else
C         
C         area integrate
C         
            tot_fld=0.0
            tot_area=0.0
C         
            do ii=1,icnt
              tot_fld=tot_fld+fld_box(ii)*area_box(ii)
              tot_area=tot_area+area_box(ii)
            end do
            
            if(tot_area.gt.area_out(i,j)*area_min) then
              fld_out(i,j)=tot_fld/tot_area
            else
              fld_out(i,j)=undef
            endif
            
            if(diag_out) then
              write(iunit_diag,*) 'qqq ',tot_area,' ',
     $             area_out(i,j)*area_min,' ',
     $             area_out(i,j),' ',fld_out(i,j)
            endif

            
          endif
          
 100      continue
          
          if(diag_out) then
            write(iunit_diag,*)
     $           'i,j fld_out(i,j) = ',i,j,fld_out(i,j)  
          endif
          
        end do
      end do
      
 999  continue
      
      return
      end

      subroutine bssl_interp(fld_in,undef,gxout,gyout,
     $     nii,nji,nio,njo,fld_out,iunit_diag,
     $     cyclicxi,spole_point,npole_point,bessel,istat)

      dimension fld_in(nii,nji),fld_out(nio,njo),
     $     gxout(nio+1),gyout(njo+1)

      dimension fr(4)

      logical cyclicxi,diag_out,bessel,spole_point,npole_point
      
      diag_out=.false.
ccc      diag_out=.true.
      jchk=2
      istat=1
C         
C         convert the box boundaries to grid point center
C         
      do i=1,nio
C         
C         check for crossing the boundaries 
C         the only way for this to occur is if the field is 
C         cyclically continuous in x
C

        if(gxout(i+1).lt.gxout(i)) then
          gxout(i)=((gxout(i)-float(nii))+gxout(i+1))*0.5+0.5
          if(gxout(i).lt.1.0) gxout(i)=float(nii)+gxout(i)
        else
          gxout(i)=(gxout(i)+gxout(i+1))*0.5+0.5
          if(gxout(i).lt.1.0) gxout(i)=float(nii)+gxout(i)
        endif
      end do

      do j=1,njo
        gyout(j)=(gyout(j)+gyout(j+1))*0.5+0.5
C         
C         check if a pole points on the input grid
C         
        if(spole_point.and.gyout(j).lt.1.0) gyout(j)=1.0
        if(npole_point.and.gyout(j).gt.nji+0.5) gyout(j)=nji

      end do

      ibb=1
      iee=nio
      jbb=1
      jee=njo

      niim1=nii-1
      njim1=nji-1
      
      do j=jbb,jee
        do i=ibb,iee
          
          ic=ifix(gxout(i))
          jc=ifix(gyout(j))
          icp1=ic+1
          if(cyclicxi.and.icp1.gt.nii) icp1=icp1-nii 
          jcp1=jc+1
          
          if(diag_out.and.j.eq.jchk) write(iunit_diag,*)
     $         'i,j,gxout,gyout,ic,jc',
     $         i,j,gxout(i),gyout(j),ic,jc
          
          if((jc.lt.1.or.jc.gt.nji).or.
     $         (.not.cyclicxi.and.(ic.lt.1.or.ic.gt.nii))) then
            fld_out(i,j)=undef
            if(diag_out.and.j.eq.jchk) 
     $           write(iunit_diag,*) 'out of bounds ' 
            go to 100
          end if
C         
C------------------------------------------------
C         
C         bilinear/bessel interpolation based on the FNOC routine
C         bssl5 by D. Hensen, FNOC
C         
C------------------------------------------------
C         
          r=gxout(i)-ic
          s=gyout(j)-jc
C         
C         interior check
C         
          if((jc.ge.2.and.jc.lt.njim1.and.cyclicxi).or.
     $         (ic.ge.2.and.jc.ge.2.and.
     $         ic.lt.niim1.and.jc.lt.njim1)) go to 10
C         
C         border zone check
C         
          if((jc.lt.nji.and.cyclicxi).or.
     $         (ic.lt.nii.and.jc.lt.nji)) go to 5 
C         
C------------------------------------------------
C         
C         top and right edge processing
C         
C------------------------------------------------
C         
          if(ic.eq.nii.and.jc.eq.nji) then
            
            fld_out(i,j)=fld_in(nii,nji)
            if(diag_out.and.j.eq.jchk) 
     $           write(iunit_diag,*) 'upper right hand corenr'
            
          else if(ic.eq.nii) then
            
            if(diag_out.and.j.eq.jchk) 
     $           write(iunit_diag,*) 'right edge'
            
            if(fld_in(ic,jc).ne.undef.and.
     $           fld_in(ic,jcp1).ne.undef) then
              fld_out(i,j)=(1.0-s)*fld_in(ic,jc)+
     $             s*fld_in(ic,jcp1)
            else
              fld_out(i,j)=undef
            endif
            
          else if(jc.eq.nji) then
            
            if(diag_out.and.j.eq.jchk) 
     $           write(iunit_diag,*) 'top edge'
            
            if(fld_in(ic,jc).ne.undef.and.
     $           fld_in(icp1,jc).ne.undef) then
              fld_out(i,j)=(1.0-r)*fld_in(ic,jc)+
     $             r*fld_in(icp1,jc)
            else
              fld_out(i,j)=undef
            endif
            
          endif
          
          go to 100
          
 5        continue
C         
C------------------------------------------------
C         
C         border zone; bilinear
C         
C------------------------------------------------
C         
          iok_bilinear=1
          if(fld_in(ic,jc).eq.undef.or.
     $         fld_in(icp1,jc).eq.undef.or.
     $         fld_in(ic,jcp1).eq.undef.or.
     $         fld_in(icp1,jcp1).eq.undef) iok_bilinear=0
          
          if(diag_out.and.j.eq.jchk) 
     $         write(iunit_diag,*) 
     $         'border zone, iok_bilinear= ',iok_bilinear
                    
          if(iok_bilinear.eq.0) then
            fld_out(i,j)=undef
          else
            fld_out(i,j)=(1.-s)*((1.-r)*fld_in(ic,jc)
     $           +r*fld_in(icp1,jc))
     $           +s*((1.-r)*fld_in(ic,jcp1)
     $           +r*fld_in(icp1,jcp1))
          endif

          go to 100
 
 10       continue
C         
C------------------------------------------------
C         
C         interior zone
C         
C------------------------------------------------
C         
C         first check if bilinear is OK
C         
          iok_bilinear=1
          if(fld_in(ic,jc).eq.undef.or.
     $         fld_in(icp1,jc).eq.undef.or.
     $         fld_in(ic,jcp1).eq.undef.or.
     $         fld_in(icp1,jcp1).eq.undef) iok_bilinear=0
          
          if(diag_out.and.j.eq.jchk) 
     $         write(iunit_diag,*) 
     $         'interior zone, iok_bilinear= ',iok_bilinear
          
          if(iok_bilinear.eq.0) then
            fld_out(i,j)=undef
            go to 100
          else 
C         
C         bilinear value is the first guess
C         
            fld_out(i,j)=(1.-s)*((1.-r)*fld_in(ic,jc)
     $           +r*fld_in(icp1,jc))
     $           +s*((1.-r)*fld_in(ic,jcp1)
     $           +r*fld_in(icp1,jcp1))
C         
C         exit if only doing bilinear
C         
            if(.not.bessel) go to 100
            
          endif
          
          
C         
C         interpolate 4 columns (i-1,i,i+1,i+2) to j+s and store in fr(1)
C         through fr(4)
C         
          r1=r-0.5
          r2=r*(r-1.)*0.5
          r3=r1*r2*0.3333333333334
          s1=s-0.5
          s2=s*(s-1.)*0.5
          s3=s1*s2*0.3333333333334
C         
          k=0
          im1=ic-1
          ip2=ic+2
          
          if(diag_out.and.j.eq.jck) write(iunit_diag,*) 'bessel interp'
          
C         
          do ii=im1,ip2
            
            k=k+1
            
            i1=ii
            
            if(cyclicxi.and.i1.lt.1) i1=nii-i1
            if(cyclicxi.and.i1.gt.nii) i1=i1-nii
            
            j1=jc
            
            j1p1=j1+1
            j1p2=j1+2
            j1m1=j1-1
            
            fij=fld_in(i1,j1)
            fijp1=fld_in(i1,j1p1)
            fijp2=fld_in(i1,j1p2)
            fijm1=fld_in(i1,j1m1)
            if(diag_out.and.j.eq.jchk) 
     $           write(iunit_diag,*) 'i1,j1 = ',i1,j1,
     $           fij,fijp1,fijp2,fijm1
C         
C         exit if any value undefined
C         
            if(fij.eq.undef.or.
     $           fijp1.eq.undef.or.
     $           fijp2.eq.undef.or.
     $           fijm1.eq.undef) go to 100 
            
            u=(fij+fijp1)*0.5
            del=fijp1-fij
            udel2=(fijp2-fijp1+fijm1-fij)*0.5
            del3=fijp2-fijp1-2.*del+fij-fijm1
            
            fr(k)=u+s1*del+s2*udel2+s3*del3
            
          end do
          
C         
C         interpolate the fr row to ii+r
C         
          u=(fr(2)+fr(3))*0.5
          del=fr(3)-fr(2)
          udel2=(fr(4)-fr(3)+fr(1)-fr(2))*0.5
          del3=fr(4)-fr(3)-2.*del+fr(2)-fr(1)
          
          fld_out(i,j)=u+r1*del+r2*udel2+r3*del3
          
          
 100      continue
          if(diag_out.and.j.eq.jchk) 
     $         write(iunit_diag,*) 'interp value = ',fld_out(i,j)
          
        end do
      end do
      
 999  continue
      
      return
      end
      
      subroutine indexx(n,arrin,indx)
      dimension arrin(n),indx(n)
      do 11 j=1,n
        indx(j)=j
11    continue
      l=n/2+1
      ir=n
10    continue
        if(l.gt.1)then
          l=l-1
          indxt=indx(l)
          q=arrin(indxt)
        else
          indxt=indx(ir)
          q=arrin(indxt)
          indx(ir)=indx(1)
          ir=ir-1
          if(ir.eq.1)then
            indx(1)=indxt
            return
          endif
        endif
        i=l
        j=l+l
20      if(j.le.ir)then
          if(j.lt.ir)then
            if(arrin(indx(j)).lt.arrin(indx(j+1)))j=j+1
          endif
          if(q.lt.arrin(indx(j)))then
            indx(i)=indx(j)
            i=j
            j=j+j
          else
            j=ir+1
          endif
        go to 20
        endif
        indx(i)=indxt
      go to 10
      end

      subroutine in_out_boundaries(xin,yin,nii,nji,
     $     xout,yout,nio,njo,cyclicxi,
     $     niip1,njip1,niop1,njop1,
     $     iunit_diag,
     $     gxout,gyout)
C         
C  Purpose:
C
C         calculate the location of grid box boundaries of
C         an "output" grid w.r.t. an "input" grid
C         
C         used in a 2-D regriding process, i.e.,
C         input --> output
C         
C  Input variables:
C         
C         xin - longitudes of the input grid
C         yin - latitudes of the input grid
C         xout - longtitudes of the output grid
C         yout - latitudes of the output grid
C         
C         cyclicxi - flag whether the input grid is cyclically continuous
C         in x
C         
C         nii - size of the x dimension of the input grid
C         nji - size of the y dimension of the input grid
C         nio - size of the x dimension of the output grid
C         njo - size of the y dimension of the output grid
C
C         niip1= nii+1, etc.
C
C         iunit_diag - unit number to write diagnositics
C
C  Output variables:         
C
C         gxout - x location of the output grid in input grid units
C         gyout - y location of the output grid in input grid units
C         

      dimension xin(*),yin(*),xout(*),yout(*),
     $     gxout(*),gyout(*)

      logical cyclicxi

C         
C         locate the output grid w.r.t. input grid in x
C         ALLOW FOR CYCLIC CONTINUITY IN X!!
C         
      ibeg=1
      iend=niip1
      
      do i=1,niop1
        
        x0=xout(i)
        
        do ii=ibeg,iend
C         
C         point is before start of input grid 
C         
          x1=xin(ii)
          x2=xin(niop1)

          if(x0.lt.x1) then
            if(cyclicxi) then
              ic=nii
              do while(x0.lt.x1)
                x2=x1
                x1=xin(ic)-360.0
                ic=ic-1
              enddo
              dx0=(x0-x1)/(x2-x1)
              gxout(i)=float(ic)+dx0
            else
              gxout(i)=0.0
            endif
            
            ibeg=1
            go to 50
            
C         
C         point is within the input grid
C         
          else if(ii.ge.1.and.ii.le.nii) then
            
            x2=xin(ii+1)
            
            write(iunit_diag,*) ibeg,i,ii,x0,x1,x2
            
            if(x0.ge.x1.and.x0.lt.x2) then
              dx0=(x0-x1)/(x2-x1)
              gxout(i)=float(ii-1)+dx0
              ibeg=ii
              go to 50
            endif
           
C         
C         point is beyond input grid
C         
          else if(x0.ge.x2) then
            ic=2
            if(cyclicxi) then
              x1=xin(niip1)
              x2=xin(ic)+360.0

              write(iunit_diag,*) 'after',i,ic,x0,x1,x2
              do while(x0.lt.x1.or.x0.ge.x2)
                ic=ic+1
                x1=x2
                x2=xin(ic)+360.0
              end do
              dx0=(x0-x1)/(x2-x1)
              gxout(i)=float(ic-2)+dx0
            else
              gxout(i)=float(nii)
            endif
            
            ibeg=niip1
            go to 50
            
          endif
          
        end do
        
 50     continue
        
      end do
C         
C         locate the output grid w.r.t. input grid in y
C         NO CYCLIC CONTINUITY!!
C         
      jbeg=1
      jend=njip1
      
      do j=1,njop1
        
        y0=yout(j)
        
        do jj=jbeg,jend
C         
C         point is before start of input grid 
C         
          y1=yin(jj)
          
          if(y0.lt.y1) then

            gyout(j)=0.0
            jbeg=1
cccc            write(iunit_diag,*) 'before y ',j,jj,gyout(j)
            go to 60
C         
C         point is within the input grid
C         
          else if(jj.ge.1.and.jj.le.nji) then
            
            y2=yin(jj+1)
            
cccc            write(iunit_diag,*) 'jbeg... ',jbeg,j,jj,y0,y1,y2
            
            if(y0.ge.y1.and.y0.lt.y2) then
              dy0=(y0-y1)/(y2-y1)
              gyout(j)=float(jj-1)+dy0
              jbeg=jj
              go to 60
            endif
C         
C         point is beyond input grid
C         
          else if(y0.ge.y2) then
            gyout(j)=float(nji)
            jbeg=njip1
cccc            write(iunit_diag,*) 'after y ',j,jj,gyout(j)
            go to 60
          endif
          
        end do
        
 60     continue
        
      end do

      return
      end

      subroutine avg_pole(a,m,n)
C         
Csss      routine to replace pole value with average 
Csss      at the penultimate point
C
      dimension a(m,n)
      ave_2=0.0
      ave_nm1=0.0
      do i=1,m
        ave_2=ave_2+a(i,2)
        ave_nm1=ave_nm1+a(i,n-1)
      end do
      ave_2=ave_2/m
      ave_nm1=ave_nm1/m
      do i=1,m
        a(i,1)=ave_2
        a(i,n)=ave_nm1
      end do
      return
      end

      subroutine bsslz1(bes,n)                                      
c                                                                   
c                                                                   
      implicit double precision (a-h,o-z)                                      
      dimension bes(n)                                              
      dimension bz(50)                                              
c                                                                   
      data pi/3.14159265358979d0/                                   
      data bz  / 2.4048255577d0, 5.5200781103d0,
     $  8.6537279129d0,11.7915344391d0,14.9309177086d0,18.0710639679d0, 
     $ 21.2116366299d0,24.3524715308d0,27.4934791320d0,30.6346064684d0,
     $ 33.7758202136d0,36.9170983537d0,40.0584257646d0,43.1997917132d0,
     $ 46.3411883717d0,49.4826098974d0,52.6240518411d0,55.7655107550d0,
     $ 58.9069839261d0,62.0484691902d0,65.1899648002d0,68.3314693299d0,
     $ 71.4729816036d0,74.6145006437d0,77.7560256304d0,80.8975558711d0,
     $ 84.0390907769d0,87.1806298436d0,90.3221726372d0,93.4637187819d0,
     $ 96.6052679510d0,99.7468198587d0,102.888374254d0,106.029930916d0,
     $ 109.171489649d0,112.313050280d0,115.454612653d0,118.596176630d0,
     $ 121.737742088d0,124.879308913d0,128.020877005d0,131.162446275d0,
     $ 134.304016638d0,137.445588020d0,140.587160352d0,143.728733573d0,
     $ 146.870307625d0,150.011882457d0,153.153458019d0,156.295034268d0/

      nn=n                                                          
      if(n.le.50) go to 12                                          
      bes(50)=bz(50)                                                
      do 5 j=51,n                                                   
    5 bes(j)=bes(j-1)+pi                                            
      nn=49                                                         
   12 do 15 j=1,nn                                                  
   15 bes(j)=bz(j)                                                  
      return                                                        
      end                                                           
      function esatw(t)
c
c     t is temperature of air in deg celcius.
c     ta is temperature in deg kelvin
c     p is pressure in mb
c     esatw is saturation vapor pressure in mb (over water)
c
      data ps/1013.246/,ts/373.16/
c
      ta = t
      if(t .lt. 100.0) then 
         ta = t + 273.16
      end if
      e1=11.344*(1.0-ta/ts)
      e2=-3.49149*(ts/ta-1.0)
      f1=-7.90298*(ts/ta-1.0)
      f2=5.02808*alog10(ts/ta)
      f3=-1.3816*(10.0**e1-1.0)*1.e-7
      f4=8.1328*(10.0**e2-1.0)*1.e-3
      f5=alog10(ps)
      f=f1+f2+f3+f4+f5
      esatw=10.0**f
      return
      end
c
      function esati(t)
c
c     t is temperature of air in deg celcius.
c     ta is temperature in deg kelvin
c     p is pressure in mb
c     esati is saturation vapor pressure with respect to ice in mb
c
      data p0/6.1071/,t0/273.16/
c
      t = ta
      if(t .lt. 100.0) then 
        ta = t + 273.16
      end if
      f1=-9.09718*(t0/ta-1.0)
      f2=-3.56654*alog10(t0/ta)
      f3=0.876793*(1.0-ta/t0)
      f4=alog10(p0)
      esati = 10.0**(f1+f2+f3+f4)
      return
      end


      subroutine fix_grid(a,ni,nj)
      dimension a(ni,nj)
      do i=1,ni
        do j=1,nj
          a(i,j)=i+j
        end do
      end do
      return
      end

      subroutine fix_poles(fld_out,nio,njo,undef,
     $     spole_point,npole_point)

      dimension fld_out(nio,njo)
      logical spole_point,npole_point

      rmeans=0.0
      rmeann=0.0
      icnts=0
      icntn=0

      do i=1,nio

        if(fld_out(i,1).ne.undef) then
          rmeans=rmeans+fld_out(i,1)
          icnts=icnts+1
        endif

        if(fld_out(i,njo).ne.undef) then
          rmeann=rmeann+fld_out(i,njo)
          icntn=icntn+1
        endif

      end do

      if(icnts.gt.0) then
        rmeans=rmeans/float(icnts)
      else
        rmeans=undef
      endif

      if(icntn.gt.0) then
        rmeann=rmeann/float(icntn)
      else
        rmeann=undef
      endif

      if(spole_point) then
        do i=1,nio
          fld_out(i,1)=rmeans
        end do
      end if

      if(npole_point) then
        do i=1,nio
          fld_out(i,njo)=rmeann
        end do
      end if

      return
      end

c
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    gaulat      calculates gaussian grid latitudes
c   prgmmr: s. j. lord       org: w/nmc22    date: 91-06-06
c
c abstract: calculates gaussian grid latitudes
c
c program history log:
c   91-06-06  s. j. lord - copied from kanamitsu library
c   930921 m.fiorino - changed from colatitude to latitude
c   yy-mm-dd  modifier1   description of change
c   yy-mm-dd  modifier2   description of change
c
c usage:    call pgm-name(inarg1, inarg2, wrkarg, outarg1, ... )
c   input argument list:
c     inarg1   - generic description, including content, units,
c     inarg2   - type.  explain function if control variable.
c
c   output argument list:      (including work arrays)
c     wrkarg   - generic description, etc., as above.
c     outarg1  - explain completely if error return
c     errflag  - even if many lines are needed
c
c   input files:   (delete if no input files in subprogram)
c     ddname1  - generic name & content
c
c   output files:  (delete if no output files in subprogram)
c     ddname2  - generic name & content as above
c     ft06f001 - include if any printout
c
c remarks: list caveats, other helpful hints or information
c
c attributes:
c   language: indicate extensions, compiler options
c   machine:  nas, cyber, whatever
c
c$$$
      subroutine gauss_lat_nmc(gaul,k)                                     
c                                                                   
      implicit double precision (a-h,o-z)
      dimension a(500)
      real gaul(1)                                                
c
      save
c                                                                   
      esp=1.d-14                                                    
      c=(1.d0-(2.d0/3.14159265358979d0)**2)*0.25d0                  
      fk=k                                                          
      kk=k/2                                                        
      call bsslz1(a,kk)                                             
      do 30 is=1,kk                                                 
      xz=cos(a(is)/sqrt((fk+0.5d0)**2+c))                           
      iter=0                                                        
   10 pkm2=1.d0                                                     
      pkm1=xz                                                       
      iter=iter+1                                                   
      if(iter.gt.10) go to 70                                       
      do 20 n=2,k                                                   
      fn=n                                                          
      pk=((2.d0*fn-1.d0)*xz*pkm1-(fn-1.d0)*pkm2)/fn                 
      pkm2=pkm1                                                     
   20 pkm1=pk                                                       
      pkm1=pkm2                                                     
      pkmrk=(fk*(pkm1-xz*pk))/(1.d0-xz**2)                          
      sp=pk/pkmrk                                                   
      xz=xz-sp                                                      
      avsp=abs(sp)                                                  
      if(avsp.gt.esp) go to 10                                      
      a(is)=xz                                                      
   30 continue                                                      
      if(k.eq.kk*2) go to 50                                        
      a(kk+1)=0.d0                                                  
      pk=2.d0/fk**2                                                 
      do 40 n=2,k,2                                                 
      fn=n                                                          
   40 pk=pk*fn**2/(fn-1.d0)**2                                      
   50 continue                                                      
      do 60 n=1,kk                                                  
      l=k+1-n                                                       
      a(l)=-a(n)                                                    
   60 continue                                                      
c                                                                   
      radi=180./(4.*atan(1.))                                       
      do 211 n=1,k                                                  
      gaul(n)=acos(a(n))*radi-90.0                                       
  211 continue                                                      
c     print *,'gaussian lat (deg) for jmax=',k                      
c     print *,(gaul(n),n=1,k)                                       
c                                                                   
      return                                                        
   70 write(6,6000)                                                 
 6000 format(//5x,14herror in gauaw//)
      stop
      end        
                                                   

      subroutine gauss_lat_pcmdi(gaul,gaulb,k)
      parameter (nmax=1000)
      double precision pa(nmax),pw(nmax),pi,pb(nmax),pt(nmax),r2d
      double precision gaul(k),gaulb(k)
C         
C         get the gaussian latitudes and integration weights
C
      call gauaw(pa,pw,k)
C         
C         reverse direction so j increase northward for output
C         
      do j=1,k
        jj=k-j+1
        pt(j)=pa(jj)
      end do
      do j=1,k
        pa(j)=pt(j)
      end do

      do j=1,k
        jj=k-j+1
        pt(j)=pw(jj)
      end do
      do j=1,k
        pw(j)=pt(j)
      end do

      pi = 4.*atan(1.d0)
      r2d = 180.0/pi
C         
C         integrate to get the latitude boundaries of the gauss grid boxes
C
      pb(1)=-pi*0.5
      do j=1,k
        pb(j+1) = asin( pw(j) + sin( pb(j) ) )
      end do

      do j=1,k+1
        gaulb(j)=r2d*pb(j)
      end do

      do j=1,k
        gaul(j)=r2d*asin(pa(j))
      end do

      return
      end

      subroutine gauaw(pa,pw,k)
c         
c****     *gauaw* - compute abscissas and weights for *gaussian integration.
c         
c         purpose.
c         --------
c         
c         *gauaw* is called to compute the abscissas and weights requir
c         to perform *gaussian integration.
c         
c**       interface.
c         ----------
c         
c         *call* *gauaw(pa,pw,k)*
c         
c         *pa*     - array, length at least *k,* to receive abscis
c         abscissas.
c         *pw*     - array, length at least *k,* to receive
c         weights.
c         
c         method.
c         -------
c         
c         the zeros of the *bessel functions are used as starting
c         approximations for the abscissas. newton iteration is used to
c         improve the values to within a tollerence of *eps.*
c         
c         external.
c         ---------
c         
c         *bsslzr* - routine to obtain zeros of *bessel functions.
c         
c         reference.
c         ----------
c         
      implicit double precision (a-h, o-z)
      double precision pa, pw
      dimension pa(k),pw(k)
      data eps/1.d-13/
c         
c         ------------------------------------------------------------------
c         
c*        1.     set constants and find zeros of bessel function.
c         --- --------- --- ---- ----- -- ------ ---------
c         
      pi = 4.*atan(1.d0)
 100  continue
      c=(1.-(2./pi)**2)*0.25
      fk=k
      kk=k/2
      call bsslzr(pa,kk)
c         
      do 290 is=1,kk
        xz=cos(pa(is)/sqrt((fk+0.5)**2+c))
c*        giving the first approximation for *xz.*
        iter=0
c         
c         ------------------------------------------------------------------
c         
c*        2.     compute abscissas and weights.
c         ------- --------- --- -------
c         
 200    continue
c         
c*        2.1     set values for next iteration.
 210    continue
        pkm2=1.
        pkm1=xz
        iter=iter+1
        if(iter.gt.10) go to 300
c         
c*        2.2     computation of the *legendre polynomial.
 220    continue
c         
        do 222 n=2,k
          fn=n
          pk=((2.*fn-1.)*xz*pkm1-(fn-1.)*pkm2)/fn
          pkm2=pkm1
          pkm1=pk
 222    continue
c         
        pkm1=pkm2
        pkmrk=(fk*(pkm1-xz*pk))/(1.-xz**2)
        sp=pk/pkmrk
        xz=xz-sp
        avsp=abs(sp)
        if(avsp.gt.eps) go to 210
c         
c*        2.3     abscissas and weights.
 230    continue
        pa(is)=xz
        pw(is)=(2.*(1.-xz**2))/(fk*pkm1)**2
c         
c*        2.4     odd *k* computation of weight at the equator.
 240    continue
        if (k.ne.kk*2) then
          pa(kk+1)=0.
          pk=2./fk**2
c         
          do 242 n=2,k,2
            fn=n
            pk=pk*fn**2/(fn-1.)**2
 242      continue
c         
          pw(kk+1)=pk
        else
c         
c*        2.5     use symmetry to obtain remaining values.
c         
 250      continue
c         
          do 252 n=1,kk
            l=k+1-n
            pa(l)=-pa(n)
            pw(l)=pw(n)
 252      continue
c         
        endif
 290  continue
c         write(6,*) iter
c         
      return
c         
c         ------------------------------------------------------------------
c         
c*        3.     error processing.
c         ----- -----------
c         
 300  continue
      write (nout,9901)
 9901 format(//,'  gauaw failed to converge after 10 iterations.')
      stop
c         
c         ------------------------------------------------------------------
c         
      end

      subroutine bsslzr(pbes,knum)
c         
c****     *bsslzr* - routine to return zeros of the j0 *bessel function.
c         
c         purpose.
c         --------
c         
c         *bsslzr* returns *knum* zeros, or if *knum>50,* *knum*
c         approximate zeros of the *bessel function j0.
c         
c**       interface.
c         ----------
c         
c         *call* *nsslzr(pbes,knum)*
c         
c         *pbes*   - array, dimensioned *knum,* to receive the
c         values.
c         *knum*   - number of zeros requested.
c         
c         method.
c         -------
c         
c         the first 50 values are obtained from a look up table. any
c         additional values requested are interpolated.
c         
c         externals.
c         ----------
c         
c         none.
c         
c         reference.
c         ----------
c         
*call     comcon
      double precision pbes(knum), zbes(50), api
      data zbes        / 2.4048255577,   5.5200781103,
     x 8.6537279129,  11.7915344391,  14.9309177086,  18.0710639679,
     x 21.2116366299,  24.3524715308,  27.4934791320,  30.6346064684,
     x 33.7758202136,  36.9170983537,  40.0584257646,  43.1997917132,
     x 46.3411883717,  49.4826098974,  52.6240518411,  55.7655107550,
     x 58.9069839261,  62.0484691902,  65.1899648002,  68.3314693299,
     x 71.4729816036,  74.6145006437,  77.7560256304,  80.8975558711,
     x 84.0390907769,  87.1806298436,  90.3221726372,  93.4637187819,
     x 96.6052679510,  99.7468198587, 102.8883742542, 106.0299309165,
     x 109.1714896498, 112.3130502805, 115.4546126537, 118.5961766309,
     x 121.7377420880, 124.8793089132, 128.0208770059, 131.1624462752,
     x 134.3040166383, 137.4455880203, 140.5871603528, 143.7287335737,
     x 146.8703076258, 150.0118824570, 153.1534580192, 156.2950342685/
c         
c         ------------------------------------------------------------------
c         
c*        1.     extract values from look up table.
c         ------- ------ ---- ---- -- ------
c         
c         set api
c         
      api=4.*atan(1.d0)
 100  continue
      inum=min0(knum,50)
c         
      do 110 j=1,inum
        pbes(j)=zbes(j)
 110  continue
c         
c         ------------------------------------------------------------------
c         
c*        2.     interpolate remaining values.
c         ----------- --------- -------
c         
 200  continue
c         
      zpi=api
      do 210 j=51,knum
        pbes(j)=pbes(j-1)+api
 210  continue
c         
c         ------------------------------------------------------------------
c         
      return
      end


      subroutine qprntu(a,qtitle,undef,
     $     ibeg,jbeg,m,n,iskip,iunit)
C        
C----------------------------------------------------------------
C         
C         version of qprntn which handles undefined values
C         by having them printed as ****
C         
C         Mike Fiorino, NMC Development Division
C        
C----------------------------------------------------------------
C         
C
C         a= fwa of m x n array
C         qtitle - title
C         ibeg,jbeg=lower left corner coords to be printed
C         up to 43 x 83 points printed
c
      dimension a(m,n),ix(81)
      character qtitle*24
c
c  determine grid limits
c
      if(iskip.eq.0) iskip=1
      iend=min0(ibeg+79*iskip,m)
      jend=min0(jbeg+79*iskip,n)
c
   24 continue
c
c  index backwards checking for max
c
   11 xm=0.
      jendsc=min0(jend,n)
      do j=jbeg,jendsc,iskip
        jend_qp = j
        do i=ibeg,iend,iskip
          xmax=abs(a(i,j))
          if(a(i,j).eq.undef) xmax=0.0
          xm=amax1(xm,xmax)
        end do
      end do
c
c  determine scaling factor limits
c
      if(xm.lt.1.0e-32.or.xm.eq.0.0) xm=99.0
      xm=alog10(99.0/xm)
      kp=xm
      if(xm.lt.0.0)kp=kp-1
c
c  print scaling constants
c
   12 write(iunit,1) qtitle,kp,iskip,(i,i=ibeg,iend,2*iskip)

    1 format('0',a,'   k=',i3,' iskip=',i2,/,' ',41i6) 
      fk=10.0**kp
c
c  quickprint field
c
      do 2 jli=jend_qp,jbeg,-iskip
        ii= 0
        if(kp.eq.0) then 
          do i=ibeg,iend,iskip
            ii=ii+1

            if(a(i,jli).eq.undef) then
              ix(ii)=999999
            else
              ix(ii)=a(i,jli)+sign(.5,a(i,jli))
            endif

          end do
        else
          do i=ibeg,iend,iskip
            ii=ii+1
            if(a(i,jli).eq.undef) then
              ix(ii)=999999
            else
              ix(ii)=a(i,jli)*fk+sign(.5,a(i,jli))
            endif
          end do
        end if
        write(iunit,'(i4,81i3)') jli,(ix(i),i=1,ii),jli
2     continue
      return
      end

      subroutine qprntn(a,qtitle,ibeg,jbeg,m,n,iskip,iunit)
c
c**********	12 APR 91 this version outputs to iunit 
c**********	using write on the Cray Y/MP 
c
c***************************************************************
c***************************************************************
c*****                                                     *****
c*****       qprint output routine (corrected 4/26/86)     *****
c*****                                                     *****
c***************************************************************
c***************************************************************
c
c a= fwa of m x n array
c qtitle - title
c ibeg,jbeg=lower left corner coords to be printed
c up to 43 x 83 points printed
c
      dimension a(m,n),ix(81)
      character qtitle*24
c
c  determine grid limits
c
      if(iskip.eq.0) iskip=1
      iend=min0(ibeg+79*iskip,m)
      jend=min0(jbeg+79*iskip,n)
c
   24 continue
c
c  index backwards checking for max
c
   11 xm=0.
      jendsc=min0(jend,n)
      do j=jbeg,jendsc,iskip
      jend_qp = j
      do i=ibeg,iend,iskip
        xm=amax1(xm,abs(a(i,j)))
      end do
      end do
c
c  determine scaling factor limits
c
      if(xm.lt.1.0e-32.or.xm.eq.0.0) xm=99.0
      xm=alog10(99.0/xm)
      kp=xm
      if(xm.lt.0.0)kp=kp-1
c
c  print scaling constants
c
   12 write(iunit,1) qtitle,kp,iskip,(i,i=ibeg,iend,2*iskip)

    1 format('0',a,'   k=',i3,' iskip=',i2,/,' ',41i6) 
      fk=10.0**kp
c
c  quickprint field
c
      do 2 jli=jend_qp,jbeg,-iskip
        ii= 0
        if(kp.eq.0) then 
          do i=ibeg,iend,iskip
            ii=ii+1
            ix(ii)=a(i,jli)+sign(.5,a(i,jli))
          end do
        else
          do i=ibeg,iend,iskip
            ii=ii+1
            ix(ii)=a(i,jli)*fk+sign(.5,a(i,jli))
          end do
        end if
        write(iunit,'(i4,81i3)') jli,(ix(i),i=1,ii),jli
2     continue
      return
      end
      function qsatw(t,p)
c
c     t is temperature of air in deg celcius.
c     ta is temperature in deg kelvin
c     p is pressure in mb
c     qsatw is saturation specific humidity in g/g (over water)
c
      data ps/1013.246/,ts/373.16/
c
      ta = t
      if(t .lt. 100.0) then
         ta = t + 273.16
      end if
      e1=11.344*(1.0-ta/ts)
      e2=-3.49149*(ts/ta-1.0)
      f1=-7.90298*(ts/ta-1.0)
      f2=5.02808*alog10(ts/ta)
      f3=-1.3816*(10.0**e1-1.0)*1.e-7
      f4=8.1328*(10.0**e2-1.0)*1.e-3
      f5=alog10(ps)
      f=f1+f2+f3+f4+f5
      es=10.0**f
      qsatw=.62197*es/(p-0.378*es)
      return
      end
c
      function qsati(t,p)
c
c     t is temperature of air in deg celcius.
c     ta is temperature in deg kelvin
c     p is pressure in mb
c     qsati is saturation specific humidity with respect to ice in g/g
c
      data p0/6.1071/,t0/273.16/
c
      t = ta
      if(t .lt. 100.0) then
         ta = t + 273.16
      end if
      f1=-9.09718*(t0/ta-1.0)
      f2=-3.56654*alog10(t0/ta)
      f3=0.876793*(1.0-ta/t0)
      f4=alog10(p0)
      es=10.0**(f1+f2+f3+f4)
      qsati=.62197*es/(p-0.378*es)
      return
      end

      subroutine read_grid(iunit,a,ni,nj,istat)
      dimension a(ni,nj)
      istat=1
      read(iunit,err=800) a
      go to 999
 800  continue
      istat=0
 999  continue
      return
      end

      function satvp(temp)
      real a(7)
      data a/6.10779961,0.4436518521,0.01428945805,2.650648471e-4,
     &       3.031240396e-6,2.034080948e-8,6.136820929e-11/
      data e0, aa, bb/6.1078, 17.2693882, 237.3/
      data t0/273.16/
c
      t = temp
c convert temperature to degrees c, if necessary.
      if(temp .gt. 100.0) t = temp - t0
c polynomial is only good from -50 to + 50 c.
c if outside this range teton's formmula is used.
      if(t .gt. 50.0 .or. t .lt. -50.0) then
        satvp = e0*exp(aa*t/(t+bb))
      else
      satvp = a(1)+t*(a(2)+t*(a(3)+t*(a(4)+t*(a(5)+t*(a(6)+a(7)*t)))))
      end if
      return
      end
 
      subroutine sfc_area(fld,rlon,rlat,undef,ni,nj,
     $     area,iunit_diag)
      dimension fld(ni,nj),rlat(nj+1),rlon(ni+1),
     $     area(ni,nj)
      deg2rad=3.14115926/180.0
      do j=1,nj
        do i=1,ni
          dlon=(rlon(i+1)-rlon(i))*deg2rad
          dlat=sin(rlat(j+1)*deg2rad)-sin(rlat(j)*deg2rad)
          area(i,j)=dlon*dlat
          if(fld(i,j).eq.undef) area(i,j)=0.0
        end do
        rlatout=(rlat(j+1)+rlat(j))*0.5
cccc        write(iunit_diag,*) 'j = ',j,' sfc area = ',rlatout,area(1,j)
      end do

      return
      end

      subroutine trim_grid(a,ni,niinew,nj,dum)
      dimension a(ni,nj),dum(niinew,nj)
      do j=1,nj
        do i=1,niinew
          dum(i,j)=a(i,j)
        end do
      end do
      return
      end

      subroutine vload2(a,b,ni,nj)
      dimension a(ni,nj),b(ni,nj)
      do j=1,nj
        do i=1,ni
          a(i,j)=b(i,j)
        end do
      end do
      return
      end
      function ichar_len(c,imax)
      character*1 c(imax)
      iend=-1
      ii=1
      do while (iend.eq.-1.and.ii.le.imax)
        if(c(ii).eq.' ') iend=ii
        ii=ii+1
      end do  
      if(ii.gt.imax) then
        ichar_len=imax
      else
        ichar_len=iend-1
      end if
      return
      end

      subroutine ul_case(cc,ilen)
      character*1 cc(ilen)
      do i=1,ilen
        if(ichar(cc(i)).ge.65.and.ichar(cc(i)).le.90)
     $       cc(i)=char(ichar(cc(i))+32)
      end do
      return
      end
      
      subroutine write_grid(iunit,a,ni,nj)
      dimension a(ni,nj)
      write(iunit) a
      return
      end
