!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  Testgfio
!
! !DESCRIPTION: This program is used to test the operation of the GFIO
!               library.
!
! !INTERFACE:
!
      program testgfio
!
! !USES:
!
      Implicit NONE
#define C_IM 72
#define C_JM 46
#define C_KM 3
#define C_LM 20
#define C_NVARS 3

!
! !INPUT PARAMETERS:
!  
!  None.
!
! !OUTPUT PARAMETERS:
!
!  None.
!
! !REVISION HISTORY:
!
!  1998.08.14  Lucchesi           Initial coding.
!
!EOP
!-------------------------------------------------------------------------


       character*80 fname
       character*80 fname2
       character*80 fname3
       character*120 title
       character*80 source
       character*40 contact
       real amiss
       integer im,jm,km,lm
       real lon(C_IM)              ! should be im long
       real lat(C_JM)              ! should be jm long
       real levs(C_KM)             ! should be km long
       character*8 levunits
       integer beg_date
       integer beg_time
       integer timinc
       integer nvars, ngatts
       character*257 vname(C_NVARS) 
       character*40 vtitle(C_NVARS) 
       character*40 vunits(C_NVARS)
       character*256 aname(50)
       integer kmvar(C_NVARS)
       real valid_range(2,C_NVARS)
       real valid_range2(2,C_NVARS)
       real packing_range(2,C_NVARS)
       integer yyyymmdd(C_LM), hhmmss(C_LM)

       real grid2D(C_IM,C_JM)
       real grid3D(C_IM,C_JM,C_KM)
       real read_grid3D(C_IM,C_JM,C_KM)
       real read_grid2D_1(C_IM,C_JM)
       real read_grid2D_2(C_IM,C_JM)
       real*4  r32
       real*8  r64
    
       integer fid
       integer fid2
       integer fid3
       integer rc
       integer type, count
       real rbuf(10)
       character*40 cbuf

       data cbuf /"                                        "/
       data fname /"test_gfio.nc"/
       data fname2 /"test_gfio64.nc"/
       data fname3 /"grads.lats.nc"/
       data title /"Dataset produced by GFIO interface"/
       data source /"NASA GSFC/Global Modeling and Assimilation Office"/
       data contact /"data@gmao.gsfc.nasa.gov"/
       data levunits /"mb"/
c      data levunits /"sigma_level"/
       data amiss /-999.9/
       data im /C_IM/
       data jm /C_JM/
       data km /C_KM/
       data lm /C_LM/
       data beg_date /19971014/
       data beg_time /000000/
       data timinc /060000/
       data nvars /3/
       data vtitle /"Sea Level Pressure", "Geopotential Heights",
     .              "Temperature"/
       data vunits /"Millibars", "Meters", "Kelvin"/
       data kmvar /0,C_KM,C_KM/

       integer i,j,k,l
       real lonInterval, latInterval, rtemp

!
! Fill data arrays with fake data.
!
       do j=1,jm
         do i=1,im
           grid2D(i,j)=j
         enddo
       enddo

       do k=1,km
         do j=1,jm
           do i=1,im
             grid3D(i,j,k)=k
           enddo
         enddo
       enddo
!
! Define variable names.
!
       vname(1)="slp"
       vname(2)="hght"
       vname(3)="tmpu"
!
! Define dimensions.
!
       lonInterval = 360.0/im
       rtemp = -180.0 + lonInterval*0.5
        
       do i=1,im
         lon(i) = rtemp
         rtemp = rtemp + lonInterval
       enddo

       latInterval = 180.0/(jm-1)
       rtemp = -90.0 
       do j=1,jm
         lat(j) = rtemp
         rtemp = rtemp + latInterval
       enddo

       rtemp = 0.0
       do k=1,km
         levs(k) = rtemp 
         rtemp = rtemp + 1.0
       enddo

!
! Set packing/valid_range info.
!
 
       packing_range(1,1) = amiss 
       packing_range(2,1) = amiss
       packing_range(1,2) = 0.0   
       packing_range(2,2) = 5.0
       packing_range(1,3) = amiss
       packing_range(2,3) = amiss
       valid_range(1,1) = 0.0
       valid_range(2,1) = 1200.0
       valid_range(1,2) = 1000.0
       valid_range(2,2) = 0.0
       valid_range(1,3) = amiss
       valid_range(2,3) = amiss

!      packing_range(1,1) = amiss 
!      packing_range(1,2) = amiss
!      packing_range(2,1) = amiss 
!      packing_range(2,2) = amiss
!      packing_range(3,1) = amiss
!      packing_range(3,2) = amiss
!      valid_range(1,1) = amiss 
!      valid_range(1,2) = amiss
!      valid_range(2,1) = amiss 
!      valid_range(2,2) = amiss
!      valid_range(3,1) = amiss
!      valid_range(3,2) = amiss
      
!
!  Create a new file.
!
       call Gfio_Create ( fname, title, source, contact, amiss,
     .                    im, jm, km, lon, lat, levs, levunits,
     .                    beg_date,beg_time,timinc,nvars,vname,
     .                    vtitle,vunits,kmvar,valid_range,
     .                    packing_range,0,fid,rc )

!
!  Create a new file.
!
       call Gfio_Create ( fname2, title, source, contact, amiss,
     .                    im, jm, km, lon, lat, levs, levunits,
     .                    beg_date,beg_time,timinc,nvars,vname,
     .                    vtitle,vunits,kmvar,valid_range,
     .                    packing_range,1,fid2,rc )

!
!  Add some user-defined attributes.
!
       call GFIO_PutIntAtt ( fid, "int_attribute", 1, 999, 1, rc)
       call GFIO_PutRealAtt ( fid, "real_attribute", 10, grid2D, 0, rc)
       call GFIO_PutCharAtt ( fid, "char_attribute", 5, "hello", rc)
!
! Write data for time 1.
!
       call Gfio_PutVar ( fid, vname(1), 19971014, 000000, im, jm, 0,
     .                    1, grid2D, rc)
       if (rc .lt. 0) then
          print *, vname(1),' warning. rc=',rc
       endif
       call Gfio_PutVar ( fid2, vname(1), 19971014, 000000, im, jm, 0,
     .                    1, grid2D, rc)
       if (rc .lt. 0) then
          print *, vname(1),' warning. rc=',rc
       endif
       call Gfio_PutVar ( fid, vname(2), 19971014, 000000, im, jm, 1,
     .                    km, grid3D, rc)
       if (rc .lt. 0) then
          print *, vname(2),' warning. rc=',rc
       endif
       call Gfio_PutVar ( fid, vname(3), 19971014, 000000, im, jm, 1,
     .                    km, grid3D, rc)
       if (rc .lt. 0) then
          print *,vname(3),' warning. rc=',rc
       endif
!
! Write data for time 2.
!
       call Gfio_PutVar (fid,vname(1),19971014,060000,im,jm,0,
     .                    1, grid2D, rc)
       if (rc .lt. 0) then
          print *, vname(1),' warning. rc=',rc
       endif

       call Gfio_PutVar (fid2, vname(1),19971014,060000,im,jm,0,
     .                    1, grid2D, rc)
       if (rc .lt. 0) then
          print *, vname(1),' warning. rc=',rc
       endif
       call Gfio_PutVar ( fid, vname(2), 19971014, 060000, im, jm, 1,
     .                    km, grid3D, rc)
       if (rc .lt. 0) then
          print *, vname(2),' warning. rc=',rc
       endif
       call Gfio_PutVar ( fid, vname(3), 19971014, 060000, im, jm, 1,
     .                    km, grid3D, rc)
       if (rc .lt. 0) then
          print *, vname(3),' warning. rc=',rc
       endif
!
! Close file.
!
       call Gfio_Close ( fid, rc )
       call Gfio_Close ( fid2, rc )
!
! Open file to read information back.
!
       call Gfio_Open ( fname, 0, fid, rc )
       if ( rc .LT. 0 ) then
         print *, 'ERROR OPENING ',fname
         stop
       endif
       call Gfio_Open ( fname2, 0, fid2, rc )
       if ( rc .LT. 0 ) then
         print *, 'ERROR OPENING ',fname2
         stop
       endif
       call Gfio_Open ( fname3, 0, fid3, rc )
       if ( rc .LT. 0 ) then
         print *, 'Cannot find ', trim(fname3)
       else
         print *, 'Using '//trim(fname3)//' instead of '//trim(fname)
         fid = fid3
       endif


!
! Get dimension information.
!
       call Gfio_DimInquire (fid, im, jm, km, lm, nvars, ngatts, rc )
       if ( rc .ne. 0 ) then
          print *, 'ERROR from Gfio_DimInquire ', rc
          stop
       end if

       print *, 'Results of DimInquire'
       print *, '---------------------'
       print *, 'im=',im
       print *, 'jm=',jm
       print *, 'km=',km
       print *, 'lm=',lm
       print *, 'nvars=',nvars
       print *, 'ngatts=',ngatts
!
! Get full information.
!
       call Gfio_Inquire ( fid, im, jm, km, lm, nvars,
     .                     title, source, contact, amiss,
     .                     lon, lat, levs, levunits,
     .                     yyyymmdd, hhmmss, timinc,
     .                     vname, vtitle, vunits, kmvar,
     .                     valid_range2, packing_range, rc)

       if ( rc .ne. 0 ) then
          print *, 'ERROR from Gfio_Inquire ', rc
          stop
       end if
       print *, 'Data Dump from Inquire'
       print *, '----------------------'
       print *, 'im=',im,' jm=',jm,' km=',km,' lm=',lm,' nvars=',nvars
       print *, 'title=',title
       print *, 'source=',source
       print *, 'contact=',contact
       print *, 'amiss=',amiss
       print *, 'lon=',lon
       print *, 'lat=',lat
       print *, 'levs=',levs
       print *, 'levunits=',levunits
       do i=1,lm
        print *, 'yyyymmdd=',yyyymmdd(i)
        print *, 'hhmmss=',hhmmss(i)
       enddo
       print *, 'timinc=',timinc
       print *, 'vname=',vname
       print *, 'vtitle=',vtitle
       print *, 'vunits=',vunits
       print *, 'kmvar=',kmvar
       print *, 'valid_range=',valid_range2
       print *, 'packing_range=',packing_range
       print *, 'rc=',rc
!
! Read a variable from the file.
!
       call Gfio_GetVar ( fid, vname(3), 19971014, 060000, im, jm, 1,
     .                    km, read_grid3D, rc )
       if (rc .LT. 0) then
         print *, 'ERROR READING ',vname(3),'rc=',rc
         stop
       endif
       print *, 'grid=', read_grid3D

!
! Read and compare data from the two files.
!

       do L=1,lm
         print *, 'Comparison for ',yyyymmdd(L),' at  ',hhmmss(L)
         print *, 'vname=',vname(1)
         call GFIO_GetVar (fid,vname(1),yyyymmdd(L),hhmmss(L),im,jm,1,
     .                     1, read_grid2D_1, rc)
         if (rc < 0) then
            print *, 'error reading file 1'
            stop
         endif
         call GFIO_GetVar (fid2,vname(1),yyyymmdd(L),hhmmss(L),im,jm,1,
     .                     1, read_grid2D_2, rc)
         if (rc < 0) then
            print *, 'error reading file 2'
            stop
         endif
       enddo
!
! Read user-defined attributes.
!
       call Gfio_AttInquire (fid,"int_attribute", type, count, rc)
       call Gfio_GetIntAtt ( fid, "int_attribute", 1, type, rc)
       print *, 'integer value=',type, ' rc=',rc
       call Gfio_AttInquire (fid,"real_attribute", type, count, rc)
       call Gfio_GetRealAtt ( fid, "real_attribute", count, rbuf, rc)
       print *, 'real array=',rbuf
       call Gfio_AttInquire (fid,"char_attribute", type, count, rc)
       call Gfio_GetCharAtt ( fid, "char_attribute", count, cbuf, rc)
       print *, 'character string=',cbuf

       call Gfio_GetAttNames (fid,ngatts,aname,rc)
       do i=1,ngatts
          print *,'Attribute ',i,' is ', aname(i)
       enddo
!
! Close file.
!
       call Gfio_Close (fid, rc)
       call Gfio_Close (fid2, rc)

       stop
       end
