!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!
!
! !PROGRAM:      zeit_read
!
! !DESCRIPTION:  A program called by zeit_pr that reads registry file
!                input and produces reports of current timings
!
! !USAGE:        zeit_read(fname,out_fname,dopt)
!                where:  fname       Name of registry input file; .zeit is 
!                                    the default fname
!                        out_fname   Output file name of current timings
!                                    report. If not specified the output is to
!                                    standard output 

      subroutine zeit_read(fname,out_fname,dopt)
      use m_zeit
      implicit none
      character*200 fname,out_fname,command
      integer i,begstr,endstr,ymd,hour,minute,sec,msec,jday,base_day,
     $     umask
      integer zeit_jday
      integer ios,ci_count,co_count
      parameter(ios=0)
      character*4 junk
      character*7 state
      character*50 name
      character*12 pname
      logical present,dopt
      real*8 tms(0:5,0:1)
      real*8 check_time
      inquire(file=fname,exist=present)
      ci_count=0
      co_count=0
      if(present)then
         open(11,file=fname,status='old')
         i=1
         do while(ios.eq.0)
            read(11,100,end=99)state,name,junk,ymd,junk,hour,
     $           minute,sec,msec
 100        format(a7,1x,a50,1x,a4,1x,i8,1x,a4,1x,i2,1x,i2,1x,i2,1x,i3)        
            endstr=len(trim(name))
            begstr=endstr-11
            pname=name
            if(endstr.gt.12)then
               pname=name(begstr:endstr)
            endif
            jday=zeit_jday(ymd)
            if(i.eq.1)then
               base_day=jday
               check_time=(real(hour)*3600.0)+(real(minute)*60.0)+
     $              real(sec)+real(msec)/1000.0
            else
               check_time=real((jday-base_day)*86400.0)+
     $              (real(hour)*3600.0)+(real(minute)*60.0)+
     $              real(sec)+real(msec)/1000.0
            endif
            if(state.eq.'zeit_ci')then
               call zeit_ci(trim(pname),check_time)
               ci_count=ci_count+1
            elseif(state.eq.'zeit_co')then
               call zeit_co(trim(pname),tms,check_time)
               co_count=co_count+1
            else
               print*,'Error in registry input file'
            endif
            i=i+1
         enddo
 99      if(ci_count.ne.co_count)then
            print*,'Unbalanced ci/co condition in registry file!'
            print*,'Check calling script to make sure calls balance.'
            print*,'Program will continue normally.'
         endif
         if(out_fname.eq.'stdout')then
            call zeit_flush(6,umask,check_time)
         else
            open(12,file=out_fname,status='unknown')
            call zeit_flush(12,umask,check_time)         
         endif      
         command='rm '//fname
         if(dopt)then
            call system(command)
         endif
      else
         print*,'Error! Could not find registry input file'
         stop
      endif
      stop
      end
