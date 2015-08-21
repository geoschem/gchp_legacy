!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!
!
! !PROGRAM:      zeit_pr.f
!
! !DESCRIPTION:  A program for printing process times for programs being
!                run in shell scripts
!
! !USAGE:      zeit_pr.x [-i fname] [-o out_fname] [-d] [-h]
!
!              where:    -i          Indicate use of a users registry file.
!                        fname       Name of registry input file; .zeit is 
!                                    the default fname.
!                        -o          Indicate name of an outputfile.
!                        out_fname   Output file name of current timings
!                                    report.  If none is specified the report
!                                    prints to standard output.
!                        -d          An option to remove the registry file.
!                        -h          An option to print usage information.

      use m_zeit
      implicit none
      integer n,i
      character*200 fname,out_fname,argin(10)
      integer*4 iargc
      logical dopt,hopt,infile,outfile
      dopt=.false.
      hopt=.false.
      infile=.false.
      outfile=.false.
      n=iargc()
      do i=1,n
         call getarg(i,argin(i))
      enddo
      do i=1,n
         if(argin(i).eq.'-d')then
            dopt=.true.
         elseif(argin(i).eq.'-h')then
            hopt=.true.
         elseif(argin(i).eq.'-i')then
            infile=.true.
            fname=trim(argin(i+1))
         elseif(argin(i).eq.'-o')then
            outfile=.true.
            out_fname=trim(argin(i+1))
         endif
      enddo
      if(n.eq.0)then
         fname='.zeit'
         out_fname='stdout'
      endif
      if(n.eq.1)then
         if(argin(1).eq.'-i'.or.argin(1).eq.'-o')then
            call zeit_usage('zeit_pr')
            stop
         endif
      endif
      if(.not.infile)then
         fname='.zeit'
      endif
      if(.not.outfile)then
         out_fname='stdout'
      endif
      if(hopt)then
         if(n.eq.1)then
            call zeit_usage('zeit_pr')
            stop
         endif
         call zeit_usage('zeit_pr')
      endif
      call zeit_read(fname,out_fname,dopt)
      stop
      end




