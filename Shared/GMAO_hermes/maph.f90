      program mapxy
 
! Program to map fvgcm dynamical core restart file: d_rst from
! one horizontal to another
! S.-J. Lin: April 1, 2000
! R.Todling: Dec 22, 2004 make it work from m_maph

      use m_maph
      implicit none

      integer im, jm                    ! original dimension
      integer in, jn                    ! target dimension
      integer km
      integer nc
      integer hold
      integer hnew

      integer iuic, iout

      write(6,*) '**************************************************'
      write(6,*) 'This program is for the horizontal mapping of fvgcm'

      write(6,*) 'dynamical restart file: d_rst'
      write(6,*) '**************************************************'

      write(*,*) 'Original data resolution?'
      write(*,*) '0: 4x5; 1: 2x2.5; 2: 1x1.25; 3: 0.5x0.625'
      read(*,*) hold

      write(*,*) 'Target data resolution?'
      write(*,*) '0: 4x5; 1: 2x2.5; 2: 1x1.25; 3: 0.5x0.625'
      read(*,*) hnew

! Original resolution
      if( hold .eq. 0 ) then
          im = 72
          jm = 46
      elseif( hold .eq. 1) then
          im = 144
          jm =  91
      elseif( hold .eq. 2) then
          im = 288
          jm = 181
      elseif( hold .eq. 3) then
          im = 576
          jm = 361
      else
          write(*,*) 'No suitable resolution chosen'
          stop
      endif

! New resolution

      if( hnew .eq. 0 ) then
          in = 72
          jn = 46
      elseif( hnew .eq. 1) then
          in = 144
          jn =  91
      elseif( hnew .eq. 2) then
          in = 288
          jn = 181
      elseif( hnew .eq. 3) then
          in = 576
          jn = 361
      else
          write(*,*) 'No suitable resolution chosen'
          stop
      endif

      write(6,*) 'Vertical dimension km=?'
      read(*,*) km


      write(6,*) 'Total number of tracers including water vapor=?'
      read(*,*) nc


      write(6,*) ' '
      write(6,*) 'Input file name is assumed to be d_rst'
      write(6,*) 'Output file will be d_rst_new'

      iuic = 71
      iout = 81

      open (unit=iuic,file='d_rst',form='unformatted',status='old')
      open (unit=iout,file='d_rst_new',form='unformatted', &
            status='unknown')

!     call h_map_drst(iuic, iout, im, jm, km, nc, in, jn)
      call h_map(iuic, iout, im, jm, km, nc, in, jn)

      end
