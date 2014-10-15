
      program mapz

! ****************
! Vertical mapping
! ****************

! this program is for mapping from an arbitrary vertical domain
! with to an arbitrary vertical domain with the same surface pressure
! and same horizontal resolution

! Developer: S.-J. Lin
! Mar 27, 2000
! R.Todling: Dec 22, 2004 make it work from m_mapz; implicit none

      use m_mapz, only : z_mapping
      implicit none
      integer im, jm, km, nl
      integer ih
      integer iuic, iout

! Horizontal resolution: (im,jm)
! Number of constituents including water vapor: nc
      integer, parameter :: nc = 1

      real ptop

! Original data with km layers

! vertical resolution of the target: nl

      write(6,*) '*************************************************'
      write(6,*) 'This program is for the vertical mapping of fvgcm'
      write(6,*) 'dynamical restart file: d_rst'
      write(6,*) '*************************************************'

      write(6,*) ' '
      write(6,*) 'Resoultion? Choose from the following:'
      write(6,*) '0: 4x5;  1: 2x2.5;  2: 1x1.25;  3: 0.5x0.625'
      read(*,*) ih

      if( ih .eq. 0) then
          im = 72
          jm = 46
      elseif( ih .eq. 1) then
          im = 144
          jm = 91
      elseif( ih .eq. 2) then
          im = 288
          jm = 181
      elseif( ih .eq. 3) then
          im = 576
          jm = 361
      else
          write(6,*) 'No suitable resolution chosen'
          stop
      endif
      
      write(6,*) 'Original vertical dimension km=?'
      read(*,*) km

      write(6,*) 'Original model top (Pa)'
      read(*,*) ptop

      write(6,*) 'Vertical dimension of the target nl=?'
      read(*,*) nl

      write(6,*) ' '
      write(6,*) 'Input file name is assumed to be d_rst'
      write(6,*) 'Output file will be d_rst_new'
      iuic = 71
      iout = 81
      open (unit=iuic,file='d_rst',form='unformatted',status='old')
      open (unit=iout,file='d_rst_new',form='unformatted',
     &      status='unknown')

      call z_mapping(iuic, iout, im, jm, km, nc, ptop, nl) 

      end
