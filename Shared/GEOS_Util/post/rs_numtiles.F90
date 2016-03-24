program rs_numtiles  

  use iso_fortran_env
  use MAPL_IOMod

  implicit none

  character(256)    :: fname1

#ifndef __GFORTRAN__
  integer           :: ftell
  external          :: ftell
#endif

  integer           :: bpos, epos, ntiles, nargs

  type(MAPL_NCIO) :: NCIO
  integer :: rc, filetype
    
! Usage
! -----

  nargs = command_argument_count()

  if (nargs /= 1) then
     write (output_unit,*) "rs_numtiles.x requires one input."
     write (output_unit,*) "   Usage: rs_numtiles.x <Input_Restart>"
     write (output_unit,*) ""
     write (output_unit,*) "   NOTE: rs_numtiles.x will always return the correct number of tiles"
     write (output_unit,*) "         for NetCDF4 restarts, but binary restarts perhaps not. This"
     write (output_unit,*) "         This program looks at the first record of a binary restart. If"
     write (output_unit,*) "         that record has subtiles, it will be a multiple of the number"
     write (output_unit,*) "         of tiles."
     call exit(2)
  end if

  call get_command_argument(1, fname1)

! Open INPUT Restart File
! -----------------------

  call MAPL_NCIOGetFileType(trim(fname1), filetype, rc=rc)

  if (filetype == 0) then
     NCIO = MAPL_NCIOOpen(trim(fname1))
  else
     open (unit=10, file=trim(fname1), form='unformatted')
  end if

! Determine NTILES
! ----------------

  if (filetype == 0) then
     call MAPL_NCIOGetDimSizes(NCIO,tile=ntiles)
  else
     bpos=0
     read (10)
     epos = ftell(10)            ! ending position of file pointer
     ntiles = (epos-bpos)/4-2    ! record size (in 4 byte words; 
     rewind 10
  end if

  write (output_unit,100) ntiles

! Close INPUT Restart File
! ------------------------

  if (filetype == 0) then
     call MAPL_NCIOClose(NCIO)
  else
     close (10)
  end if

100 format (1x,'Total Tiles: ',i10)

end program rs_numtiles
