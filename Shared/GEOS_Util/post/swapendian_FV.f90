program swapFV

  implicit none
  
  integer :: int_pack(6)
  integer :: unitR
  integer :: unitW
  integer :: IM, JM, KM
  integer :: L, status
  real*8,allocatable  :: C(:)
  real*8,allocatable  :: A(:,:)
  character(128) :: str, f_in, f_out

  integer*4 :: iargc
  external  :: iargc


! Begin

  if (iargc() /= 2) then
     call getarg(0,str)
     write(*,*) "Usage:",trim(str)," <big_endian_FV_internal_in> <native_FV_interal_out>"
     call exit(2)
  end if

  call getarg(1,f_in)
  call getarg(2,f_out)

  unitR = 7
  unitW = 8

  open(unit=unitR, file=trim(f_in),  convert="big_endian", form="unformatted")
  open(unit=unitW, file=trim(f_out), convert="native",     form="unformatted")

  read (unitR) int_pack(1:6)
  write(unitW) int_pack(1:6)
  read (unitR) int_pack(1:5)
  write(unitW) int_pack(1:5)
  
  IM = int_pack(1)
  JM = int_pack(2)
  KM = int_pack(3)

  print *, 'Converting FV restart: ',trim(f_in)
  print *, '           Resolution: ',im,jm,km

  allocate(C(KM+1), A(IM,JM), stat=status)
  if (status /=0 ) call exit(1)

  read (unitR) C !AK
  write(unitW) C
  read (unitR) C !BK
  write(unitW) C

  do L = 1, 5*KM+1 ! 5 vars (U,V,PT,PE,PKZ), PE is edge quantity and has extra level
     read (unitR) A
     write(unitW) A
  end do

  deallocate(a, c)

  close(unitR)
  close(unitW)

end program swapFV
