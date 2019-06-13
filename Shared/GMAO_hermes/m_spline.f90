module m_spline
use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
implicit none
private
public :: spline
public :: ispline
interface spline
   module procedure spline_
   module procedure spline_interp_r4_
   module procedure spline_interp_r8_
end interface
contains
  subroutine spline_interp_r8_ ( xi, xo, yi, yo )

  implicit none

  real(kind=REAL64),intent(in)    :: xi(:)
  real(kind=REAL64),intent(in)    :: xo(:)
  real(kind=REAL64),intent(in)    :: yi(:)
  real(kind=REAL64),intent(inout) :: yo(:)

  real(kind=REAL64), allocatable :: b(:), c(:), d(:)
  real(kind=REAL64), allocatable :: xxi(:)
  real(kind=REAL64), allocatable :: yyi(:)
  real(kind=REAL64), allocatable :: xxo(:)

  integer k,ni,no
  logical reverse_

  ni=size(xi)
  no=size(xo)

  allocate(b(ni),c(ni),d(ni))
  allocate(xxi(ni))
  allocate(yyi(ni))
  allocate(xxo(no))

  reverse_=xi(1)>xi(ni)
  if (reverse_) then
     xxi=xi(ni:1:-1)
     yyi=yi(ni:1:-1)
     xxo=xo(no:1:-1)
  else
     xxi=xi
     yyi=yi
     xxo=xo
  endif

! Derive spline coefficients
  call spline_ ( xxi, yyi, b, c, d, ni )

! Apply spline coefficients
  yo(1)=yyi(1)
  do k=2,no-1
     yo(k) = ispline( xxo(k), xxi, yyi, b, c, d, ni)
  enddo
  yo(no)=yyi(ni)
  if (reverse_) then
     yo=yo(no:1:-1)
  endif

  deallocate(xxo)
  deallocate(yyi)
  deallocate(xxi)
  deallocate(b,c,d)

  end subroutine spline_interp_r8_

  subroutine spline_interp_r4_ ( xi, xo, yi, yo )

  implicit none

  real(kind=REAL32) :: xi(:)
  real(kind=REAL32) :: xo(:)
  real(kind=REAL32) :: yi(:)
  real(kind=REAL32) :: yo(:)

  real(kind=REAL64), allocatable, dimension(:) :: b(:), c(:), d(:)
  real(kind=REAL64), allocatable, dimension(:) :: xxi(:)
  real(kind=REAL64), allocatable, dimension(:) :: yyi(:)
  real(kind=REAL64), allocatable, dimension(:) :: xxo(:)

  integer k,ni,no
  logical reverse_

  ni=size(xi)
  no=size(xo)

  allocate(b(ni),c(ni),d(ni))
  allocate(xxi(ni))
  allocate(yyi(ni))
  allocate(xxo(no))

  reverse_=xi(1)>xi(ni)
  if (reverse_) then
     xxi=xi(ni:1:-1)
     yyi=yi(ni:1:-1)
     xxo=xo(no:1:-1)
  else
     xxi=xi
     yyi=yi
     xxo=xo
  endif

! Derive spline coefficients
  call spline_ ( xxi, yyi, b, c, d, ni )

! Apply spline coefficients
  yo(1)=yyi(1)
  do k=2,no-1
     yo(k) = ispline( xxo(k), xxi, yyi, b, c, d, ni)
  enddo
  yo(no)=yyi(ni)
  if (reverse_) then
     yo=yo(no:1:-1)
  endif

  deallocate(xxo)
  deallocate(yyi)
  deallocate(xxi)
  deallocate(b,c,d)

  end subroutine spline_interp_r4_

   subroutine spline_ (x, y, b, c, d, n)
!======================================================================
!  Calculate the coefficients b(i), c(i), and d(i), i=1,2,...,n
!  for cubic spline interpolation
!  s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
!  for  x(i) <= x <= x(i+1)
!  Alex G: January 2010
!----------------------------------------------------------------------
!  input..
!  x = the arrays of data abscissas (in strictly increasing order)
!  y = the arrays of data ordinates
!  n = size of the arrays xi() and yi() (n>=2)
!  output..
!  b, c, d  = arrays of spline coefficients
!  comments ...
!  spline.f90 program is based on fortran version of program spline.f
!  the accompanying function fspline can be used for interpolation
!======================================================================
implicit none
integer n
real(kind=REAL64) :: x(n), y(n), b(n), c(n), d(n)
integer i, j, gap
real(kind=REAL64) :: h

gap = n-1
! check input
if ( n < 2 ) return
if ( n < 3 ) then
  b(1) = (y(2)-y(1))/(x(2)-x(1))   ! linear interpolation
  c(1) = 0.
  d(1) = 0.
  b(2) = b(1)
  c(2) = 0.
  d(2) = 0.
  return
end if
!
! step 1: preparation
!
d(1) = x(2) - x(1)
c(2) = (y(2) - y(1))/d(1)
do i = 2, gap
  d(i) = x(i+1) - x(i)
  b(i) = 2.0*(d(i-1) + d(i))
  c(i+1) = (y(i+1) - y(i))/d(i)
  c(i) = c(i+1) - c(i)
end do
!
! step 2: end conditions 
!
b(1) = -d(1)
b(n) = -d(n-1)
c(1) = 0.0
c(n) = 0.0
if(n /= 3) then
  c(1) = c(3)/(x(4)-x(2)) - c(2)/(x(3)-x(1))
  c(n) = c(n-1)/(x(n)-x(n-2)) - c(n-2)/(x(n-1)-x(n-3))
  c(1) = c(1)*d(1)**2/(x(4)-x(1))
  c(n) = -c(n)*d(n-1)**2/(x(n)-x(n-3))
end if
!
! step 3: forward elimination 
!
do i = 2, n
  h = d(i-1)/b(i-1)
  b(i) = b(i) - h*d(i-1)
  c(i) = c(i) - h*c(i-1)
end do
!
! step 4: back substitution
!
c(n) = c(n)/b(n)
do j = 1, gap
  i = n-j
  c(i) = (c(i) - d(i)*c(i+1))/b(i)
end do
!
! step 5: compute spline coefficients
!
b(n) = (y(n) - y(gap))/d(gap) + d(gap)*(c(gap) + 2.0*c(n))
do i = 1, gap
  b(i) = (y(i+1) - y(i))/d(i) - d(i)*(c(i+1) + 2.0*c(i))
  d(i) = (c(i+1) - c(i))/d(i)
  c(i) = 3.*c(i)
end do
c(n) = 3.0*c(n)
d(n) = d(n-1)
end subroutine spline_

  function ispline(u, x, y, b, c, d, n)
!======================================================================
! function ispline evaluates the cubic spline interpolation at point z
! ispline = y(i)+b(i)*(u-x(i))+c(i)*(u-x(i))**2+d(i)*(u-x(i))**3
! where  x(i) <= u <= x(i+1)
!----------------------------------------------------------------------
! input..
! u       = the abscissa at which the spline is to be evaluated
! x, y    = the arrays of given data points
! b, c, d = arrays of spline coefficients computed by spline
! n       = the number of data points
! output:
! ispline = interpolated value at point u
!=======================================================================
implicit none
real(kind=REAL64) :: ispline
integer n
real(kind=REAL64) :: u, x(n), y(n), b(n), c(n), d(n)
integer i, j, k
real(kind=REAL64) :: dx

! if u is ouside the x() interval take a boundary value (left or right)
if(u <= x(1)) then
  ispline = y(1)
  return
end if
if(u >= x(n)) then
  ispline = y(n)
  return
end if

!*
!  binary search for for i, such that x(i) <= u <= x(i+1)
!*
i = 1
j = n+1
do while (j > i+1)
  k = (i+j)/2
  if(u < x(k)) then
    j=k
    else
    i=k
   end if
end do
!*
!  evaluate spline interpolation
!*
dx = u - x(i)
ispline = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
end function ispline

end module m_spline

