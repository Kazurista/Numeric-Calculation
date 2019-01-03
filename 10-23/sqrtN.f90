program sqrtN
  implicit none
  real(8) a, f, df, x
  integer :: i, k=20, n
  read(5,*) a
  x=1.0d0
  do i=1, k
   f = x ** 5 - a
   df = 5 * x ** 4
   x = x - f / df
   write(6,*) 'sqrt ', a, x
  end do
end program sqrtN
