program mat2

implicit none
integer :: n, i
real(8), allocatable :: a(:, :)
open(10, file = 'mat.txt')
read(10, *) n
allocate (a(n, n))

do i = 1, n
  read(10, *) a(i, 1:n)
end do

do i = 1, n
  write(*, '(f15.2)') (a(i, 1:n))
end do

deallocate (a)
end program mat2
