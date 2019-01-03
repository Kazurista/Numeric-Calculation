program n_n_matrix

implicit none
integer :: n, m, i, k, j
real(8), allocatable :: a(:, :)
real(8), allocatable :: b(:, :)
real(8), allocatable :: c(:, :)

! Insert matrix_A from mat_a.txt

open(11, file = 'mat_a.txt')
read(11, *) n
allocate (a(n, n))

do i = 1, n
  read(11, *) a(i, 1:n)
end do

write (*, *) 'matrix_A is...'
do i = 1, n
  write(*, '(f15.1)') (a(i, 1:n))
end do

close(11)

! Insert matrix_B from mat_b.txt

open(12, file = 'mat_b.txt')
read(12, *) m
allocate (b(m, m))

do i = 1, m
  read(12, *) b(i, 1:m)
end do

write (*, *) 'matrix_B is...'
do i = 1, m
  write(*, '(f15.1)') (b(i, 1:m))
end do

close(12)

! Calculate matrix_C by multiplication two matrix!
allocate(c(n, n))

do j = 1, n
  do i = 1, n
    c(i, j) = 0.0d0
    do k = 1, n
      c(i, j) = c(i, j) + a(i, k) * b(k, j)
    end do
  end do
end do

write (*, *) 'matrix_A multiplied by matrix_B equals...'
do i =1, n
  write(*, '(f15.1)') (c(i, 1:n))
end do

deallocate (c)
deallocate (a)
deallocate (b)
end program n_n_matrix
