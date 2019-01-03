module subprog
  implicit none
contains

  subroutine insert_matrix(x, n, f_no) ! txtファイルから行列を読み込むサブルーチン
    real(8), allocatable :: x(:,:)
    integer i, f_no, n
    read(f_no, *) n
    allocate(x(n, n))
    do i = 1, n
      read(f_no, *) x(i, 1:n)
    end do
    do i = 1, n
      write(*, '(f15.1)') (x(i, 1:n))
    end do
  end subroutine insert_matrix

end module subprog

program main
  use subprog
  implicit none
  real(8), allocatable :: a(:,:), b(:,:) ,c(:,:)
  integer :: f1 = 11, f2 = 12, i, j, k, n
  open(f1, file = 'mat_a.txt')
  write (*, *) 'matrix_A is...'
  call insert_matrix(a, n, f1)
  close(f1)
  write (*, *) ''
  open(f2, file = 'mat_b.txt')
  write (*, *) 'matrix_B is...'
  call insert_matrix(b, n, f2)
  close(f2)
  write (*, *) ''
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
end program main
