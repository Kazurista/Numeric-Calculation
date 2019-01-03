program nVector

  implicit none
  integer i
  integer, parameter :: n = 5
  real(8) inner, a(n), b(n)

  open(10, file='datasA.txt')
    do i = 1, n
      read(10, *) a(i)
    end do
  close(10)

  open(11, file='datasB.txt')
    do i = 1, n
      read(11, *) b(i)
    end do
  close(11)

  inner = 0.0d0
  do i = 1, n
    inner = inner + a(i)*b(i)
  end do
  
  write (*,*) 'inner product =', inner

end program nVector
