module subprog
  implicit none
contains
  function f(x, y) result(v)
    real(8), intent(in) :: x, y
    real(8) v
    v = cos(x) * y
  end function f

  subroutine Runge(x, y, d)
    real(8), intent(in) :: x, d
    real(8), intent(inout) :: y
    real(8) :: k1, k2, k3, k4
    k1 = f(x, y)
    k2 = f(x+d/2, y+(d/2)*k1)
    k3 = f(x+d/2, y+(d/2)*k2)
    k4 = f(x+d, y+d*k3)
    y = y + (d/6)*(k1+2*k2+2*k3+k4)
  end subroutine Runge
end module subprog

program runge_kutta
  use subprog
  implicit none
  real(8) x, y, del
  integer i, n
  open(10, file = 'output.txt')
  write(*, '(a)', advance = 'no') 'input n:'
  read(*, *) n
  del = 10.d0 / dble(n)
  x = 0.d0
  y = 1.d0
  do i = 1, n
    call Runge(x, y, del)
    x = del * dble(i)
    write(10, '(2f13.8)') x, y
  end do
  close (10)
end program runge_kutta
