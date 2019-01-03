module subprog
  implicit none
contains

  function f(t,x,y) result(v)
    real(8),intent(in) :: t,x,y
    real(8) b,k,m,v
    m = 10.0d0
    k = 10.0d0
    b = 2.0d0
    v =  - (k/m)*x - (b/m)*y
  end function f

  subroutine euler(t,x,v,d)
    real(8),intent(in) :: d
    real(8),intent(inout) :: x, v, t
    real(8) a
    a = f(t, x, v)
    x = x + v * d
    v = v + a * d
    t = t + d
  end subroutine euler

  subroutine runge_kutta(t,x,v,d)
    real(8),intent(in) :: d
    real(8),intent(inout) :: x, v, t
    real(8) :: kx1,kx2,kx3,kx4,kv1,kv2,kv3,kv4
    kx1 = v
    kv1 = f(t, x, v)
    kx2 = v + (d/2)*kv1
    kv2 = f(t+(d/2), x+(d/2)*kx1, v+(d/2)*kv1)
    kx3 = v + (d/2)*kv2
    kv3 = f(t+(d/2), x+(d/2)*kx2, v+(d/2)*kv2)
    kx4 = v + d*kv3
    kv4 = f(t+(d/2), x+(d/2)*kx3, v+(d/2)*kv3)
    x = x + (d/6)*(kx1+2*kx2+2*kx3+kx4)
    v = v + (d/6)*(kv1+2*kv2+2*kv3+kv4)
    t = t + d
  end subroutine runge_kutta
end module subprog

program damped_oscillation
  use subprog
  implicit none
  real(8) t,x,v,dt
  integer i, n
  open(10, file = 'data_of_graph.txt')
  open(11, file = 'data_of_graph_runge.txt')
  n = 1000
  dt = 20.d0 / dble(n)
  t = 0.d0
  x = 1.d0
  v = 0.d0
  do i = 1, n
    call euler(t,x,v,dt)
    write(10,'(3f13.8)') t,x,v
  end do
  close(10)
  dt = 20.d0 / dble(n)
  t = 0.d0
  x = 1.d0
  v = 0.d0
  do i = 1, n
    call runge_kutta(t,x,v,dt)
    write(11,'(3f13.8)') t,x,v
  end do
  close(11)
end program damped_oscillation
