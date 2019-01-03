module subprog
  implicit none
contains

  function f(t,a,b) result(k)
    implicit none
    real(8), intent(in) :: t,a,b
    real(8) G,large_M,r,k
    G = 1.0d0
    large_M = 1.0d0
    r = sqrt(a**2 + b**2)
    k = - (G*large_M*a) / r**3
  end function f

  function en(t,a,b,c,d) result(k)
    implicit none
    real(8), intent(in) :: t,a,b,c,d
    real(8) G,large_M,small_m,r,v,k
    G = 1.0d0
    large_M = 1.0d0
    small_m = 1.0d0
    r = sqrt(a**2 + b**2)
    v = sqrt(c**2 + d**2)
    k = 0.5*small_m*(v**2) - (G*large_M*small_m)/r
  end function en

  function an(t,a,b,c,d) result(k)
    implicit none
    real(8), intent(in) :: t,a,b,c,d
    real(8) small_m,k
    small_m = 1.0d0
    k = small_m*(a*d - b*c)
  end function an

  subroutine euler(t,x,y,vx,vy,d,energy,angular)
    real(8),intent(in) :: d
    real(8),intent(inout) :: x,y,vx,vy,t,energy,angular
    real(8) fx,fy
    fx = f(t,x,y)
    fy = f(t,y,x)
    x = x + vx * d
    y = y + vy * d
    vx = vx + fx * d
    vy = vy + fy * d
    t = t + d
    energy = en(t,x,y,vx,vy)
    angular = an(t,x,y,vx,vy)
  end subroutine euler

  subroutine runge_kutta(t,x,y,vx,vy,d,energy,angular)
    real(8), intent(in) :: d
    real(8), intent(inout) :: x,y,vx,vy,t,energy,angular
    real(8) :: k_x1,k_x2,k_x3,k_x4,k_vx1,k_vx2,k_vx3,k_vx4,k_y1,k_y2,k_y3,k_y4,k_vy1,k_vy2,k_vy3,k_vy4
    k_x1 = vx
    k_y1 = vy
    k_vx1 = f(t, x, y)
    k_vy1 = f(t, y, x)
    k_x2 = vx + (d/2)*k_vx1
    k_y2 = vy + (d/2)*k_vy1
    k_vx2 = f(t+(d/2), x+(d/2)*k_x1, y+(d/2)*k_y1)
    k_vy2 = f(t+(d/2), y+(d/2)*k_y1, x+(d/2)*k_x1)
    k_x3 = vx + (d/2)*k_vx2
    k_y3 = vy + (d/2)*k_vy2
    k_vx3 = f(t+(d/2), x+(d/2)*k_x2, y+(d/2)*k_y2)
    k_vy3 = f(t+(d/2), y+(d/2)*k_x2, x+(d/2)*k_x2)
    k_x4 = vx + d*k_vx3
    k_y4 = vy + d*k_vy3
    k_vx4 = f(t+(d/2), x+(d/2)*k_x3, y+(d/2)*k_y3)
    k_vy4 = f(t+(d/2), y+(d/2)*k_y3, x+(d/2)*k_x3)
    x = x + (d/6)*(k_x1+2*k_x2+2*k_x3+k_x4)
    y = y + (d/6)*(k_y1+2*k_y2+2*k_y3+k_y4)
    vx = vx + (d/6)*(k_vx1+2*k_vx2+2*k_vx3+k_vx4)
    vy = vy + (d/6)*(k_vy1+2*k_vy2+2*k_vy3+k_vy4)
    t = t + d
    energy = en(t,x,y,vx,vy)
    angular = an(t,x,y,vx,vy)
  end subroutine runge_kutta

end module

program kepler
  use subprog
  implicit none
  real(8) t,x,y,vx,vy,dt,e,l
  integer i, n
  open(10, file = 'graph_output.txt')
  open(11, file = 'enan.txt')
  n = 30000
  dt = 40.d0 / dble(n)
  t = 0.d0
  x = 4.d0
  y = 0.d0
  vx = 0
  vy = 0.4d0
  e = 0
  l = 0
  do i = 1, n
    call runge_kutta(t,x,y,vx,vy,dt,e,l)
    write(10,'(3f13.8)') x,y,t
    write(11,'(3f13.8)') e,l
  end do
  close(10)
  close(11)
end program kepler
