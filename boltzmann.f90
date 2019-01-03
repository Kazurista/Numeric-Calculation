program boltzmann

  implicit none
  integer n, i, num
  real(8) :: m, t, pro_sum, h, dp, a, b
  real(8), parameter :: pi = atan(1.d0)*4d0, kb = 1.38065030e-23_8
  m = 0.004/(6*10d23)   !質量
  t = 300   !温度
  n = 10000   !積分区間を分割
  pro_sum = 0d0
  a = 0d0
  h = sqrt(2*m*kb*t)

  do num = 1, 10
    b = sqrt(2*num*m*kb*t)
    dp = (b-a)/n
    do i = 1, n-1
      pro_sum = pro_sum + 0.5*dp*(1/(sqrt(pi)*h)**3)*4*pi*((exp(-(a/h)**2)*a**2+(exp(-((a+dp)/h)**2))*(a+dp)**2))
      a = a + dp
    end do
    write(*,*) "num =",num,":",pro_sum
  end do

end program boltzmann
