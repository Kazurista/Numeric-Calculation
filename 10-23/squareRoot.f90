program squareRoot
  implicit none
  integer :: i, n = 10000, fo = 11
  real(8) :: a, x, y, dx, xmax, xmin, ymin = 1.0d10, xx, y2
  read (*,*) a
  open(fo, file = 'output.txt')
  xmin=0
  xmax=1.0d3
  dx=(xmax - xmin) / dble(n)
  do i=0, n
    x = xmin + dx * dble(i)
    y = x * x - a
    y2 = y ** 2
    write(fo, '(3e12.4)') x, y
    if( y2 < ymin ) then
      ymin = y2
      xx = x
    end if
  end do
  close(fo)
  write(*,*) 'minimum y=', ymin, ' x=', xx
end program squareRoot
