program averageAndVarience

  implicit none
  integer i, n
  real(8), allocatable, dimension(:) :: x
  real(8) xBar, varience, sum1, sum2
  write(*,*) 'Enter a number between 1 and 10.'
  read(*,*) n
  allocate (x(n))

  open(13, file = 'XiDatas.txt')
    do i = 1, n
      read(13, *) x(i)
    end do
  close(13)

  write(*,*) 'XiDatas are...'
  write(*, '(f15.1)') x

  sum1 = 0
  do i = 1, n
    sum1 = sum1 + x(i)
  end do
  xBar = sum1 / n

  write(*,*) ''
  write(*,*) 'The average of datas is', xBar

  sum2 = 0
  do i = 1, n
    sum2 = sum2 + ( x(i) - xBar ) ** 2
  end do
  varience = sum2 / n

  write(*,*) 'The varience of datas is', varience
  deallocate (x)

end program averageAndVarience
