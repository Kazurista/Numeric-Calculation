program order

  implicit none
  integer i, n, j
  real(8), allocatable, dimension(:) :: array
  real(8) minOfData
  write(*,*) 'Enter a number between 1 and 10'
  read (*,*) n
  allocate ( array(n) )

  open (12, file='nDatas.txt')
  read (12, *) (array(i), i = 1, n)
  close(12)



  write(*, *) 'array contents are...'
  write(*, '(f15.1)') array

  do j = 1, n-1
    do i = j+1, n
      if (array(j) > array(i)) then
        minOfData = array(i)
        array(i) = array(j)
        array(j) = minOfData
      end if
    end do
  end do

  write(*,*) ''
  write(*,*) 'sorting result is...'
  do i = 1, n
    write(*, '(f15.1)') array(i)
  end do

  deallocate (array)

end program order
