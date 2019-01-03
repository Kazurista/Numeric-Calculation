program overflow

  implicit none
  integer :: i, j, k
  i = 1073741823
  j = 2
  k = i * j
  write(*,*) k

end program overflow
