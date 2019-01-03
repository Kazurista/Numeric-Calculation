module subprog
  implicit none
contains

  subroutine gauss_jordan(a0,x1,x2,x3,b1,b2,b3,n)
    integer, intent(in) :: n
    real(8), intent(in) :: a0(n,n),b1(n),b2(n),b3(n),x1(n),x2(n),x3(n)
    real(8), intent(out) :: x1(n),x2(n),x3(n)
    integer i, j, k, h
    real(8) ar, a(n,n)
    a(:,:) = a0(:,:)
!    do i = 1,n
!      x1(:) = b(i,1)
!      x2(:) = b(i,2)
!      x3(:) = b(i,3)
!    end do
!    do h = 1,n
!      if (h == 1) then
!        x(:) = x1(n)
!      elseif ( h == 2 ) then
!        x(:) = x2(n)
!      elseif (h == 3) then
!        x(:) = x3(n)
!      end if
      do k = 1,n
        if(a(k,k) == 0.0d0) stop "pivot = 0"
        ar = 1.0d0 / a(k,k)
        a(k,k) = 1.0d0
        do j = k+1,n
          a(k,j) = ar * a(k,j)
        end do
        x1(k) = ar * x1(k)
        do i = 1,n
          if(i /= k) then
            do j = k+1,n
              a(i,j) = a(i,j) - a(i,k) * a(k,j)
            end do
            x1(i) = x1(i) - a(i,k) * x1(k)
            a(i,k) = 0.0d0
          end if
        end do
      end do
!      if (h == 1) then
!        x1(n) = x(n)
!      elseif ( h == 2 ) then
!      elseif (h == 3) then
!        x3(n) = x(n)
!      end if
!    end do
  end subroutine gauss_jordan

  subroutine set_random_matrix(a,b1,b2,b3,n)
    integer, intent(in) :: n
    real(8), intent(out) :: a(n,n),b1(n),b2(n),b3(n)
    real(8), allocatable :: b(n,n)
    integer i,k
    call random_seed
    call random_number(a)
    do i = 1,n
      a(i,1:n) = aint(a(i,1:n)*10)
    end do
    do i = 1,n
      do k = 1,n
        if (i == k) then
          b(i,k) = 1.0d0
        else
          b(i,k) = 0.0d0
        end if
      end do
    end do
    do i = 1,n
      b1(:) = b(i,1)
      b2(:) = b(i,2)
      b3(:) = b(i,3)
    end do
  end subroutine set_random_matrix

end module subprog

program main
  use subprog
  implicit none
  real(8), allocatable :: a(:,:),b1(:),b2(:),b3(:),x1(:),x2(:),x3(:),r1(:),r2(:),r3(:)
  integer n,i,j,k
  n = 3
  allocate (a(n,n),b1(n),b2(n),b3(n),x1(n),x2(n),x3(n),r1(n),r2(n),r3(n))
  call set_random_matrix(a,b1,b2,b3,n)
  call gauss_jordan(a,x1,x2,x3,b1,b2,b3,n)
!  r(:) = b(:) - matmul(a,x)
  write(*,*) 'A:'
  do i = 1,n
    write(*,*) a(i,1:n)
  end do
  write(*,*) 'B:'
  write(*,*) b1(1:n), b2(1:n), b3(1:n)
!  write(*,*) 'x:',x(1:n)
!  write(*,*) 'Ax:',matmul(a,x)
!  write(*,*) 'b-Ax:',r(1:n)
  deallocate (a,b1,b2,b3,x1,x2,x3,r)
end program main
