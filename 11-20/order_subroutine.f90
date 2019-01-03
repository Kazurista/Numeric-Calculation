module subprog
  implicit none
contains
  subroutine swap(a, b, c)
    integer a, b, c, tmp
    if ( a >= b ) then
      if ( b >= c ) then
        tmp = c
        c = a
        a = tmp
      else
        tmp = b
        if ( a >= c ) then
          b = c
          c = a
          a = tmp
        else
          b = a
          a = tmp
        end if
      end if
    else !b>a
      if ( b >= c ) then
        if ( a >= c ) then
          tmp = c
          c = b
          b = a
          a = tmp
        else !c>a
          tmp = c
          c = b
          b = tmp
        end if
      end if
    end if
  end subroutine swap
end module subprog

program main
  use subprog
  implicit none
  integer x, y, z
  read(*,*) x, y, z
  write(*,*) 'x,y,z=',x,y,z
  call swap(x, y, z)
  write(*,*) x,'<=',y,'<=',z
end program main
