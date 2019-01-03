program condenser
  implicit none
  integer, parameter :: nt = 200, nx = 100, ny = 100
  integer ix, iy, n
  real(8) v(0:nx, 0:ny)

  do iy = 0, ny
    do ix = 0, nx
      v(ix, iy) = 0.0d0  !初期値 10000個の点が0
    end do
  end do

  open (11, file='output.txt')

  do n = 1, nt
    do iy = 1, ny-1
      do ix = 1, nx-1
        if (ix == nx/2 .and. iy == ny/2) then
          v(ix,iy) = 1.0d2
        else
          v(ix,iy) = (v(ix-1,iy)+v(ix+1,iy)+v(ix,iy-1)+v(ix,iy+1))/4.0d0
        end if
      end do
    end do
  end do

  do iy =0, ny, 5
    do ix = 0, nx, 5
      write(11, *) ix, iy, v(ix, iy)
    end do
    write(11,*)
  end do
  close(11)
end program condenser
