program romberg_integral
    implicit none

    ! Using real(8) for double precision throughout
    real(8), dimension(0:8) :: x_vals, f_vals
    real(8), dimension(4, 4) :: R
    real(8) :: h
    integer :: i, j, k, step_size

    R = 0.0d0

    open(10, file='A5Q3_in.txt', status='old')
    read(10, *) x_vals
    read(10, *) f_vals
    close(10)

    ! --- ROMBERG INTEGRATION ---
    
    h = x_vals(8) - x_vals(0)
    R(1,1) = (h / 2.0d0) * (f_vals(0) + f_vals(8))

    do i = 2, 4
        h = h / 2.0d0
        step_size = 8 / (2**(i-1))
        
        R(i,1) = 0.5d0 * R(i-1,1)
        do k = step_size, 8 - step_size, 2 * step_size
            R(i,1) = R(i,1) + h * f_vals(k)
        end do
    end do

    do j = 2, 4
        do i = j, 4
            R(i,j) = R(i,j-1) + (R(i,j-1) - R(i-1,j-1)) / (4.0d0**(j-1) - 1.0d0)
        end do
    end do

    print *, "Romberg Table:"
    print *, "--------------------------------------------"
    do i = 1, 4
        print '(4F12.6)', (R(i, j), j = 1, i)
    end do
    print *, "--------------------------------------------"
    print '(A, F12.6)', "Estimated Integral: ", R(4, 4)

end program romberg_integral