program linear_solver
    implicit none
    integer, parameter :: n = 4
    real :: x(n), x_old(n), error, tol, omega
    integer :: iter, max_iter, method

    tol = 1.0e-3
    max_iter = 100
    omega = 1.1

    do method = 1, 3
        x = 0.0  ! Initial guess (0,0,0,0)
        iter = 0
        
        if (method == 1) print *, "--- Jacobi Method ---"
        if (method == 2) print *, "--- Gauss-Seidel Method ---"
        if (method == 3) print *, "--- SOR Method (w=1.1) ---"
        
        do while (iter < max_iter)
            x_old = x
            iter = iter + 1
            
            ! Update equations
            if (method == 1) then
                ! Jacobi: Uses only x_old values
                x(1) = (6.0 + x_old(2) - 2.0*x_old(3)) / 10.0
                x(2) = (25.0 + x_old(1) + x_old(3) - 3.0*x_old(4)) / 11.0
                x(3) = (-11.0 - 2.0*x_old(1) + x_old(2) + x_old(4)) / 10.0
                x(4) = (15.0 - 3.0*x_old(2) + x_old(3)) / 8.0
            else
                ! Gauss-Seidel and SOR: Uses latest available values
                call update_gs(x)
                if (method == 3) then
                    ! SOR refinement: x_new = (1-w)*x_old + w*x_gs
                    x = (1.0 - omega) * x_old + omega * x
                end if
            end if

            ! Calculate L-infinity norm error
            error = maxval(abs(x - x_old))
            if (error < tol) exit
        end do

        print "(A, I3, A, 4F8.4)", " Converged in", iter, " iterations. x =", x
        print *
    end do

contains

    subroutine update_gs(v)
        real, intent(inout) :: v(4)
        v(1) = (6.0 + v(2) - 2.0*v(3)) / 10.0
        v(2) = (25.0 + v(1) + v(3) - 3.0*v(4)) / 11.0
        v(3) = (-11.0 - 2.0*v(1) + v(2) + v(4)) / 10.0
        v(4) = (15.0 - 3.0*v(2) + v(3)) / 8.0
    end subroutine update_gs

end program linear_solver