program bisection
    implicit none
    real(8) :: a, b, tol, mid, f_a, f_mid
    integer :: iter

    ! Setup for the specific problem: x^3 + 4x^2 - 20 = 0
    a = 1.0d0
    b = 2.0d0
    tol = 1.0d-4  ! This is 0.0001 (not 10e-4)

    ! Initial check
    f_a = a**3 + 4.0d0*a**2 - 20.0d0

    print '(A6, A18, A18, A18)', "Iter", "a", "b", "Midpoint (x)"
    print '(A60)', "------------------------------------------------------------"

    iter = 0
    ! The loop continues until the precision is finer than the tolerance
    do while (((b - a) / 2.0d0) >= tol)
        iter = iter + 1
        mid = (a + b) / 2.0d0
        f_mid = mid**3 + 4.0d0*mid**2 - 20.0d0

        print '(I6, F18.12, F18.12, F18.12)', iter, a, b, mid

        ! Classic Bisection logic
        if (f_a * f_mid < 0.0d0) then
            b = mid
        else
            a = mid
            f_a = f_mid
        end if
    end do

    print '(A60)', "--------------------------------------- ---------------------"
    print *, "Total Iterations: ", iter
    print '(A, F18.12)', "Final Root: ", (a + b) / 2.0d0

end program bisection
