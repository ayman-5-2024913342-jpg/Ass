program bisection_solver
    implicit none

    real(8) :: a, b, tol, root
    integer :: status

    ! 1. Input Section
    print *, "Enter the interval [a, b] :"
    read *, a, b
    print *, "Enter the tolerance :"
    read *, tol

    ! 2. Call the Bisection Subroutine
    call find_root(a, b, tol, root, status)

    ! 3. Output Results
    if (status == 0) then
        print '(A, F12.8)', " Root found at: ", root
    else
        print *, "Error: Function has the same sign at both ends of the interval."
    end if

contains

    ! The function we are solving: x^3 + 4x^2 - 20 = 0
    function f(x)
        real(8) :: f, x
        f = x**3 + 4.0d0*x**2 - 20.0d0
    end function f

    subroutine find_root(a_in, b_in, tol, mid, status)
        real(8), intent(in) :: a_in, b_in, tol
        real(8), intent(out) :: mid
        integer, intent(out) :: status
        real(8) :: a, b, fa, fm

        a = a_in
        b = b_in
        status = 0

        ! Check if a root exists in the interval
        if (f(a) * f(b) >= 0.0d0) then
            status = -1
            return
        end if

        ! Iterative bisection
        do while ((b - a) / 2.0d0 > tol)
            mid = (a + b) / 2.0d0
            fm = f(mid)

            if (abs(fm) < 1.0d-12) exit ! Exact root found

            if (f(a) * fm < 0.0d0) then
                b = mid
            else
                a = mid
            end if
        end do

        mid = (a + b) / 2.0d0
    end subroutine find_root

end program bisection_solver
