program secant_algorithm
    implicit none

    real(8) :: p0, p1, p, q0, q1, tol
    integer :: i, n0

    p0 = 0.0d0
    p1 = 1.0d0
    tol = 1.0d-6
    n0 = 20

    i = 2
    q0 = f(p0)
    q1 = f(p1)

    ! Table Header for clarity
    print '(A4, A12, A12, A15, A15, A12)', &
          "i", "p0", "p1", "q0", "q1", "p"
    print '(A75)', "---------------------------------------------------------------------------"

    do while (i <= n0)
        if (abs(q1 - q0) < 1.0d-12) then
            print *, "Failure: Division by zero."
            stop
        end if

        p = p1 - q1 * (p1 - p0) / (q1 - q0)

        print '(I4, F12.6, F12.6, F15.6, F15.6, F12.6)', &
              i, p0, p1, q0, q1, p

        if (abs(p - p1) < tol) then
            print '(A75)', "---------------------------------------------------------------------------"
            print '(A25, F10.6)', "SUCCESS! Root p = ", p
            stop
        end if

        ! Step 5
        i = i + 1

        ! Step 6: Update p0, q0, p1, q1
        p0 = p1
        q0 = q1
        p1 = p
        q1 = f(p1)
    end do

    ! Step 7
    print *, "The method failed after N0 iterations, N0 =", n0
    stop

contains

    ! The specific function requested: 4x^3 - 1 - 0.5 * exp(x^2 / 2)
    function f(x)
        real(8) :: f, x
        f = 4.0d0 * x**3 - 1.0d0 - 0.5d0 * exp(x**2 / 2.0d0)
    end function f

end program secant_algorithm
