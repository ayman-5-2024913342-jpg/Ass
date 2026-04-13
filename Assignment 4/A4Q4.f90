program false_position
    implicit none

    ! Variables
    real(8) :: a, b, c, fa, fb, fc, tol
    integer :: n

    ! Initial Interval [1, 2]
    a = 1.0d0
    b = 2.0d0
    tol = 1.0d-5

    ! Table Headings
    print '(A4, A12, A12, A12, A15, A15, A12)', &
          "n", "a", "b", "c", "f(a)", "f(b)", "f(c)"
    print '(A85)', "-------------------------------------------------------------------------------------"

    n = 1
    do
        fa = f(a)
        fb = f(b)

        ! False Position Formula
        c = (a * fb - b * fa) / (fb - fa)
        fc = f(c)

        ! Print formatted row
        print '(I4, F12.6, F12.6, F12.6, F15.6, F15.6, F12.6)', &
              n, a, b, c, fa, fb, fc

        ! Check for convergence (accuracy within 10^-5)
        if (abs(fc) < tol) then
            print '(A85)', "-------------------------------------------------------------------------------------"
            print '(A25, F10.6)', "Root found at x = ", c
            exit
        end if

        ! Update interval: If f(a) and f(c) have opposite signs, root is in [a, c]
        if (fa * fc < 0.0d0) then
            b = c
        else
            a = c
        end if

        n = n + 1
        if (n > 100) exit ! Safety break
    end do

contains

    ! Function: e^x + 2^-x + 2cos(x) - 6
    function f(x)
        real(8) :: f, x
        f = exp(x) + (2.0d0**(-x)) + 2.0d0*cos(x) - 6.0d0
    end function f

end program false_position
