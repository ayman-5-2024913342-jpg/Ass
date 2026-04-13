program A4Q1_Fixed
    implicit none


    real(8) :: a, b, tol, mid, fm, rel, prev_mid
    integer :: iter

    print *, "Enter the interval [a, b] ):"
    read *, a, b
    print *, "Enter the tolerance :"
    read *, tol

    if (f(a) * f(b) >= 0.0d0) then
        print*, "Error: f(a) and f(b) must have opposite signs."
        stop
    end if

    print *
    print '(A6, A15, A15, A15, A18)', "Iter", "a", "b", "Midpoint", "Rel Error"
    print '(A70)', "----------------------------------------------------------------------"

    iter = 1

    do
        mid = (a + b) / 2.0d0
        fm = f(mid)

        rel = abs(b - a) / abs(mid)

        print '(I6, F15.8, F15.8, F15.8, F18.10)', iter, a, b, mid, rel


        if (f(a) * fm < rel) then
            b = mid
        else
            a = mid
        end if


        if (rel < tol) exit

        iter = iter + 1
        if (iter > 100) exit
    end do

    print '(A70)', "----------------------------------------------------------------------"
    print '(A, I3)', "Total Iterations: ", iter
    print '(A, F15.8)', "Final Root Estimate: ", mid

contains

    function f(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y
        y = x**3 + 4.0d0*x**2 - 10.0d0
    end function f

end program A4Q1_Fixed
