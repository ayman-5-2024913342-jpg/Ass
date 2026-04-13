program iteration_analysis_final
    implicit none

    real(8) :: alpha, x2, x3, x_old, g_prime_1, g_prime_2, g_prime_3
    real(8) :: rel_err2, rel_err3, tol
    integer :: i

    alpha = 1.328269d0
    x2 = 1.4d0
    x3 = 1.4d0
    tol = 0.000006d0

    g_prime_1 = abs(-1.5d0 * alpha**2)
    g_prime_2 = abs(-2.0d0 / (3.0d0 * (5.0d0 - 2.0d0 * alpha)**(2.0d0/3.0d0)))
    g_prime_3 = abs((6.0d0 * alpha * (alpha**3 + 2.0d0*alpha - 5.0d0)) / &
                (3.0d0 * alpha**2 + 2.0d0)**2)

    print *, "CONVERGENCE TEST RESULTS (at alpha):"
    print '(A25, F10.6)', "Formula 1 |g'(x)|:", g_prime_1
    print '(A25, F10.6)', "Formula 2 |g'(x)|:", g_prime_2
    print '(A25, F10.6)', "Formula 3 |g'(x)|:", g_prime_3
    print *

    print*, "Oscilatory"
    print '(A60)', "------------------------------------------------------------"
    print '(A5, A18, A18, A18)', "n", "Xn+1 (Formula 2)", "Abs Error", "Rel Error"
    print '(A60)', "------------------------------------------------------------"

    do i = 1, 15
        x_old = x2
        x2 = (5.0d0 - 2.0d0 * x2)**(1.0d0/3.0d0)
        rel_err2 = abs((x2 - x_old) / x2)

        print '(I4, F18.6, F18.6, F18.6)', i, x2, abs(x2-x_old), rel_err2

        if (abs(x2 - x_old) < tol) exit
    end do

    print *
    print*, "Non-Oscilatory"
    print '(A60)', "------------------------------------------------------------"
    print '(A5, A18, A18, A18)', "n", "Xn+1 (Formula 3)", "Abs Error", "Rel Error"
    print '(A60)', "------------------------------------------------------------"

    do i = 1, 15
        x_old = x3
        x3 = (2.0d0 * x3**3 + 5.0d0) / (3.0d0 * x3**2 + 2.0d0)
        rel_err3 = abs((x3 - x_old) / x3)

        print '(I4, F18.6, F18.6, F18.6)', i, x3, abs(x3-x_old), rel_err3

        if (abs(x3 - x_old) < tol) exit
    end do

    print '(A60)', "------------------------------------------------------------"
    print '(A25, F10.6)', "Final Root Estimate:", x3

end program iteration_analysis_final
