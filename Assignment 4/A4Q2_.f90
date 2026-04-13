program newton_raphson_solve
    implicit none

    ! Variables
    real(8) :: p_old, p_new, fx, dfx, error, tol
    integer :: i, iter, max_iter
    real(8), dimension(3) :: initial_guesses

    ! Initial setups
    initial_guesses = (/-2.0d0, 1.0d0, 4.0d0/)
    tol = 1.0d-7
    max_iter = 25

    do i = 1, 3
        p_old = initial_guesses(i)

        print *, ""
        print *, "RUNNING FOR INITIAL GUESS X0 = ", p_old
        print '(A12, A15, A12, A15, A18)', "Pn-1", "df(Pn-1)", "Pn", "f(Pn)", "(Pn-Pn-1)/Pn"
        print '(A75)', "---------------------------------------------------------------------------"

        do iter = 1, max_iter
            dfx = df(p_old)

            ! Newton-Raphson Formula: Pn = Pn-1 - f(Pn-1)/f'(Pn-1)
            p_new = p_old - (f(p_old) / dfx)

            ! Calculate the function value at the new point
            fx = f(p_new)

            ! Relative difference (Pn - Pn-1)/Pn
            error = (p_new - p_old) / p_new

            ! Output row
            print '(F12.6, F15.6, F12.6, F15.6, F18.6)', p_old, dfx, p_new, fx, error

            ! Convergence Check
            if (abs(p_new - p_old) < tol) then
                print *, "Root found: ", p_new, " in ", iter, " iterations."
                exit
            end if

            p_old = p_new

            if (iter == max_iter) print *, "Warning: Did not converge within max iterations."
        end do
    end do

contains

    ! Function: f(x) = 5x^2 + cos(3x) - 2e^x - e^-x
    function f(x)
        real(8) :: f, x
        f = 5.0d0*x**2 + cos(3.0d0*x) - 2.0d0*exp(x) - exp(-x)
    end function f

    ! Derivative: f'(x) = 10x - 3sin(3x) - 2e^x + e^-x
    function df(x)
        real(8) :: df, x
        df = 10.0d0*x - 3.0d0*sin(3.0d0*x) - 2.0d0*exp(x) + exp(-x)
    end function df

end program newton_raphson_solve
