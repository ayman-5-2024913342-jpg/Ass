program newton_raphson_complex
    implicit none
    real(8) :: x, x_next, tol
    integer :: i, max_iter, j
    real(8), dimension(3) :: starts = (/-2.0d0, 1.0d0, 4.0d0/)

    tol = 1.0d-6
    max_iter = 50

    do j = 1, 3
        x = starts(j)
        print *, ""
        print *, "Testing Initial Guess x0 = ", x
        print '(A6, A20, A20)', "Iter", "x_n", "f(x_n)"
        print '(A50)', "--------------------------------------------------"

        do i = 1, max_iter
            ! Newton's Formula: x_{n+1} = x_n - f(x_n) / f'(x_n)
            if (abs(df(x)) < 1.0d-12) then
                print *, "Error: Derivative is zero. Method failed."
                exit
            end if

            x_next = x - f(x) / df(x)
            print '(I6, F20.12, F20.12)', i, x_next, f(x_next)

            if (abs(x_next - x) < tol) then
                print *, "Success! Root found at: ", x_next
                exit
            end if

            x = x_next

            if (i == max_iter) print *, "Warning: Did not converge."
        end do
    end do

contains

    ! Original Function
    function f(x)
        real(8) :: f, x
        f = 5.0d0*x**2 + cos(3.0d0*x) - 2.0d0*exp(x) - exp(-x)
    end function f

    ! Derivative Function
    function df(x)
        real(8) :: df, x
        df = 10.0d0*x - 3.0d0*sin(3.0d0*x) - 2.0d0*exp(x) + exp(-x)
    end function df

end program newton_raphson_complex



!ier no      Pn-1   f'(Pn - 1)    Pn    f
