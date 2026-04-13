program A4Q3
    implicit none

    real :: f, tol, a, b, c, rel = 1
    integer :: iter = 0

    a = 0
    b = 1

    tol = 0.0000001

    print*, "          Itr no          ", " a", "                b", "             c   ", "            erRor"

    do while(rel > tol)
        iter = iter + 1
        c = b - f(b) * ((a-b) / (f(a)-f(b)))
        rel = abs((c-b)/c)

        print*, iter, "           ", a, b, c, rel
        a = b
        b = c
    end do

    print*, "Ans: ", b
end program


real function f(x) result(y)
    implicit none

    real :: x

    y = 4 * x**3 - 1.0 - exp(x**2 / 2.0)
end function
