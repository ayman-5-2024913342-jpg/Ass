program A4Q2
    implicit none

    real :: f, df, tol, p, p0
    integer :: iter

    print *, "Enter the tolerance :"
    read *, tol


end program

real function f(x) result(y)
    implicit none
    real :: x
    y = 5.0*x**2 + cos(3.0*x) - 2.0*exp(x) - exp(-x)
end function

real  function df(w) result(z)
    implicit none
    real :: w
    z = 10.0*W - 3.0*sin(3.0*w) - 2.0*exp(W) + exp(-w)
end function df
