program Traphizoidal
    implicit none

    real :: a = 1, b=9, n, h, x, x_curr, f_, res, f, real_out, error_
    integer :: i
    real, dimension(31) :: x_vals, f_vals

    !open(10, file='A5Q1_in.txt')

    !read(10,*)a,b,n

    h = (b - a) / n

    x_vals(1) = a
    f_vals(1) = f(x_vals(1))

    do i = 2,31

        x_vals(i) = x_vals(i-1) + h
        f_vals(i) = f(x_vals(i))

    end do
    print*, f_vals
    res = 0
    do i = 1,30
        if (mod(i,2) == 0) then
            res = res + 2.0 * f_vals(i)
            print*, res
        else
            res = res + 4.0 * f_vals(i)
            print*, res
        end if
    end do

    res = f_vals(1) + f_vals(31)

    print*, res

end program

real function f(t) result(y)
    implicit none
    real :: t

    y = ((2.0 * t ** 2) + (t** (5.0/2.0)) - 1) / (t**2.0)
end function

real function f_(t) result(y)
    implicit none
    real :: t

    y = (2.0*t + 2.0/3.0 * t**(3.0/2.0) + 1.0/t)
end function

